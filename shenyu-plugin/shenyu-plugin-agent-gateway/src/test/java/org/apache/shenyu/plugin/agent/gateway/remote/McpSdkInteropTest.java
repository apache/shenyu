/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.shenyu.plugin.agent.gateway.remote;

import com.google.gson.JsonParser;
import org.apache.shenyu.common.dto.AgentGatewayMcpConfig;
import org.apache.shenyu.plugin.agent.gateway.AgentTrafficContext;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpDispatcher;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpHttpHandler;
import org.apache.shenyu.plugin.agent.gateway.security.AgentMcpIdentity;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolRegistry;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledIfEnvironmentVariable;
import org.springframework.http.server.reactive.ReactorHttpHandlerAdapter;
import org.springframework.web.server.adapter.WebHttpHandlerBuilder;
import reactor.core.publisher.Mono;
import reactor.netty.DisposableServer;
import reactor.netty.http.server.HttpServer;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/** Loopback SDK-to-SDK interoperability. See src/test/interop for the pinned Node fixture. */
@EnabledIfEnvironmentVariable(named = "SHENYU_MCP_SDK_INTEROP", matches = "true")
class McpSdkInteropTest {

    @Test
    void shouldRouteSdkCallsAcrossIndependentGateways() throws Exception {
        Path fixture = Path.of("src/test/interop/sdk.mjs").toAbsolutePath();
        Path evidence = Path.of("target/mcp-interop-8196.json").toAbsolutePath();
        Files.deleteIfExists(evidence);
        String credential = UUID.randomUUID().toString();
        ProcessBuilder builder = new ProcessBuilder("node", fixture.toString(), evidence.toString()).redirectError(ProcessBuilder.Redirect.INHERIT);
        builder.environment().put("SHENYU_INTEROP_SERVICE_TOKEN", credential);
        Process node = builder.start();
        // Killing a stalled child also unblocks readLine during startup or the exercise.
        ScheduledExecutorService timeout = Executors.newSingleThreadScheduledExecutor();
        timeout.schedule(node::destroyForcibly, 90, TimeUnit.SECONDS);
        try (BufferedReader output = node.inputReader(StandardCharsets.UTF_8); BufferedWriter input = node.outputWriter(StandardCharsets.UTF_8)) {
            URI upstream = URI.create("http://127.0.0.1:" + Integer.parseInt(output.readLine()));
            try (Gateway first = Gateway.start(upstream, credential, "json"); Gateway second = Gateway.start(upstream, credential, "sse")) {
                command(input, "[" + first.server().port() + "," + second.server().port() + "]");
                assertEquals("calls-complete", output.readLine());
            }
            // Keep the upstream recorder running until both catalogs have completed cleanup.
            command(input, "catalogs-closed");
            assertTrue(node.waitFor(10, TimeUnit.SECONDS));
            assertEquals(0, node.exitValue());
            assertTrue(Files.isRegularFile(evidence));
        } finally {
            timeout.shutdownNow();
            node.destroyForcibly();
            assertTrue(node.waitFor(5, TimeUnit.SECONDS));
        }
    }

    private static void command(final BufferedWriter input, final String value) throws IOException {
        input.write(value);
        input.newLine();
        input.flush();
    }

    private record Gateway(ManagedRemoteMcpCatalog catalog, DisposableServer server) implements AutoCloseable {

        private static Gateway start(final URI upstream, final String credential, final String responseMode) {
            Set<String> tools = Set.of("json.count", "sse.count", "legacy.count");
            String config = """
                    {"aggregation":{"revision":"fixture","servers":[
                    {"name":"json","endpoint":"%1$s/json","credentialRef":"fixture","credentialVersion":"v1","protocolProfile":"native-2026-07-28"},
                    {"name":"sse","endpoint":"%1$s/sse","credentialRef":"fixture","credentialVersion":"v1","protocolProfile":"native-2026-07-28"},
                    {"name":"legacy","endpoint":"%1$s/legacy","credentialRef":"fixture","credentialVersion":"v1"}]}}
                    """.formatted(upstream);
            ManagedRemoteMcpCatalog catalog = new ManagedRemoteMcpCatalog(Set.of(upstream.resolve("/json"), upstream.resolve("/sse"), upstream.resolve("/legacy")),
                    Set.of(), reference -> new RemoteServerBinding.Credential(reference, credential));
            try {
                catalog.reload(config).block(Duration.ofSeconds(10));
                AgentMcpDispatcher dispatcher = new AgentMcpDispatcher(new AgentToolRegistry(List.of()), "interop", "1", catalog);
                AgentMcpHttpHandler handler = new AgentMcpHttpHandler(dispatcher, exchange -> Mono.just(new AgentMcpIdentity("fixture-agent", tools)));
                AgentGatewayMcpConfig rule = AgentGatewayMcpConfig.parse(JsonParser.parseString("""
                        {"allowedTools":["json.count","sse.count","legacy.count"],"responseMode":"%s"}
                        """.formatted(responseMode)), true);
                ReactorHttpHandlerAdapter adapter = new ReactorHttpHandlerAdapter(WebHttpHandlerBuilder.webHandler(exchange -> handler.handle(exchange, rule,
                        new AgentTrafficContext(exchange.getRequest().getId(), "mcp", "selector", "rule"), 1)).build());
                DisposableServer server = HttpServer.create().host("127.0.0.1").port(0).handle(adapter).bindNow(Duration.ofSeconds(5));
                return new Gateway(catalog, server);
            } catch (RuntimeException error) {
                catalog.close();
                throw error;
            }
        }

        @Override
        public void close() {
            try {
                server.disposeNow();
            } finally {
                catalog.close();
            }
        }
    }
}
