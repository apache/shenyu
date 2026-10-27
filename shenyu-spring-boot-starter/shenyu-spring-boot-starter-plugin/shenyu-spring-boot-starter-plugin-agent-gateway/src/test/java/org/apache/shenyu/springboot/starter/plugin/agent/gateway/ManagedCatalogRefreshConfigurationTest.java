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

package org.apache.shenyu.springboot.starter.plugin.agent.gateway;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.sun.net.httpserver.HttpServer;
import io.modelcontextprotocol.common.McpTransportContext;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.metrics.AgentMcpCatalogMetrics;
import org.apache.shenyu.plugin.agent.gateway.remote.ManagedRemoteMcpCatalog;
import org.apache.shenyu.plugin.agent.gateway.remote.RemoteServerBinding;
import org.apache.shenyu.plugin.agent.gateway.remote.RemoteServiceCredentialResolver;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import reactor.core.publisher.Mono;

import java.net.InetSocketAddress;
import java.net.URI;
import java.time.Duration;
import java.time.Instant;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.BooleanSupplier;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ManagedCatalogRefreshConfigurationTest {

    private static final Duration WAIT = Duration.ofSeconds(5);

    private static final URI TRUSTED_ENDPOINT = URI.create("https://192.0.2.10:443/mcp");

    private final ApplicationContextRunner runner = new ApplicationContextRunner()
            .withUserConfiguration(AgentGatewayPluginConfiguration.class)
            .withPropertyValues("shenyu.plugins.agent.gateway.enabled=true", "shenyu.plugins.agent.gateway.aggregation.enabled=true");

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void syncedConcurrencyAndDeletionFenceRemoteHandshakes(final boolean removeDuringHandshake) throws Exception {
        final CountDownLatch bothEntered = new CountDownLatch(2);
        final CountDownLatch release = new CountDownLatch(1);
        final AtomicReference<ManagedRemoteMcpCatalog> owned = new AtomicReference<>();
        final int registered = AgentMcpCatalogMetrics.snapshots().size();
        try (Fixture alpha = new Fixture("alpha", bothEntered, release); Fixture beta = new Fixture("beta", bothEntered, release)) {
            alpha.armed.set(true);
            beta.armed.set(true);
            runner.withPropertyValues("shenyu.plugins.agent.gateway.aggregation.allowed-endpoints=" + alpha.endpoint + "," + beta.endpoint)
                    .withBean(RemoteServiceCredentialResolver.class, () -> target -> new RemoteServerBinding.Credential(target, "fixture-token"))
                    .run(context -> {
                        assertNull(context.getStartupFailure());
                        ManagedRemoteMcpCatalog catalog = context.getBean(ManagedRemoteMcpCatalog.class);
                        owned.set(catalog);
                        final PluginDataHandler handler = context.getBean(PluginDataHandler.class);
                        String source = "{\"aggregation\":{\"revision\":\"v1\",\"refreshConcurrency\":2,\"servers\":["
                                + server("alpha", alpha.endpoint) + "," + server("beta", beta.endpoint) + "],\"refreshPolicy\":{"
                                + "\"refreshIntervalMs\":60000,\"initialBackoffMs\":1000,\"maxBackoffMs\":30000,\"maxRetries\":3}}}";
                        try {
                            handler.handlerPlugin(event(source));
                            assertTrue(bothEntered.await(5, TimeUnit.SECONDS), "Both handshakes must start before either is released");
                            if (removeDuringHandshake) {
                                handler.removePlugin(event(source));
                            }
                        } finally {
                            release.countDown();
                        }
                        if (removeDuringHandshake) {
                            await(() -> catalog.diagnosticsState().lifecycle().currentVersion() > 1L
                                    && catalog.diagnosticsState().lifecycle().clients() == 0);
                            assertTrue(!catalog.diagnosticsState().refresh().available());
                        } else {
                            await(() -> catalog.diagnosticsState().refresh().available());
                            Set<String> names = catalog.withSnapshot(snapshot -> Mono.just(new ObjectMapper().valueToTree(snapshot.definitions())))
                                    .block(WAIT).properties().stream().map(Map.Entry::getKey).collect(Collectors.toSet());
                            assertEquals(Set.of("alpha.lookup", "beta.lookup"), names);
                            assertEquals(2, catalog.diagnosticsState().lifecycle().clients());
                            handler.removePlugin(event(source));
                            assertTrue(!catalog.diagnosticsState().refresh().available());
                            await(() -> catalog.diagnosticsState().lifecycle().clients() == 0);
                        }
                    });
        } finally {
            release.countDown();
        }
        assertTrue(owned.get().diagnosticsState().lifecycle().closed());
        assertEquals(registered, AgentMcpCatalogMetrics.snapshots().size());
    }

    @ParameterizedTest
    @ValueSource(strings = {"\"refreshConcurrency\":0", "\"refreshConcurrency\":9", "\"refreshConcurrency\":\"2\"",
        "\"refreshConcurrency\":2.0", "\"refreshPolicy\":null", "\"refreshPolicy\":{}"})
    void syncedInvalidPolicyIsRejectedBeforeCredentialResolution(final String field) {
        final AtomicInteger resolutions = new AtomicInteger();
        runner.withPropertyValues("shenyu.plugins.agent.gateway.aggregation.allowed-endpoints=" + TRUSTED_ENDPOINT)
                .withBean(RemoteServiceCredentialResolver.class, () -> target -> {
                    resolutions.incrementAndGet();
                    throw new SecurityException("unused fixture resolver");
                }).run(context -> {
                    ManagedRemoteMcpCatalog catalog = context.getBean(ManagedRemoteMcpCatalog.class);
                    String source = "{\"aggregation\":{\"revision\":\"v1\",\"servers\":[" + server("orders", TRUSTED_ENDPOINT) + "]," + field + "}}";
                    context.getBean(PluginDataHandler.class).handlerPlugin(event(source));
                    await(() -> "IllegalArgumentException".equals(catalog.diagnosticsState().lastFailure()));
                    assertEquals(0, resolutions.get());
                    assertEquals(0L, catalog.diagnosticsState().refresh().attempts());
                    assertEquals(0, catalog.diagnosticsState().lifecycle().clients());
                    assertTrue(catalog.diagnosticsState().refresh().failureActive());
                });
    }

    @Test
    void syncedRefreshConfigurationCannotExpandDeploymentAllowlist() {
        final AtomicInteger resolutions = new AtomicInteger();
        runner.withPropertyValues("shenyu.plugins.agent.gateway.aggregation.allowed-endpoints=" + TRUSTED_ENDPOINT)
                .withBean(RemoteServiceCredentialResolver.class, () -> target -> {
                    resolutions.incrementAndGet();
                    throw new SecurityException("unused fixture resolver");
                }).run(context -> {
                    ManagedRemoteMcpCatalog catalog = context.getBean(ManagedRemoteMcpCatalog.class);
                    String source = "{\"aggregation\":{\"revision\":\"v1\",\"refreshConcurrency\":8,\"servers\":["
                            + server("orders", URI.create("https://192.0.2.11:443/mcp")) + "]}}";
                    context.getBean(PluginDataHandler.class).handlerPlugin(event(source));
                    await(() -> "SecurityException".equals(catalog.diagnosticsState().lastFailure()));
                    assertEquals(0, resolutions.get());
                    assertEquals(0, catalog.diagnosticsState().lifecycle().clients());
                    assertTrue(!catalog.diagnosticsState().refresh().available());
                });
    }

    private static PluginData event(final String source) {
        PluginData data = new PluginData();
        data.setEnabled(true);
        data.setConfig(source);
        return data;
    }

    private static String server(final String name, final URI endpoint) {
        return "{\"name\":\"" + name + "\",\"endpoint\":\"" + endpoint
                + "\",\"credentialRef\":\"service/" + name + "\",\"credentialVersion\":\"v1\"}";
    }

    private static void await(final BooleanSupplier condition) throws InterruptedException {
        long deadline = System.nanoTime() + WAIT.toNanos();
        while (!condition.getAsBoolean() && System.nanoTime() < deadline) {
            Thread.sleep(5);
        }
        assertTrue(condition.getAsBoolean());
    }

    private static final class Fixture implements AutoCloseable {

        private final ObjectMapper json = new ObjectMapper();

        private final HttpServer http;

        private final URI endpoint;

        private final AtomicBoolean armed = new AtomicBoolean();

        private Fixture(final String name, final CountDownLatch entered, final CountDownLatch release) throws Exception {
            http = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
            http.createContext("/mcp", exchange -> {
                try {
                    if ("DELETE".equals(exchange.getRequestMethod())) {
                        exchange.sendResponseHeaders(204, -1);
                        return;
                    }
                    var request = json.readTree(exchange.getRequestBody());
                    if ("notifications/initialized".equals(request.path("method").asText())) {
                        exchange.sendResponseHeaders(202, -1);
                        return;
                    }
                    var result = json.createObjectNode();
                    if ("initialize".equals(request.path("method").asText())) {
                        if (armed.get()) {
                            entered.countDown();
                            try {
                                assertTrue(release.await(5, TimeUnit.SECONDS));
                            } catch (InterruptedException error) {
                                Thread.currentThread().interrupt();
                                throw new IllegalStateException("Interrupted fixture", error);
                            }
                        }
                        result.put("protocolVersion", "2025-06-18");
                        result.putObject("capabilities").putObject("tools");
                        result.putObject("serverInfo").put("name", name).put("version", "1");
                    } else {
                        result.putArray("tools").addObject().put("name", "lookup").put("description", "Fixture lookup")
                                .putObject("inputSchema").put("type", "object").putObject("properties");
                    }
                    var response = json.createObjectNode().put("jsonrpc", "2.0");
                    response.set("id", request.get("id"));
                    response.set("result", result);
                    byte[] bytes = json.writeValueAsBytes(response);
                    exchange.getResponseHeaders().set("Content-Type", "application/json");
                    exchange.sendResponseHeaders(200, bytes.length);
                    exchange.getResponseBody().write(bytes);
                } finally {
                    exchange.close();
                }
            });
            http.start();
            endpoint = URI.create("http://127.0.0.1:" + http.getAddress().getPort() + "/mcp");
            var reference = new RemoteServerBinding.Config(name, endpoint, "service/" + name, "v1");
            var warmup = RemoteServerBinding.resolve(reference, Set.of(endpoint), target -> new RemoteServerBinding.Credential(target, "fixture-token")).newClient();
            try {
                warmup.initialize().then(warmup.listTools(null)).contextWrite(context -> context.put(McpTransportContext.KEY,
                        McpTransportContext.create(Map.of("deadline", Instant.now().plusSeconds(30))))).block(Duration.ofSeconds(30));
            } finally {
                warmup.closeGracefully().block(WAIT);
            }
        }

        @Override
        public void close() {
            http.stop(0);
        }
    }
}
