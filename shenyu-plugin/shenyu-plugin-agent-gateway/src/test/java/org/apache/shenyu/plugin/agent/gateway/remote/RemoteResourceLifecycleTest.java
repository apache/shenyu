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

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import io.modelcontextprotocol.spec.McpSchema;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpRemoteCatalog;
import org.apache.shenyu.plugin.agent.gateway.resource.AgentResourceNamespace;
import org.junit.jupiter.api.Test;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;

import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class RemoteResourceLifecycleTest {

    private static final Duration WAIT = Duration.ofSeconds(3);

    private final String uri = AgentResourceNamespace.expose("orders", "test://status", false);

    @Test
    void inFlightReadPinsOldGenerationWhileNewReadsUseTheNewCatalog() {
        RemoteCatalogLifecycle lifecycle = new RemoteCatalogLifecycle(3, true);
        Fake first = new Fake("old");
        first.result = Sinks.one();
        Fake next = new Fake("new");
        try {
            lifecycle.refresh(List.of(config(first))).block(WAIT);
            final var pending = lifecycle.withSnapshot(directory -> directory.resources().read(uri, Set.of("orders.status"))).toFuture();
            assertEquals(1, lifecycle.diagnosticsState().references());
            lifecycle.refresh(List.of(config(next))).block(WAIT);
            assertEquals(0, first.closes.get());
            assertEquals("new", lifecycle.withSnapshot(directory -> directory.resources().read(uri, Set.of("orders.status"))).block(WAIT)
                    .path("contents").get(0).path("text").asText());
            first.result.tryEmitValue(first.contents());
            assertEquals("old", Mono.fromFuture(pending).block(WAIT).path("contents").get(0).path("text").asText());
            assertEquals(1, first.closes.get());
            assertEquals(0, lifecycle.diagnosticsState().references());
        } finally {
            lifecycle.closeGracefully().block(WAIT);
        }
        assertEquals(1, next.closes.get());
    }

    @Test
    void cancellationReleasesResourceLeaseAndClosesOwnedSession() {
        RemoteCatalogLifecycle lifecycle = new RemoteCatalogLifecycle(3, true);
        Fake fake = new Fake("pending");
        fake.result = Sinks.one();
        lifecycle.refresh(List.of(config(fake))).block(WAIT);
        var pending = lifecycle.withSnapshot(directory -> directory.resources().read(uri, Set.of("orders.status"))).toFuture();
        assertEquals(1, lifecycle.diagnosticsState().references());
        pending.cancel(true);
        assertEquals(0, lifecycle.diagnosticsState().references());
        lifecycle.closeGracefully().block(WAIT);
        assertEquals(1, fake.closes.get());
        assertEquals(0, lifecycle.diagnosticsState().clients());
    }

    @Test
    void malformedRemoteResourceCatalogDoesNotPublishPartialToolsOrResources() {
        RemoteCatalogLifecycle lifecycle = new RemoteCatalogLifecycle(3, true);
        Fake fake = new Fake("malformed");
        fake.badPage = true;
        try {
            assertThrows(IllegalArgumentException.class, () -> lifecycle.refresh(List.of(config(fake))).block(WAIT));
            assertThrows(AgentMcpRemoteCatalog.CatalogUnavailableException.class, () -> lifecycle.withSnapshot(directory -> Mono.just(true)).block(WAIT));
            assertEquals(1, fake.closes.get());
            assertEquals(0, lifecycle.diagnosticsState().clients());
        } finally {
            lifecycle.closeGracefully().block(WAIT);
        }
    }

    private RemoteCatalogLifecycle.Config config(final Fake fake) {
        return new RemoteCatalogLifecycle.Config("orders", fake.version, () -> fake);
    }

    private static final class Fake implements RemoteMcpEndpoint {

        private final ObjectMapper mapper = new ObjectMapper();

        private final AtomicInteger closes = new AtomicInteger();

        private final String version;

        private Sinks.One<ObjectNode> result;

        private boolean badPage;

        private Fake(final String version) {
            this.version = version;
        }

        @Override
        public Mono<McpSchema.InitializeResult> initialize() {
            return Mono.just(mapper.convertValue(Map.of("protocolVersion", "2025-06-18", "capabilities", Map.of("resources", Map.of()),
                    "serverInfo", Map.of("name", "resources", "version", version)), McpSchema.InitializeResult.class));
        }

        @Override
        public Mono<McpSchema.ListToolsResult> listTools(final String cursor) {
            return Mono.error(new AssertionError("Resources-only server must not list tools"));
        }

        @Override
        public Mono<ObjectNode> listResources(final String cursor) {
            ObjectNode page = mapper.createObjectNode();
            if (!badPage) {
                page.putArray("resources").addObject().put("name", "status").put("uri", "test://status");
            }
            return Mono.just(page);
        }

        @Override
        public Mono<ObjectNode> listResourceTemplates(final String cursor) {
            ObjectNode page = mapper.createObjectNode();
            page.putArray("resourceTemplates");
            return Mono.just(page);
        }

        @Override
        public Mono<ObjectNode> readResource(final String identifier) {
            return Objects.isNull(result) ? Mono.just(contents()) : result.asMono();
        }

        private ObjectNode contents() {
            ObjectNode contents = mapper.createObjectNode();
            contents.putArray("contents").addObject().put("uri", "test://status").put("text", version);
            return contents;
        }

        @Override
        public Mono<McpSchema.CallToolResult> callTool(final McpSchema.CallToolRequest request) {
            return Mono.error(new AssertionError("No tool calls"));
        }

        @Override
        public Mono<ObjectNode> callRaw(final McpSchema.CallToolRequest request) {
            return Mono.error(new AssertionError("No tool calls"));
        }

        @Override
        public Mono<Void> closeGracefully() {
            return Mono.fromRunnable(closes::incrementAndGet);
        }

        @Override
        public int pending() {
            return 0;
        }
    }
}
