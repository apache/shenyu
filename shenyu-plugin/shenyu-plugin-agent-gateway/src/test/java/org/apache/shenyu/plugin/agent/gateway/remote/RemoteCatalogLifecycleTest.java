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
import org.junit.jupiter.api.Test;
import reactor.core.Disposable;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;

import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class RemoteCatalogLifecycleTest {

    private static final Duration WAIT = Duration.ofSeconds(3);

    private static final Set<String> TOOLS = Set.of("orders.lookup", "orders.lookup.detail");

    @Test
    void discoversCompletePagesAndUsesOriginalDottedName() {
        Fake client = new Fake("one");
        RemoteCatalogLifecycle manager = new RemoteCatalogLifecycle(3);
        manager.refresh(List.of(config(client))).block(WAIT);
        assertEquals(TOOLS, manager.visible(TOOLS, TOOLS));
        assertEquals("lookup.detail", manager.callRaw("orders.lookup.detail", TOOLS, TOOLS, Map.of()).block(WAIT).path("original").textValue());
        var metadata = manager.definitions(TOOLS, TOOLS).get("orders.lookup");
        metadata.inputSchema().properties().clear();
        assertTrue(!manager.definitions(TOOLS, TOOLS).get("orders.lookup").inputSchema().properties().isEmpty());
        manager.closeGracefully().block(WAIT);
        assertEquals(1, client.closes.get());
    }

    @Test
    void keepsOldInflightVersionUntilItsLastLeaseReleases() {
        Fake old = new Fake("old");
        old.result = Sinks.one();
        Fake next = new Fake("next");
        RemoteCatalogLifecycle manager = new RemoteCatalogLifecycle(3);
        manager.refresh(List.of(config(old))).block(WAIT);
        final var inflight = manager.callRaw("orders.lookup", TOOLS, TOOLS, Map.of()).toFuture();
        manager.refresh(List.of(config(next))).block(WAIT);
        assertEquals(0, old.closes.get());
        assertEquals("next", manager.callRaw("orders.lookup", TOOLS, TOOLS, Map.of()).block(WAIT).path("version").textValue());
        old.result.tryEmitValue(new ObjectMapper().createObjectNode().put("version", "old"));
        assertEquals("old", Mono.fromFuture(inflight).block(WAIT).path("version").textValue());
        assertEquals(1, old.closes.get());
        manager.closeGracefully().block(WAIT);
    }

    @Test
    void cancellationReleasesOldGenerationWithoutClosingNewClient() {
        Fake old = new Fake("old");
        old.result = Sinks.one();
        Fake next = new Fake("next");
        RemoteCatalogLifecycle manager = new RemoteCatalogLifecycle(3);
        manager.refresh(List.of(config(old))).block(WAIT);
        Disposable cancelled = manager.callRaw("orders.lookup", TOOLS, TOOLS, Map.of()).subscribe();
        manager.refresh(List.of(config(next))).block(WAIT);
        cancelled.dispose();
        assertEquals(1, old.closes.get());
        assertEquals(0, next.closes.get());
        assertEquals(0, manager.diagnostics().get("references"));
        manager.closeGracefully().block(WAIT);
    }

    @Test
    void rejectsRepeatedCursorAndWithdrawsPartialDirectory() {
        Fake client = new Fake("bad");
        client.repeatCursor = true;
        RemoteCatalogLifecycle manager = new RemoteCatalogLifecycle(3);
        assertThrows(RuntimeException.class, () -> manager.refresh(List.of(config(client))).block(WAIT));
        assertThrows(IllegalStateException.class, () -> manager.visible(TOOLS, TOOLS));
        assertEquals(1, client.closes.get());
        manager.closeGracefully().block(WAIT);
    }

    @Test
    void collisionIsRejectedBeforeDirectoryPublication() {
        Fake client = new Fake("bad");
        RemoteCatalogLifecycle manager = new RemoteCatalogLifecycle(3);
        assertThrows(RuntimeException.class, () -> manager.refresh(List.of(config(client)), List.of(), Set.of("orders.lookup")).block(WAIT));
        assertEquals(0L, manager.diagnostics().get("currentVersion"));
        manager.closeGracefully().block(WAIT);
    }

    @Test
    void failedCloseRemainsQuarantinedAndPreventsNewAllocation() {
        Fake client = new Fake("bad");
        client.failClose = true;
        RemoteCatalogLifecycle manager = new RemoteCatalogLifecycle(3);
        manager.refresh(List.of(config(client))).block(WAIT);
        manager.withdraw();
        assertThrows(RuntimeException.class, () -> manager.closeGracefully().block(WAIT));
        assertEquals(1, manager.diagnostics().get("clients"));
        assertThrows(RuntimeException.class, () -> manager.refresh(List.of(config(new Fake("next")))).block(WAIT));
        assertEquals(1, client.closes.get());
    }

    @Test
    void emptyConfigurationRetiresRemoteAccessWithoutBreakingLocalOnlyDirectory() {
        Fake client = new Fake("old");
        RemoteCatalogLifecycle manager = new RemoteCatalogLifecycle(3);
        manager.refresh(List.of(config(client))).block(WAIT);
        for (int index = 0; index < 24; index++) {
            manager.refresh(List.of()).block(WAIT);
            assertTrue(manager.visible(TOOLS, TOOLS).isEmpty());
        }
        assertEquals(1, client.closes.get());
        assertEquals(false, manager.diagnostics().get("updating"));
        manager.closeGracefully().block(WAIT);
    }

    @Test
    void withdrawalAlsoRejectsCandidateCurrentlyInitializing() {
        Fake client = new Fake("pending");
        client.handshake = Sinks.one();
        RemoteCatalogLifecycle manager = new RemoteCatalogLifecycle(3);
        var update = manager.refresh(List.of(config(client))).toFuture();
        manager.withdraw();
        client.handshake.tryEmitValue(client.info());
        assertThrows(RuntimeException.class, () -> Mono.fromFuture(update).block(WAIT));
        assertEquals(0L, manager.diagnostics().get("currentVersion"));
        manager.closeGracefully().block(WAIT);
        assertEquals(1, client.closes.get());
    }

    private static RemoteCatalogLifecycle.Config config(final Fake client) {
        return new RemoteCatalogLifecycle.Config("orders", client.version, () -> client);
    }

    private static final class Fake implements RemoteMcpEndpoint {

        private final ObjectMapper json = new ObjectMapper();

        private final String version;

        private final AtomicInteger closes = new AtomicInteger();

        private Sinks.One<ObjectNode> result;

        private Sinks.One<McpSchema.InitializeResult> handshake;

        private boolean failClose;

        private boolean repeatCursor;

        private Fake(final String version) {
            this.version = version;
        }

        private McpSchema.InitializeResult info() {
            return json.convertValue(Map.of("protocolVersion", "2025-06-18", "capabilities", Map.of("tools", Map.of()),
                    "serverInfo", Map.of("name", "unit", "version", "1")), McpSchema.InitializeResult.class);
        }

        @Override
        public Mono<McpSchema.InitializeResult> initialize() {
            return java.util.Objects.isNull(handshake) ? Mono.just(info()) : handshake.asMono();
        }

        @Override
        public Mono<McpSchema.ListToolsResult> listTools(final String cursor) {
            Map<String, Object> tool = Map.of("name", java.util.Objects.isNull(cursor) ? "lookup" : "lookup.detail", "description", "unit tool",
                    "inputSchema", Map.of("type", "object", "properties", Map.of("token", Map.of("type", "string"))));
            var page = json.createObjectNode();
            page.set("tools", json.valueToTree(List.of(tool)));
            if (java.util.Objects.isNull(cursor) || repeatCursor) {
                page.put("nextCursor", "page-two");
            }
            return Mono.just(json.convertValue(page, McpSchema.ListToolsResult.class));
        }

        @Override
        public Mono<McpSchema.CallToolResult> callTool(final McpSchema.CallToolRequest request) {
            return Mono.error(new UnsupportedOperationException("Raw channel required"));
        }

        @Override
        public Mono<ObjectNode> callRaw(final McpSchema.CallToolRequest request) {
            return java.util.Objects.isNull(result) ? Mono.just(json.createObjectNode().put("version", version).put("original", request.name())) : result.asMono();
        }

        @Override
        public Mono<Void> closeGracefully() {
            closes.incrementAndGet();
            return failClose ? Mono.error(new IllegalStateException("injected close failure")) : Mono.empty();
        }

        @Override
        public int pending() {
            return 0;
        }
    }
}
