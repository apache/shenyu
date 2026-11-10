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
import com.sun.net.httpserver.HttpServer;
import io.modelcontextprotocol.common.McpTransportContext;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.metrics.AgentMcpCatalogMetrics;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpRemoteCatalog.CatalogUnavailableException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;
import reactor.core.publisher.Sinks;
import reactor.test.scheduler.VirtualTimeScheduler;

import java.net.InetSocketAddress;
import java.net.URI;
import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.BooleanSupplier;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ManagedRemoteMcpCatalogRefreshTest {

    private static final Duration WAIT = Duration.ofSeconds(5);

    private static final URI UNREACHABLE = URI.create("https://192.0.2.10:443/mcp");

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void pollingPreservesResourceOptInAndPublishesChangedDefinitionsTogether(final boolean resourcesEnabled) throws Exception {
        try (Fixture fixture = new Fixture()) {
            final VirtualTimeScheduler timer = VirtualTimeScheduler.create();
            try (ManagedRemoteMcpCatalog catalog = new ManagedRemoteMcpCatalog(Set.of(fixture.endpoint), Set.of(),
                    target -> new RemoteServerBinding.Credential(target, "fixed-test-token"), resourcesEnabled,
                    Schedulers.newBoundedElastic(1, 16, "agent-mcp-resource-refresh-test"), timer)) {
                catalog.reload(config(fixture.endpoint, "v1")).block(WAIT);
                assertTrue(catalog.withSnapshot(snapshot -> {
                    assertEquals(resourcesEnabled ? 1 : 0, snapshot.resourceDefinitions().size());
                    assertTrue(snapshot.definitions().containsKey("orders.lookup"));
                    return Mono.just(new ObjectMapper().createObjectNode());
                }).block(WAIT).isObject());
                fixture.tool.set("changed");
                fixture.resource.set("changed");
                timer.advanceTimeBy(Duration.ofSeconds(60));
                await(() -> catalog.refreshTiming().nextRefreshAt() == 120000L);
                assertTrue(catalog.withSnapshot(snapshot -> {
                    assertEquals(Set.of("orders.changed"), snapshot.definitions().keySet());
                    assertEquals(resourcesEnabled ? 1 : 0, snapshot.resourceDefinitions().size());
                    if (resourcesEnabled) {
                        assertEquals("orders.changed", snapshot.resourceDefinitions().values().iterator().next().path("name").asText());
                    }
                    return Mono.just(new ObjectMapper().createObjectNode());
                }).block(WAIT).isObject());
                fixture.resource.set(null);
                timer.advanceTimeBy(Duration.ofSeconds(60));
                await(() -> catalog.refreshTiming().nextRefreshAt() == 180000L);
                assertTrue(catalog.withSnapshot(snapshot -> {
                    assertTrue(snapshot.resourceDefinitions().isEmpty());
                    assertEquals(Set.of("orders.changed"), snapshot.definitions().keySet());
                    return Mono.just(new ObjectMapper().createObjectNode());
                }).block(WAIT).isObject());
                assertEquals(resourcesEnabled ? 3 : 0, fixture.resourceLists.get());
            }
        }
    }

    @Test
    void pollingDiscoversChangedAndRemovedToolsWithoutChangingConfigRevision() throws Exception {
        try (Fixture fixture = new Fixture()) {
            VirtualTimeScheduler timer = VirtualTimeScheduler.create();
            AtomicInteger resolutions = new AtomicInteger();
            try (ManagedRemoteMcpCatalog catalog = catalog(fixture.endpoint, timer, target -> {
                resolutions.incrementAndGet();
                return new RemoteServerBinding.Credential(target, "fixed-test-token");
            })) {
                catalog.reload(config(fixture.endpoint, "v1")).block(WAIT);
                final long revision = catalog.diagnosticsState().revision();
                assertEquals(Set.of("orders.lookup"), definitions(catalog));
                assertEquals(1L, catalog.diagnosticsState().refresh().attempts());
                assertEquals(0L, catalog.diagnosticsState().refresh().recoveries());
                assertEquals(0L, catalog.diagnosticsState().refresh().lastSuccessAgeMillis());
                assertTrue(catalog.diagnosticsState().refresh().available());
                assertEquals(60000L, catalog.refreshTiming().nextRefreshAt());
                timer.advanceTimeBy(Duration.ofSeconds(59));
                assertEquals(1, resolutions.get());
                assertEquals(59000L, catalog.diagnosticsState().refresh().lastSuccessAgeMillis());
                fixture.tool.set("changed");
                fixture.schemaProperty.set("orderId");
                timer.advanceTimeBy(Duration.ofSeconds(1));
                await(() -> catalog.refreshTiming().nextRefreshAt() == 120000L);
                assertEquals(Set.of("orders.changed"), definitions(catalog));
                assertTrue(catalog.withSnapshot(snapshot -> Mono.just(snapshot.definitions().get("orders.changed")))
                        .block(WAIT).path("inputSchema").path("properties").has("orderId"));
                fixture.tool.set(null);
                timer.advanceTimeBy(Duration.ofSeconds(60));
                await(() -> catalog.refreshTiming().nextRefreshAt() == 180000L);
                assertEquals(Set.of(), definitions(catalog));
                assertEquals(3, resolutions.get());
                assertEquals(revision, catalog.diagnosticsState().revision());
            }
            assertTrue(timer.isDisposed());
        }
    }

    @Test
    void expiredSnapshotRejectsNewLeasesEvenWhenTimerHasNotRun() throws Exception {
        try (Fixture fixture = new Fixture()) {
            final AtomicLong now = new AtomicLong();
            final VirtualTimeScheduler timer = VirtualTimeScheduler.create();
            final Sinks.One<com.fasterxml.jackson.databind.node.ObjectNode> pending = Sinks.one();
            try (ManagedRemoteMcpCatalog catalog = new ManagedRemoteMcpCatalog(Set.of(fixture.endpoint), Set.of(),
                    target -> new RemoteServerBinding.Credential(target, "fixed-test-token"),
                    Schedulers.newBoundedElastic(1, 16, "agent-mcp-config-test"), timer, now::get)) {
                catalog.reload(config(fixture.endpoint, "v1")).block(WAIT);
                final var leased = catalog.withSnapshot(snapshot -> pending.asMono()).toFuture();
                assertEquals(1, catalog.diagnosticsState().lifecycle().references());
                now.set(TimeUnit.MILLISECONDS.toNanos(59999));
                assertEquals(Set.of("orders.lookup"), definitions(catalog));
                now.set(TimeUnit.SECONDS.toNanos(60));
                assertThrows(CatalogUnavailableException.class, () -> definitions(catalog));
                assertTrue(!catalog.diagnosticsState().refresh().available());
                assertEquals(60000L, catalog.diagnosticsState().refresh().lastSuccessAgeMillis());
                assertEquals(1, catalog.diagnosticsState().lifecycle().clients(), "The already acquired lease still owns its client");
                pending.tryEmitValue(new ObjectMapper().createObjectNode().put("completed", true));
                assertTrue(Mono.fromFuture(leased).block(WAIT).path("completed").asBoolean());
                assertEquals(0, catalog.diagnosticsState().lifecycle().clients());
                timer.advanceTimeBy(Duration.ofSeconds(60));
                await(() -> catalog.refreshTiming().nextRefreshAt() == 120000L);
                assertEquals(Set.of("orders.lookup"), definitions(catalog));
                assertEquals(0L, catalog.diagnosticsState().refresh().lastSuccessAgeMillis());
            }
        }
    }

    @Test
    void failedConstructionDisposesBothOwnedSchedulers() {
        var worker = Schedulers.newBoundedElastic(1, 16, "agent-mcp-construction-test");
        VirtualTimeScheduler timer = VirtualTimeScheduler.create();
        final int sources = AgentMcpCatalogMetrics.snapshots().size();
        assertThrows(IllegalArgumentException.class, () -> new ManagedRemoteMcpCatalog(Set.of(URI.create("https://example.com:443/mcp")),
                Set.of(), target -> new RemoteServerBinding.Credential(target, "unused"), worker, timer));
        assertTrue(worker.isDisposed());
        assertTrue(timer.isDisposed());
        assertEquals(sources, AgentMcpCatalogMetrics.snapshots().size());
    }

    @Test
    void replacementDoesNotReuseOldPublicationAgeOrCountAsRecovery() throws Exception {
        try (Fixture fixture = new Fixture()) {
            VirtualTimeScheduler timer = VirtualTimeScheduler.create();
            try (ManagedRemoteMcpCatalog catalog = catalog(fixture.endpoint, timer, target -> {
                if ("v2".equals(target.credentialVersion())) {
                    throw new SecurityException("fixture credential unavailable");
                }
                return new RemoteServerBinding.Credential(target, "fixed-test-token");
            })) {
                catalog.reload(config(fixture.endpoint, "v1")).block(WAIT);
                timer.advanceTimeBy(Duration.ofSeconds(30));
                var before = catalog.diagnosticsState().refresh();
                assertEquals(30000L, before.lastSuccessAgeMillis());
                assertThrows(SecurityException.class, () -> catalog.reload(config(fixture.endpoint, "v2")).block(WAIT));
                var after = catalog.diagnosticsState().refresh();
                assertEquals(-1L, after.lastSuccessAgeMillis());
                assertEquals(2L, after.attempts());
                assertEquals(1L, after.failures());
                assertEquals(0L, after.recoveries());
                assertTrue(after.failureActive());
                assertTrue(!after.available());
                assertEquals(30000L, before.lastSuccessAgeMillis());
            }
        }
    }

    @Test
    void closingCatalogUnregistersItsMetricsSource() {
        final int before = AgentMcpCatalogMetrics.snapshots().size();
        VirtualTimeScheduler timer = VirtualTimeScheduler.create();
        ManagedRemoteMcpCatalog catalog = catalog(UNREACHABLE, timer, target -> {
            throw new AssertionError("Unused credential resolver");
        });
        try {
            assertEquals(before + 1, AgentMcpCatalogMetrics.snapshots().size());
            assertEquals(0L, catalog.diagnosticsState().refresh().attempts());
            assertEquals(-1L, catalog.diagnosticsState().refresh().lastSuccessAgeMillis());
        } finally {
            catalog.closeAsync().block(WAIT);
        }
        assertEquals(before, AgentMcpCatalogMetrics.snapshots().size());
    }

    @Test
    void outagesUseBoundedBackoffThenSlowProbesAndRecover() throws Exception {
        try (Fixture fixture = new Fixture()) {
            VirtualTimeScheduler timer = VirtualTimeScheduler.create();
            AtomicInteger resolutions = new AtomicInteger();
            try (ManagedRemoteMcpCatalog catalog = catalog(fixture.endpoint, timer, target -> {
                resolutions.incrementAndGet();
                return new RemoteServerBinding.Credential(target, "fixed-test-token");
            })) {
                fixture.status.set(503);
                catalog.accept(event(true, config(fixture.endpoint, "v1")));
                await(() -> catalog.refreshTiming().nextRefreshAt() == 1000L);
                assertThrows(CatalogUnavailableException.class, () -> definitions(catalog));
                timer.advanceTimeBy(Duration.ofMillis(999));
                assertEquals(1, resolutions.get());
                timer.advanceTimeBy(Duration.ofMillis(1));
                await(() -> catalog.refreshTiming().nextRefreshAt() == 3000L);
                timer.advanceTimeBy(Duration.ofSeconds(2));
                await(() -> catalog.refreshTiming().nextRefreshAt() == 7000L);
                timer.advanceTimeBy(Duration.ofSeconds(4));
                await(() -> catalog.refreshTiming().nextRefreshAt() == 67000L);
                assertEquals(4, resolutions.get());
                assertEquals(4, catalog.refreshTiming().consecutiveFailures());
                assertEquals(4L, catalog.diagnosticsState().refresh().attempts());
                assertEquals(4L, catalog.diagnosticsState().refresh().failures());
                assertEquals(-1L, catalog.diagnosticsState().refresh().lastSuccessAgeMillis());
                assertTrue(catalog.diagnosticsState().refresh().failureActive());
                timer.advanceTimeBy(Duration.ofSeconds(59));
                assertEquals(4, resolutions.get());
                fixture.status.set(200);
                timer.advanceTimeBy(Duration.ofSeconds(1));
                await(() -> catalog.refreshTiming().nextRefreshAt() == 127000L);
                assertEquals(0, catalog.refreshTiming().consecutiveFailures());
                assertEquals("", catalog.diagnosticsState().lastFailure());
                assertEquals(Set.of("orders.lookup"), definitions(catalog));
                assertEquals(5, resolutions.get());
                assertEquals(1L, catalog.diagnosticsState().refresh().recoveries());
                assertEquals(0L, catalog.diagnosticsState().refresh().lastSuccessAgeMillis());
                fixture.status.set(503);
                timer.advanceTimeBy(Duration.ofSeconds(60));
                await(() -> catalog.refreshTiming().nextRefreshAt() == 128000L);
                assertEquals(1, catalog.refreshTiming().consecutiveFailures());
                assertThrows(CatalogUnavailableException.class, () -> definitions(catalog));
                assertTrue(!catalog.diagnostics().toString().contains("fixed-test-token"));
                assertEquals(60000L, catalog.diagnosticsState().refresh().lastSuccessAgeMillis());
                assertEquals(5L, catalog.diagnosticsState().refresh().failures());
                assertEquals(1L, catalog.diagnosticsState().refresh().recoveries());
            }
        }
    }

    @Test
    void credentialRecoveryResolvesRotatedCredentialAfterCooldown() throws Exception {
        try (Fixture fixture = new Fixture()) {
            VirtualTimeScheduler timer = VirtualTimeScheduler.create();
            AtomicBoolean available = new AtomicBoolean(false);
            AtomicInteger resolutions = new AtomicInteger();
            try (ManagedRemoteMcpCatalog catalog = catalog(fixture.endpoint, timer, target -> {
                resolutions.incrementAndGet();
                if (!available.get()) {
                    throw new SecurityException("SECRET_STORE_UNAVAILABLE");
                }
                return new RemoteServerBinding.Credential(target, "rotated-test-token");
            })) {
                catalog.accept(event(true, config(fixture.endpoint, "v1")));
                await(() -> catalog.refreshTiming().nextRefreshAt() == 60000L);
                assertEquals("SecurityException", catalog.diagnosticsState().lastFailure());
                timer.advanceTimeBy(Duration.ofSeconds(59));
                assertEquals(1, resolutions.get());
                fixture.authorization.set("Bearer rotated-test-token");
                available.set(true);
                timer.advanceTimeBy(Duration.ofSeconds(1));
                await(() -> catalog.refreshTiming().nextRefreshAt() == 120000L);
                assertEquals(2, resolutions.get());
                assertEquals(Set.of("orders.lookup"), definitions(catalog));
                assertTrue(!catalog.diagnostics().toString().contains("SECRET"));
                assertTrue(!catalog.diagnostics().toString().contains("rotated-test-token"));
            }
        }
    }

    @Test
    void pollingWithdrawsOldDirectoryBeforeWaitingForCredentialResolution() throws Exception {
        try (Fixture fixture = new Fixture()) {
            final VirtualTimeScheduler timer = VirtualTimeScheduler.create();
            final AtomicBoolean waiting = new AtomicBoolean();
            final CountDownLatch entered = new CountDownLatch(1);
            final CountDownLatch release = new CountDownLatch(1);
            ManagedRemoteMcpCatalog catalog = catalog(fixture.endpoint, timer, target -> {
                if (waiting.get()) {
                    entered.countDown();
                    try {
                        assertTrue(release.await(5, TimeUnit.SECONDS));
                    } catch (InterruptedException error) {
                        Thread.currentThread().interrupt();
                    }
                }
                return new RemoteServerBinding.Credential(target, "fixed-test-token");
            });
            try {
                catalog.reload(config(fixture.endpoint, "v1")).block(WAIT);
                assertEquals(Set.of("orders.lookup"), definitions(catalog));
                waiting.set(true);
                timer.advanceTimeBy(Duration.ofSeconds(60));
                assertTrue(entered.await(5, TimeUnit.SECONDS));
                assertThrows(CatalogUnavailableException.class, () -> definitions(catalog));
                assertTrue(!catalog.diagnosticsState().refresh().available());
                assertEquals(0, catalog.diagnosticsState().lifecycle().clients());
                release.countDown();
                await(() -> catalog.refreshTiming().nextRefreshAt() == 120000L);
                assertEquals(Set.of("orders.lookup"), definitions(catalog));
            } finally {
                release.countDown();
                catalog.closeAsync().block(WAIT);
            }
        }
    }

    @Test
    void customPolicyControlsRuntimeRetryBudgetAndCooldown() throws Exception {
        try (Fixture fixture = new Fixture()) {
            VirtualTimeScheduler timer = VirtualTimeScheduler.create();
            AtomicInteger resolutions = new AtomicInteger();
            try (ManagedRemoteMcpCatalog catalog = catalog(fixture.endpoint, timer, target -> {
                resolutions.incrementAndGet();
                return new RemoteServerBinding.Credential(target, "fixed-test-token");
            })) {
                fixture.status.set(503);
                String source = config(fixture.endpoint, "v1").replace("\"servers\":", "\"refreshPolicy\":{"
                        + "\"refreshIntervalMs\":5000,\"initialBackoffMs\":1000,\"maxBackoffMs\":2000,\"maxRetries\":1},\"servers\":");
                catalog.accept(event(true, source));
                await(() -> catalog.refreshTiming().nextRefreshAt() == 1000L);
                timer.advanceTimeBy(Duration.ofSeconds(1));
                await(() -> catalog.refreshTiming().nextRefreshAt() == 6000L);
                assertEquals(2, resolutions.get());
                timer.advanceTimeBy(Duration.ofSeconds(4));
                assertEquals(2, resolutions.get());
                fixture.status.set(200);
                timer.advanceTimeBy(Duration.ofSeconds(1));
                await(() -> catalog.refreshTiming().nextRefreshAt() == 11000L);
                assertEquals(0, catalog.refreshTiming().consecutiveFailures());
                assertEquals(1L, catalog.diagnosticsState().refresh().recoveries());
                assertEquals(Set.of("orders.lookup"), definitions(catalog));
            }
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void disableAndCloseCancelPendingRecovery(final boolean close) throws Exception {
        VirtualTimeScheduler timer = VirtualTimeScheduler.create();
        AtomicInteger resolutions = new AtomicInteger();
        ManagedRemoteMcpCatalog catalog = catalog(UNREACHABLE, timer, target -> {
            resolutions.incrementAndGet();
            throw new SecurityException("fixture");
        });
        try {
            catalog.accept(event(true, config(UNREACHABLE, "v1")));
            await(() -> catalog.refreshTiming().nextRefreshAt() == 60000L);
            if (close) {
                catalog.closeAsync().block(WAIT);
                assertTrue(timer.isDisposed());
            } else {
                catalog.accept(event(false, "{}"));
                await(() -> catalog.diagnosticsState().lifecycle().currentVersion() > 1L);
                timer.advanceTimeBy(Duration.ofHours(1));
            }
            assertEquals(0L, catalog.refreshTiming().nextRefreshAt());
            assertEquals(1, resolutions.get());
        } finally {
            catalog.closeAsync().block(WAIT);
        }
    }

    @Test
    void replacementCancelsOldTimerAndMalformedConfigStopsRecovery() throws Exception {
        VirtualTimeScheduler timer = VirtualTimeScheduler.create();
        List<String> versions = new CopyOnWriteArrayList<>();
        try (ManagedRemoteMcpCatalog catalog = catalog(UNREACHABLE, timer, target -> {
            versions.add(target.credentialVersion());
            throw new SecurityException("fixture");
        })) {
            catalog.accept(event(true, config(UNREACHABLE, "v1")));
            await(() -> catalog.refreshTiming().nextRefreshAt() == 60000L);
            catalog.accept(event(true, config(UNREACHABLE, "v2")));
            await(() -> versions.size() == 2 && catalog.refreshTiming().nextRefreshAt() == 60000L);
            timer.advanceTimeBy(Duration.ofSeconds(60));
            await(() -> catalog.refreshTiming().nextRefreshAt() == 120000L);
            assertEquals(List.of("v1", "v2", "v2"), versions);
            catalog.accept(event(true, "[]"));
            await(() -> "IllegalArgumentException".equals(catalog.diagnosticsState().lastFailure()));
            timer.advanceTimeBy(Duration.ofHours(1));
            assertEquals(3, versions.size());
            assertEquals(0L, catalog.refreshTiming().nextRefreshAt());
        }
    }

    @Test
    void slowRefreshDoesNotAccumulateTimerTasksOrBlockTimerThread() throws Exception {
        VirtualTimeScheduler timer = VirtualTimeScheduler.create();
        CountDownLatch entered = new CountDownLatch(1);
        CountDownLatch release = new CountDownLatch(1);
        AtomicInteger resolutions = new AtomicInteger();
        ManagedRemoteMcpCatalog catalog = catalog(UNREACHABLE, timer, target -> {
            resolutions.incrementAndGet();
            assertTrue(Thread.currentThread().getName().startsWith("agent-mcp-config"));
            entered.countDown();
            try {
                assertTrue(release.await(5, TimeUnit.SECONDS));
            } catch (InterruptedException error) {
                Thread.currentThread().interrupt();
            }
            throw new SecurityException("fixture");
        });
        try {
            catalog.accept(event(true, config(UNREACHABLE, "v1")));
            assertTrue(entered.await(5, TimeUnit.SECONDS));
            timer.advanceTimeBy(Duration.ofHours(1));
            assertEquals(0L, catalog.refreshTiming().nextRefreshAt());
            assertEquals(1, resolutions.get());
            release.countDown();
            await(() -> catalog.refreshTiming().nextRefreshAt() == 3660000L);
        } finally {
            release.countDown();
            catalog.closeAsync().block(WAIT);
        }
    }

    @Test
    void manualReloadReportsInvalidConfigAndRejectsWorkAfterClose() {
        VirtualTimeScheduler timer = VirtualTimeScheduler.create();
        ManagedRemoteMcpCatalog catalog = catalog(UNREACHABLE, timer, target -> {
            throw new AssertionError("Invalid config must not resolve credentials");
        });
        try {
            assertThrows(IllegalArgumentException.class, () -> catalog.reload("[]").block(WAIT));
            assertEquals(0L, catalog.refreshTiming().nextRefreshAt());
        } finally {
            catalog.closeAsync().block(WAIT);
        }
        assertThrows(CatalogUnavailableException.class, () -> catalog.reload("{}").block(WAIT));
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void oversizedReplacementCancelsPollingAndClosesRetiredClients(final boolean manual) throws Exception {
        try (Fixture fixture = new Fixture()) {
            VirtualTimeScheduler timer = VirtualTimeScheduler.create();
            AtomicInteger resolutions = new AtomicInteger();
            try (ManagedRemoteMcpCatalog catalog = catalog(fixture.endpoint, timer, target -> {
                resolutions.incrementAndGet();
                return new RemoteServerBinding.Credential(target, "fixed-test-token");
            })) {
                catalog.reload(config(fixture.endpoint, "v1")).block(WAIT);
                String oversized = " ".repeat(16385);
                if (manual) {
                    assertThrows(IllegalArgumentException.class, () -> catalog.reload(oversized).block(WAIT));
                } else {
                    catalog.accept(event(true, oversized));
                }
                await(() -> catalog.diagnosticsState().lifecycle().clients() == 0);
                assertEquals("ConfigurationByteLimit", catalog.diagnosticsState().lastFailure());
                assertEquals(0L, catalog.refreshTiming().nextRefreshAt());
                assertThrows(CatalogUnavailableException.class, () -> definitions(catalog));
                timer.advanceTimeBy(Duration.ofHours(1));
                assertEquals(1, resolutions.get());
            }
        }
    }

    private static ManagedRemoteMcpCatalog catalog(final URI endpoint, final VirtualTimeScheduler timer,
                                                  final RemoteServiceCredentialResolver credentials) {
        return new ManagedRemoteMcpCatalog(Set.of(endpoint), Set.of(), credentials,
                Schedulers.newBoundedElastic(1, 16, "agent-mcp-config-test"), timer);
    }

    private static Set<String> definitions(final ManagedRemoteMcpCatalog catalog) {
        return catalog.withSnapshot(snapshot -> Mono.just(new ObjectMapper().valueToTree(snapshot.definitions())))
                .block(WAIT).properties().stream().map(Map.Entry::getKey).collect(java.util.stream.Collectors.toSet());
    }

    private static PluginData event(final boolean enabled, final String source) {
        PluginData event = new PluginData();
        event.setEnabled(enabled);
        event.setConfig(source);
        return event;
    }

    private static String config(final URI endpoint, final String version) {
        return "{\"aggregation\":{\"revision\":\"v1\",\"servers\":[{\"name\":\"orders\",\"endpoint\":\"" + endpoint
                + "\",\"credentialRef\":\"service/orders\",\"credentialVersion\":\"" + version + "\"}]}}";
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

        private final HttpServer server;

        private final URI endpoint;

        private final AtomicInteger status = new AtomicInteger(200);

        private final AtomicReference<String> tool = new AtomicReference<>("lookup");

        private final AtomicReference<String> resource = new AtomicReference<>("status");

        private final AtomicInteger resourceLists = new AtomicInteger();

        private final AtomicReference<String> schemaProperty = new AtomicReference<>();

        private final AtomicReference<String> authorization = new AtomicReference<>("Bearer fixed-test-token");

        private Fixture() throws Exception {
            server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
            server.createContext("/mcp", exchange -> {
                try {
                    if ("DELETE".equals(exchange.getRequestMethod())) {
                        exchange.sendResponseHeaders(204, -1);
                        return;
                    }
                    if (status.get() != 200 || !authorization.get().equals(exchange.getRequestHeaders().getFirst("Authorization"))) {
                        exchange.sendResponseHeaders(status.get() == 200 ? 401 : status.get(), -1);
                        return;
                    }
                    var request = json.readTree(exchange.getRequestBody());
                    if ("notifications/initialized".equals(request.path("method").asText())) {
                        exchange.sendResponseHeaders(202, -1);
                        return;
                    }
                    var result = json.createObjectNode();
                    if ("initialize".equals(request.path("method").asText())) {
                        result.put("protocolVersion", "2025-06-18");
                        var capabilities = result.putObject("capabilities");
                        capabilities.putObject("tools");
                        capabilities.putObject("resources");
                        result.putObject("serverInfo").put("name", "fixture").put("version", "1");
                    } else if ("resources/list".equals(request.path("method").asText())) {
                        resourceLists.incrementAndGet();
                        var resources = result.putArray("resources");
                        String name = resource.get();
                        if (Objects.nonNull(name)) {
                            resources.addObject().put("name", name).put("uri", "test://" + name).put("description", "Fixture resource");
                        }
                    } else if ("resources/templates/list".equals(request.path("method").asText())) {
                        result.putArray("resourceTemplates");
                    } else {
                        var tools = result.putArray("tools");
                        String name = tool.get();
                        if (Objects.nonNull(name)) {
                            var properties = tools.addObject().put("name", name).put("description", "Fixture lookup tool")
                                    .putObject("inputSchema").put("type", "object").putObject("properties");
                            String property = schemaProperty.get();
                            if (Objects.nonNull(property)) {
                                properties.putObject(property).put("type", "string");
                            }
                        }
                    }
                    var envelope = json.createObjectNode().put("jsonrpc", "2.0");
                    envelope.set("id", request.get("id"));
                    envelope.set("result", result);
                    byte[] bytes = json.writeValueAsBytes(envelope);
                    exchange.getResponseHeaders().set("Content-Type", "application/json");
                    exchange.sendResponseHeaders(200, bytes.length);
                    exchange.getResponseBody().write(bytes);
                } finally {
                    exchange.close();
                }
            });
            server.start();
            endpoint = URI.create("http://127.0.0.1:" + server.getAddress().getPort() + "/mcp");
            var reference = new RemoteServerBinding.Config("orders", endpoint, "service/orders", "v1");
            var warmup = RemoteServerBinding.resolve(reference, Set.of(endpoint), target -> new RemoteServerBinding.Credential(target, "fixed-test-token")).newClient();
            try {
                warmup.initialize().then(warmup.listTools(null)).contextWrite(context -> context.put(McpTransportContext.KEY,
                        McpTransportContext.create(Map.of("deadline", Instant.now().plusSeconds(30))))).block(Duration.ofSeconds(30));
            } finally {
                warmup.closeGracefully().block(WAIT);
            }
        }

        @Override
        public void close() {
            server.stop(0);
        }
    }
}
