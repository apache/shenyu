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

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.google.gson.JsonParser;
import com.sun.net.httpserver.HttpServer;
import io.modelcontextprotocol.common.McpTransportContext;
import org.apache.shenyu.common.dto.AgentGatewayAggregationConfig.ProtocolProfile;
import org.apache.shenyu.common.dto.AgentGatewayMcpConfig;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.metrics.AgentRuntimeMetrics;
import org.apache.shenyu.plugin.agent.gateway.AgentTrafficContext;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpDispatcher;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpHttpHandler;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpRemoteCatalog;
import org.apache.shenyu.plugin.agent.gateway.security.AgentMcpIdentity;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolRegistry;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import reactor.adapter.JdkFlowAdapter;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.ArgumentMatchers;
import org.mockito.MockedStatic;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.junit.jupiter.params.provider.MethodSource;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BooleanSupplier;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

class ManagedRemoteMcpCatalogTest {

    private static final ObjectMapper JSON = new ObjectMapper();

    private static final Duration WAIT = Duration.ofSeconds(10);

    @Test
    void independentGatewaysCanListAndCallTheSameNativeUpstream() throws Exception {
        try (ProfileServer server = new ProfileServer()) {
            try (ManagedRemoteMcpCatalog first = server.catalog(); ManagedRemoteMcpCatalog second = server.catalog()) {
                String config = server.config(ProtocolProfile.NATIVE_2026_07_28);
                first.reload(config).block(WAIT);
                second.reload(config).block(WAIT);
                for (ManagedRemoteMcpCatalog catalog : List.of(first, second)) {
                    var gateway = handler(catalog, Set.of("orders.lookup"));
                    assertEquals("orders.lookup", send(gateway, "tools/list", true, 200).block(WAIT).at("/result/tools/0/name").asText());
                    assertEquals("2026-07-28", send(gateway, "tools/call", true, 200).block(WAIT).at("/result/content/0/text").asText());
                }
            }
            assertEquals(List.of("server/discover", "tools/list", "server/discover", "tools/list", "tools/call", "tools/call"), server.methods());
            assertTrue(server.requests.stream().allMatch(wire -> Objects.isNull(wire.header("MCP-Session-Id"))));
            assertEquals(6L, server.requests.stream().map(wire -> wire.body().path("id").textValue()).distinct().count());
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void usesRuleAndIdentityGrantsAndOnlyTheBoundServiceCredential(final boolean nativeProfile) throws Exception {
        try (ProfileServer server = new ProfileServer(); ManagedRemoteMcpCatalog catalog = server.catalog()) {
            ProtocolProfile profile = nativeProfile ? ProtocolProfile.NATIVE_2026_07_28 : ProtocolProfile.LEGACY_2025_06_18;
            catalog.reload(server.config(profile)).block(WAIT);
            var granted = handler(catalog, Set.of("orders.lookup"));
            var denied = handler(catalog, Set.of());
            assertTrue(send(denied, "tools/list", true, 200).block(WAIT).at("/result/tools").isEmpty());
            assertTrue(send(granted, "tools/list", false, 200).block(WAIT).at("/result/tools").isEmpty());
            send(denied, "tools/call", true, 403).block(WAIT);
            send(granted, "tools/call", false, 403).block(WAIT);
            assertFalse(server.methods().contains("tools/call"));
            assertEquals(profile.version(), send(granted, "tools/call", true, 200).block(WAIT).at("/result/content/0/text").asText());
            assertEquals(1L, server.methods().stream().filter("tools/call"::equals).count());
            assertTrue(server.requests.stream().allMatch(wire -> "Bearer service-secret".equals(wire.header("Authorization"))
                    && Objects.isNull(wire.header("Cookie")) && Objects.isNull(wire.header("X-Agent-Subject"))));
            var capabilities = catalog.diagnosticsState().lifecycle().capabilities();
            assertEquals(List.of(RemoteProtocolCapability.forProfile("orders", profile, true)), capabilities);
            assertEquals(List.of(capabilities.get(0).toMap()), catalog.diagnostics().get("capabilities"));
            for (String secret : List.of("service-secret", "agent-secret", "private-subject", "private-argument", "service/orders", "/mcp")) {
                assertFalse(catalog.diagnostics().toString().contains(secret));
                assertFalse(catalog.diagnosticsState().toString().contains(secret));
            }
            assertThrows(UnsupportedOperationException.class, () -> capabilities.get(0).supportedMethods().clear());
        }
    }

    @Test
    void mixedCatalogDiscoversResourcesOnlyForLegacyTargets() throws Exception {
        try (ProfileServer server = new ProfileServer(); ManagedRemoteMcpCatalog catalog = server.catalog()) {
            ObjectNode config = (ObjectNode) JSON.readTree(server.config(ProtocolProfile.NATIVE_2026_07_28));
            var servers = ((ObjectNode) config.get("aggregation")).withArray("servers");
            ObjectNode legacy = ((ObjectNode) servers.get(0)).deepCopy().put("name", "old");
            legacy.remove("protocolProfile");
            servers.add(legacy);
            catalog.reload(config.toString()).block(WAIT);
            catalog.withSnapshot(snapshot -> {
                assertEquals(Set.of("orders.lookup", "old.lookup"), snapshot.definitions().keySet());
                assertEquals(Set.of("old.status"), snapshot.resourceDefinitions().keySet());
                return Mono.just(JSON.createObjectNode());
            }).block(WAIT);
            var capabilities = catalog.diagnosticsState().lifecycle().capabilities();
            assertEquals(List.of("orders", "old"), capabilities.stream().map(RemoteProtocolCapability::server).toList());
            assertFalse(capabilities.get(0).resourcesSupported());
            assertTrue(capabilities.get(1).resourcesSupported());
            assertEquals(List.of("resources/list", "resources/templates/list"),
                    server.requests.stream().filter(wire -> wire.method().startsWith("resources/")).map(Wire::method).toList());
            assertTrue(server.requests.stream().filter(wire -> wire.method().startsWith("resources/")).allMatch(wire -> "legacy-session".equals(wire.header("MCP-Session-Id"))));
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"null", "\"\"", "\"unknown\""})
    void invalidProfileLeavesCurrentCatalogAvailableWithoutResolvingCredentials(final String profile) {
        URI endpoint = URI.create("https://192.0.2.10:443/mcp");
        try (ManagedRemoteMcpCatalog catalog = new ManagedRemoteMcpCatalog(Set.of(endpoint), Set.of(), reference -> {
            throw new AssertionError("An invalid profile reached credential resolution");
        })) {
            long active = catalog.reload("{}").block(WAIT);
            String invalid = config().replace("\"credentialVersion\"", "\"protocolProfile\":" + profile + ",\"credentialVersion\"");
            StepVerifier.create(catalog.reload(invalid)).expectError(IllegalArgumentException.class).verify();
            assertEquals(active, catalog.diagnosticsState().lifecycle().currentVersion());
            StepVerifier.create(catalog.withSnapshot(snapshot -> Mono.just(JSON.createObjectNode()))).expectNextCount(1).verifyComplete();
        }
    }

    @Test
    void leasedNativeCallSurvivesRefreshRollbackAndInvalidProfile() throws Exception {
        try (ProfileServer server = new ProfileServer(); ManagedRemoteMcpCatalog catalog = server.catalog()) {
            String nativeConfig = server.config(ProtocolProfile.NATIVE_2026_07_28);
            catalog.reload(nativeConfig).block(WAIT);
            var gateway = handler(catalog, Set.of("orders.lookup"));
            server.holdNextCall = true;
            var pending = send(gateway, "tools/call", true, 200).toFuture();
            try {
                assertEquals(1L, server.methods().stream().filter("tools/call"::equals).count());
                long previous = catalog.diagnosticsState().lifecycle().currentVersion();
                String invalid = nativeConfig.replace("native-2026-07-28", "unknown");
                StepVerifier.create(catalog.reload(invalid)).expectError(IllegalArgumentException.class).verify();
                assertEquals(previous, catalog.diagnosticsState().lifecycle().currentVersion());
                assertEquals("2026-07-28", send(gateway, "tools/call", true, 200).block(WAIT).at("/result/content/0/text").asText());
                catalog.reload(server.config(ProtocolProfile.LEGACY_2025_06_18)).block(WAIT);
                assertEquals("2025-06-18", send(gateway, "tools/call", true, 200).block(WAIT).at("/result/content/0/text").asText());
                catalog.reload(nativeConfig).block(WAIT);
                assertEquals("2026-07-28", send(gateway, "tools/call", true, 200).block(WAIT).at("/result/content/0/text").asText());
                assertFalse(pending.isDone());
                assertEquals(1, catalog.diagnosticsState().lifecycle().references());
            } finally {
                server.releaseCall.complete(null);
            }
            assertEquals("2026-07-28", Mono.fromFuture(pending).block(WAIT).at("/result/content/0/text").asText());
            assertEquals(0, catalog.diagnosticsState().lifecycle().references());
            assertEquals(List.of("2026-07-28", "2026-07-28", "2025-06-18", "2026-07-28"),
                    server.requests.stream().filter(wire -> "tools/call".equals(wire.method())).map(wire -> wire.header("MCP-Protocol-Version")).toList());
            assertEquals(1L, server.methods().stream().filter("DELETE"::equals).count());
        }
    }

    @Test
    void sameMountedCredentialBindsBothProfilesAndLegacyStillClosesItsSession(final @TempDir Path root) throws Exception {
        var directory = Files.createDirectories(root.resolve("service/orders"));
        Files.writeString(directory.resolve("v1.json"), """
                {"name":"orders","endpoint":"https://192.0.2.10:443/mcp","credentialRef":"service/orders","credentialVersion":"v1","bearerToken":"mounted-secret"}
                """);
        try (ProfileServer server = new ProfileServer();
                ManagedRemoteMcpCatalog catalog = new ManagedRemoteMcpCatalog(Set.of(ProfileServer.ENDPOINT), Set.of(), new FileRemoteServiceCredentialResolver(root))) {
            var gateway = handler(catalog, Set.of("orders.lookup"));
            catalog.reload(config()).block(WAIT);
            assertEquals("2025-06-18", send(gateway, "tools/call", true, 200).block(WAIT).at("/result/content/0/text").asText());
            String nativeConfig = server.config(ProtocolProfile.NATIVE_2026_07_28);
            StepVerifier.create(catalog.reload(nativeConfig.replace("\"orders\"", "\"other\""))).expectError(SecurityException.class).verify();
            catalog.reload(nativeConfig).block(WAIT);
            assertEquals("2026-07-28", send(gateway, "tools/call", true, 200).block(WAIT).at("/result/content/0/text").asText());
            catalog.close();
            assertEquals(List.of("initialize", "notifications/initialized", "tools/list", "tools/call", "DELETE",
                    "server/discover", "tools/list", "tools/call"), server.methods());
            for (Wire wire : server.requests) {
                assertEquals("Bearer mounted-secret", wire.header("Authorization"));
                boolean session = "2025-06-18".equals(wire.header("MCP-Protocol-Version")) && !"initialize".equals(wire.method());
                assertEquals(session ? "legacy-session" : null, wire.header("MCP-Session-Id"));
            }
        }
    }

    @ParameterizedTest(name = "{0}")
    @MethodSource("configurationFixtures")
    void validatesSharedContractBeforeCandidatePreparation(final String name, final boolean accepted, final String source) {
        ManagedRemoteMcpCatalog catalog = new ManagedRemoteMcpCatalog(Set.of(), Set.of(), reference -> {
            throw new IllegalStateException("Local-only fixtures must not resolve credentials");
        });
        try {
            long current = catalog.diagnosticsState().lifecycle().currentVersion();
            if (accepted) {
                StepVerifier.create(catalog.reload(source)).expectNextCount(1).verifyComplete();
            } else {
                StepVerifier.create(catalog.reload(source)).expectError(IllegalArgumentException.class).verify();
                assertEquals(current, catalog.diagnosticsState().lifecycle().currentVersion());
            }
        } finally {
            catalog.closeAsync().block(Duration.ofSeconds(5));
        }
    }

    private static java.util.stream.Stream<org.junit.jupiter.params.provider.Arguments> configurationFixtures() throws java.io.IOException {
        try (var input = java.util.Objects.requireNonNull(ManagedRemoteMcpCatalogTest.class.getResourceAsStream(
                "/META-INF/shenyu/config-contract-fixtures/agentGateway-plugin-v1.tsv"));
             var reader = new java.io.BufferedReader(new java.io.InputStreamReader(input, java.nio.charset.StandardCharsets.UTF_8))) {
            return reader.lines().filter(line -> !line.isBlank() && !line.startsWith("#")).map(line -> {
                String[] fields = line.split("\\t", 3);
                String source = "@missing".equals(fields[2]) ? null : "@blank".equals(fields[2]) ? "" : fields[2];
                return org.junit.jupiter.params.provider.Arguments.of(fields[0], Boolean.parseBoolean(fields[1]), source);
            }).toList().stream();
        }
    }


    @Test
    void catalogMetricsDescribeAcquisitionNotBusinessOutcome() {
        AgentRuntimeMetrics metrics = org.mockito.Mockito.mock(AgentRuntimeMetrics.class);
        ManagedRemoteMcpCatalog catalog = new ManagedRemoteMcpCatalog(Set.of(), Set.of(), reference -> {
            throw new IllegalStateException("No credentials expected");
        });
        try {
            StepVerifier.create(catalog.withSnapshot(snapshot -> Mono.error(new AgentMcpRemoteCatalog.CatalogUnavailableException()))
                    .contextWrite(context -> context.put(AgentRuntimeMetrics.KEY, metrics)))
                    .expectError(AgentMcpRemoteCatalog.CatalogUnavailableException.class).verify();
            org.mockito.Mockito.verify(metrics).catalog(true);
            org.mockito.Mockito.verify(metrics, org.mockito.Mockito.never()).catalog(false);
            catalog.close();
            StepVerifier.create(catalog.withSnapshot(snapshot -> Mono.just(new ObjectMapper().createObjectNode()))
                    .contextWrite(context -> context.put(AgentRuntimeMetrics.KEY, metrics)))
                    .expectError(AgentMcpRemoteCatalog.CatalogUnavailableException.class).verify();
            org.mockito.Mockito.verify(metrics).catalog(false);
        } finally {
            catalog.close();
        }
    }

    @Test
    void newerFencedEventClearsOnlyTheSupersededFailureDiagnostic() throws Exception {
        CountDownLatch entered = new CountDownLatch(1);
        CountDownLatch release = new CountDownLatch(1);
        ManagedRemoteMcpCatalog catalog = new ManagedRemoteMcpCatalog(Set.of(URI.create("https://192.0.2.10:443/mcp")), Set.of(), target -> {
            entered.countDown();
            try {
                assertTrue(release.await(10, TimeUnit.SECONDS));
            } catch (InterruptedException error) {
                Thread.currentThread().interrupt();
                throw new IllegalStateException("Interrupted fixture resolver");
            }
            return new RemoteServerBinding.Credential(target, "fixed-test-token");
        });
        try {
            catalog.accept(event(true, "[]"));
            await(() -> "IllegalArgumentException".equals(catalog.diagnosticsState().lastFailure()));
            catalog.accept(event(true, config()));
            assertTrue(entered.await(5, TimeUnit.SECONDS));
            assertEquals("", catalog.diagnosticsState().lastFailure());
            long resolvingRevision = catalog.diagnosticsState().revision();
            catalog.accept(event(false, "{}"));
            assertTrue(catalog.diagnosticsState().revision() > resolvingRevision);
            assertEquals("", catalog.diagnosticsState().lastFailure());
            release.countDown();
            await(() -> catalog.diagnosticsState().lifecycle().currentVersion() > 1L);
            assertEquals("", catalog.diagnosticsState().lastFailure());
            assertEquals(0, catalog.diagnosticsState().lifecycle().clients());
            catalog.accept(event(true, "[]"));
            await(() -> "IllegalArgumentException".equals(catalog.diagnosticsState().lastFailure()));
        } finally {
            release.countDown();
            catalog.closeAsync().block(Duration.ofSeconds(5));
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void withdrawnDuringSuccessfulCredentialResolutionDoesNotStartRemoteHandshake(final boolean emptyConfiguration) throws Exception {
        final CountDownLatch entered = new CountDownLatch(1);
        final CountDownLatch release = new CountDownLatch(1);
        AtomicInteger handshakes = new AtomicInteger();
        var server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        server.createContext("/ready", exchange -> {
            exchange.sendResponseHeaders(204, -1);
            exchange.close();
        });
        server.createContext("/mcp", exchange -> {
            try {
                if ("DELETE".equals(exchange.getRequestMethod())) {
                    exchange.sendResponseHeaders(204, -1);
                    return;
                }
                var json = new ObjectMapper();
                var request = json.readTree(exchange.getRequestBody());
                if ("notifications/initialized".equals(request.path("method").asText())) {
                    exchange.sendResponseHeaders(202, -1);
                    return;
                }
                var result = json.createObjectNode();
                if ("initialize".equals(request.path("method").asText())) {
                    handshakes.incrementAndGet();
                    result.put("protocolVersion", "2025-06-18");
                    result.putObject("capabilities").putObject("tools");
                    result.putObject("serverInfo").put("name", "fixture").put("version", "1");
                } else {
                    result.putArray("tools");
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
        URI endpoint = URI.create("http://127.0.0.1:" + server.getAddress().getPort() + "/mcp");
        warmFixture(endpoint);
        handshakes.set(0);
        ManagedRemoteMcpCatalog catalog = new ManagedRemoteMcpCatalog(Set.of(endpoint), Set.of(), target -> {
            entered.countDown();
            try {
                assertTrue(release.await(10, TimeUnit.SECONDS));
            } catch (InterruptedException error) {
                Thread.currentThread().interrupt();
                throw new IllegalStateException("Interrupted fixture resolver");
            }
            return new RemoteServerBinding.Credential(target, "fixed-test-token");
        });
        try {
            catalog.accept(event(true, config().replace("https://192.0.2.10:443/mcp", endpoint.toString())));
            assertTrue(entered.await(5, TimeUnit.SECONDS));
            catalog.accept(event(emptyConfiguration, "{}"));
            release.countDown();
            await(() -> catalog.diagnosticsState().lifecycle().currentVersion() > 1L);
            assertEquals(0, handshakes.get(), "A superseded enable event must not allocate or initialize a remote client");
            assertEquals(0, catalog.diagnosticsState().lifecycle().clients());
            long disabledVersion = catalog.diagnosticsState().lifecycle().currentVersion();
            catalog.accept(event(true, config().replace("https://192.0.2.10:443/mcp", endpoint.toString())));
            await(() -> catalog.diagnosticsState().lifecycle().currentVersion() > disabledVersion);
            assertEquals(1, handshakes.get(), "A fresh explicit enable must still discover the remote directory");
            catalog.accept(event(false, "{}"));
            await(() -> catalog.diagnosticsState().lifecycle().currentVersion() > disabledVersion + 1L);
            assertEquals(0, catalog.diagnosticsState().lifecycle().clients());
        } finally {
            release.countDown();
            catalog.closeAsync().block(Duration.ofSeconds(5));
            server.stop(0);
        }
    }

    @Test
    void resolvesCredentialsOnOwnedWorkerAndDropsSupersededEnableEvents() throws Exception {
        CountDownLatch entered = new CountDownLatch(1);
        CountDownLatch release = new CountDownLatch(1);
        final AtomicInteger resolutions = new AtomicInteger();
        final AtomicBoolean workerThread = new AtomicBoolean();
        URI endpoint = URI.create("https://192.0.2.10:443/mcp");
        final ManagedRemoteMcpCatalog catalog = new ManagedRemoteMcpCatalog(Set.of(endpoint), Set.of(), target -> {
            resolutions.incrementAndGet();
            workerThread.set(Thread.currentThread().getName().startsWith("agent-mcp-config"));
            entered.countDown();
            try {
                assertTrue(release.await(10, TimeUnit.SECONDS));
            } catch (InterruptedException error) {
                Thread.currentThread().interrupt();
            }
            throw new SecurityException("SECRET_STORE_FAILURE");
        });
        try {
            catalog.accept(event(true, config()));
            assertTrue(entered.await(2, TimeUnit.SECONDS));
            catalog.accept(event(true, config()));
            catalog.accept(event(false, config()));
            release.countDown();
            await(() -> catalog.diagnosticsState().lifecycle().currentVersion() > 1L);
            assertTrue(workerThread.get());
            assertEquals(1, resolutions.get());
            assertEquals(0, catalog.diagnosticsState().lifecycle().clients());
        } finally {
            release.countDown();
            catalog.closeAsync().block(Duration.ofSeconds(5));
        }
    }

    @Test
    void boundsQueueAndRequiresExplicitRefreshAfterOverflow() throws Exception {
        CountDownLatch entered = new CountDownLatch(1);
        CountDownLatch release = new CountDownLatch(1);
        final ManagedRemoteMcpCatalog catalog = new ManagedRemoteMcpCatalog(Set.of(URI.create("https://192.0.2.10:443/mcp")), Set.of(), target -> {
            entered.countDown();
            try {
                assertTrue(release.await(10, TimeUnit.SECONDS));
            } catch (InterruptedException error) {
                Thread.currentThread().interrupt();
            }
            throw new SecurityException("injected");
        });
        try {
            catalog.accept(event(true, config()));
            assertTrue(entered.await(2, TimeUnit.SECONDS));
            for (int index = 0; index < 32; index++) {
                catalog.accept(event(true, config()));
            }
            assertEquals("ConfigurationQueueRejected", catalog.diagnosticsState().lastFailure());
            assertEquals(0L, catalog.diagnosticsState().lifecycle().currentVersion());
            release.countDown();
            await(() -> {
                if (catalog.diagnosticsState().lifecycle().currentVersion() > 1L) {
                    return true;
                }
                // Recovery remains explicit while the worker drains superseded events.
                catalog.accept(event(true, "{}"));
                return false;
            });
            assertEquals(0, catalog.diagnosticsState().lifecycle().clients());
        } finally {
            release.countDown();
            catalog.closeAsync().block(Duration.ofSeconds(5));
        }
    }

    private static AgentMcpHttpHandler handler(final ManagedRemoteMcpCatalog catalog, final Set<String> grants) {
        return new AgentMcpHttpHandler(new AgentMcpDispatcher(new AgentToolRegistry(List.of()), "node", "1", catalog),
                exchange -> Mono.just(new AgentMcpIdentity("private-subject", grants)));
    }

    private static Mono<JsonNode> send(final AgentMcpHttpHandler gateway, final String method, final boolean ruleAllows, final int status) {
        ObjectNode body = JSON.createObjectNode().put("jsonrpc", "2.0").put("id", "same-agent-id").put("method", method);
        ObjectNode params = body.putObject("params");
        params.putObject("_meta").put("io.modelcontextprotocol/protocolVersion", "2026-07-28").putObject("io.modelcontextprotocol/clientCapabilities");
        var request = MockServerHttpRequest.post("/agent").contentType(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.ACCEPT, "application/json, text/event-stream")
                .header(HttpHeaders.AUTHORIZATION, "Bearer agent-secret").header(HttpHeaders.COOKIE, "private-cookie")
                .header("X-Agent-Subject", "forged-subject").header("MCP-Protocol-Version", "2026-07-28").header("Mcp-Method", method);
        if ("tools/call".equals(method)) {
            params.put("name", "orders.lookup").putObject("arguments").put("key", "private-argument");
            request.header("Mcp-Name", "orders.lookup");
        }
        var exchange = MockServerWebExchange.from(request.body(body.toString()));
        var config = AgentGatewayMcpConfig.parse(JsonParser.parseString(ruleAllows ? "{\"allowedTools\":[\"orders.lookup\"]}" : "{\"allowedTools\":[]}"), true);
        return gateway.handle(exchange, config, new AgentTrafficContext("trusted-request", "mcp", "selector", "rule"), 1)
                .then(Mono.defer(() -> {
                    assertEquals(status, exchange.getResponse().getStatusCode().value());
                    return exchange.getResponse().getBodyAsString();
                })).flatMap(text -> Mono.fromCallable(() -> JSON.readTree(text)));
    }

    private static void warmFixture(final URI endpoint) throws Exception {
        // Warm fixture transport and protocol classes before measuring control-update behavior.
        var ready = HttpRequest.newBuilder(endpoint.resolve("/ready")).timeout(Duration.ofSeconds(30)).GET().build();
        assertEquals(204, HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(30)).build()
                .send(ready, HttpResponse.BodyHandlers.discarding()).statusCode());
        var reference = new RemoteServerBinding.Config("orders", endpoint, "service/orders", "v1");
        var warmup = RemoteServerBinding.resolve(reference, Set.of(endpoint), target -> new RemoteServerBinding.Credential(target, "fixed-test-token")).newClient();
        try {
            warmup.initialize().then(warmup.listTools(null))
                    .contextWrite(context -> context.put(McpTransportContext.KEY,
                            McpTransportContext.create(Map.of("deadline", Instant.now().plusSeconds(30)))))
                    .block(Duration.ofSeconds(30));
        } finally {
            warmup.closeGracefully().block(Duration.ofSeconds(5));
        }
    }

    private static PluginData event(final boolean enabled, final String source) {
        PluginData event = new PluginData();
        event.setEnabled(enabled);
        event.setConfig(source);
        return event;
    }

    private static String config() {
        return "{\"aggregation\":{\"revision\":\"v1\",\"servers\":[{\"name\":\"orders\",\"endpoint\":\"https://192.0.2.10:443/mcp\","
                + "\"credentialRef\":\"service/orders\",\"credentialVersion\":\"v1\"}]}}";
    }

    private static void await(final BooleanSupplier condition) throws InterruptedException {
        long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(5);
        while (!condition.getAsBoolean() && System.nanoTime() < deadline) {
            Thread.sleep(5);
        }
        assertTrue(condition.getAsBoolean());
    }

    private record Wire(HttpRequest request, JsonNode body) {

        String method() {
            return body.path("method").asText(request.method());
        }

        String header(final String name) {
            return request.headers().firstValue(name).orElse(null);
        }
    }

    /** In-process upstream, replacing only the JDK client's network exchange for each new binding. */
    private static final class ProfileServer implements AutoCloseable {

        private static final URI ENDPOINT = URI.create("https://192.0.2.10:443/mcp");

        private final MockedStatic<RemoteMcpHttpSupport> clients = mockStatic(RemoteMcpHttpSupport.class, CALLS_REAL_METHODS);

        private final List<Wire> requests = new CopyOnWriteArrayList<>();

        private final CompletableFuture<Void> releaseCall = new CompletableFuture<>();

        private boolean holdNextCall;

        ProfileServer() {
            clients.when(() -> RemoteMcpHttpSupport.newHttpClient(null)).thenAnswer(invocation -> {
                HttpClient client = mock();
                when(client.sendAsync(any(), ArgumentMatchers.<HttpResponse.BodyHandler<?>>any()))
                        .thenAnswer(call -> receive(call.getArgument(0), call.getArgument(1)));
                return client;
            });
        }

        ManagedRemoteMcpCatalog catalog() {
            return new ManagedRemoteMcpCatalog(Set.of(ENDPOINT), Set.of(), target -> new RemoteServerBinding.Credential(target, "service-secret"), true);
        }

        String config(final ProtocolProfile profile) {
            return JSON.valueToTree(Map.of("aggregation", Map.of("revision", "v1", "servers", List.of(Map.of("name", "orders", "endpoint", ENDPOINT.toString(),
                    "credentialRef", "service/orders", "credentialVersion", "v1", "protocolProfile", profile.id()))))).toString();
        }

        List<String> methods() {
            return requests.stream().map(Wire::method).toList();
        }

        private <T> CompletableFuture<HttpResponse<T>> receive(final HttpRequest request, final HttpResponse.BodyHandler<T> handler) throws IOException {
            assertEquals(ENDPOINT, request.uri());
            JsonNode body = JSON.createObjectNode();
            if ("POST".equals(request.method())) {
                var reader = HttpResponse.BodySubscribers.ofString(StandardCharsets.UTF_8);
                JdkFlowAdapter.flowPublisherToFlux(request.bodyPublisher().orElseThrow()).map(List::of)
                        .as(JdkFlowAdapter::publisherToFlowPublisher).subscribe(reader);
                body = JSON.readTree(reader.getBody().toCompletableFuture().join());
            }
            Wire wire = new Wire(request, body);
            requests.add(wire);
            String method = wire.method();
            int status = 200;
            byte[] bytes = new byte[0];
            if ("DELETE".equals(method)) {
                status = 204;
            } else if ("notifications/initialized".equals(method)) {
                status = 202;
            } else {
                ObjectNode envelope = JSON.createObjectNode().put("jsonrpc", "2.0");
                envelope.set("id", body.get("id"));
                envelope.set("result", result(wire));
                bytes = JSON.writeValueAsBytes(envelope);
            }
            var subscriber = handler.apply(mock());
            Flux.just(List.of(ByteBuffer.wrap(bytes))).as(JdkFlowAdapter::publisherToFlowPublisher).subscribe(subscriber);
            final HttpResponse<T> response = mock();
            when(response.statusCode()).thenReturn(status);
            when(response.body()).thenReturn(subscriber.getBody().toCompletableFuture().join());
            var headers = "initialize".equals(method) ? Map.of("Content-Type", List.of("application/json"), "MCP-Session-Id", List.of("legacy-session"))
                    : Map.of("Content-Type", List.of("application/json"));
            when(response.headers()).thenReturn(java.net.http.HttpHeaders.of(headers, (key, value) -> true));
            if ("tools/call".equals(method) && holdNextCall) {
                holdNextCall = false;
                return releaseCall.thenApply(ignored -> response);
            }
            return CompletableFuture.completedFuture(response);
        }

        private ObjectNode result(final Wire wire) {
            String version = wire.header("MCP-Protocol-Version");
            ObjectNode result = JSON.createObjectNode();
            boolean nativeProfile = "2026-07-28".equals(version);
            if (nativeProfile) {
                assertEquals(version, wire.body().path("params").path("_meta").path("io.modelcontextprotocol/protocolVersion").textValue());
                assertEquals(wire.method(), wire.header("Mcp-Method"));
                result.put("resultType", "complete");
            }
            switch (wire.method()) {
                case "server/discover", "initialize" -> {
                    ObjectNode capabilities = result.putObject("capabilities");
                    capabilities.putObject("tools");
                    capabilities.putObject("resources");
                    result.putObject("serverInfo").put("name", "unit").put("version", "1");
                    if (nativeProfile) {
                        result.putArray("supportedVersions").add(version);
                    } else {
                        result.put("protocolVersion", version);
                    }
                }
                case "tools/list" -> result.putArray("tools").addObject().put("name", "lookup").put("description", "Look up a value")
                        .putObject("inputSchema").put("type", "object").putObject("properties").putObject("key").put("type", "string");
                case "resources/list" -> result.putArray("resources").addObject().put("name", "status").put("uri", "test://status");
                case "resources/templates/list" -> result.putArray("resourceTemplates");
                case "tools/call" -> {
                    assertEquals("lookup", wire.body().at("/params/name").textValue());
                    assertEquals("private-argument", wire.body().at("/params/arguments/key").textValue());
                    result.putArray("content").addObject().put("type", "text").put("text", version);
                }
                default -> throw new AssertionError("Unexpected method: " + wire.method());
            }
            return result;
        }

        @Override
        public void close() {
            clients.close();
        }
    }
}
