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
import com.sun.net.httpserver.HttpsConfigurator;
import com.sun.net.httpserver.HttpsServer;
import com.google.gson.JsonParser;
import io.modelcontextprotocol.common.McpTransportContext;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.metrics.AgentRuntimeMetrics;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpRemoteCatalog;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpResourceAdmission;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.junit.jupiter.params.provider.MethodSource;

import java.net.InetSocketAddress;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.KeyStore;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BooleanSupplier;
import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;
import javax.net.ssl.TrustManagerFactory;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ManagedRemoteMcpCatalogTest {

    @TempDir
    private static Path certificates;

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
        ManagedRemoteMcpCatalog catalog = ManagedRemoteMcpCatalog.forRoutes(Set.of(), Set.of(), reference -> {
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
        ManagedRemoteMcpCatalog catalog = ManagedRemoteMcpCatalog.forRoutes(Set.of("https://192.0.2.10:443/mcp="), Set.of(), target -> {
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
            await(() -> catalog.diagnosticsState().lifecycle().currentVersion() == 0L);
            assertEquals("", catalog.diagnosticsState().lastFailure());
            assertEquals(0, catalog.diagnosticsState().lifecycle().clients());
            catalog.accept(event(true, "[]"));
            await(() -> "IllegalArgumentException".equals(catalog.diagnosticsState().lastFailure()));
        } finally {
            release.countDown();
            catalog.closeAsync().block(Duration.ofSeconds(5));
        }
    }

    @Test
    void validConfigurationReplacesAdmissionLimitsAfterCredentialResolution() throws InterruptedException {
        AgentMcpResourceAdmission.ResourceAdmissionController admission = AgentMcpResourceAdmission.create(
                new org.apache.shenyu.common.dto.AgentGatewayAdmissionConfig(128, 64, 32, 16, 8, 16));
        ManagedRemoteMcpCatalog catalog = new ManagedRemoteMcpCatalog(Set.of(URI.create("https://192.0.2.10:443/mcp")), Set.of(), target ->
                new RemoteServerBinding.Credential(target, "fixed-test-token"), admission, false);
        try {
            assertEquals(128, catalog.admissionConfig().globalInFlight());
            catalog.accept(event(true, configWithAdmission(16, 8, 4)));
            admission.replace(AgentMcpResourceAdmission.create(
                    new org.apache.shenyu.common.dto.AgentGatewayAdmissionConfig(16, 32, 16, 8, 8, 4)));
            await(() -> catalog.diagnosticsState().lifecycle().currentVersion() > 1L);
            assertEquals(16, catalog.admissionConfig().globalInFlight());
            assertEquals(8, catalog.admissionConfig().remoteInFlight());
            assertEquals(4, catalog.admissionConfig().remoteQueued());
            assertEquals(16, admission.snapshot().stream()
                    .filter(state -> state.scope() == org.apache.shenyu.common.metrics.AgentAdmissionMetrics.Scope.GLOBAL)
                    .findFirst().orElseThrow().inFlightLimit());
        } finally {
            catalog.close();
        }
    }

    @Test
    void credentialRotationPinsNewRequestsAndRetiresTheOldLeaseWithoutLeakingSecrets() throws Exception {
        CountDownLatch firstCallEntered = new CountDownLatch(1);
        CountDownLatch releaseFirstCall = new CountDownLatch(1);
        List<String> authorizations = new java.util.concurrent.CopyOnWriteArrayList<>();
        SSLContext context = tlsContext("rotation", "SAN=DNS:mcp.invalid");
        var server = rotationServer(context, firstCallEntered, releaseFirstCall, authorizations);
        URI endpoint = URI.create("https://mcp.invalid:" + server.getAddress().getPort() + "/mcp");
        ManagedRemoteMcpCatalog catalog = ManagedRemoteMcpCatalog.forRoutesWithVerificationTlsContext(
                Set.of(endpoint + "=127.0.0.1"), Set.<String>of(),
                target -> new RemoteServerBinding.Credential(target, "v2".equals(target.credentialVersion()) ? "v2-secret" : "v1-secret"), context);
        try {
            catalog.reload(rotationConfig("v1").replace("https://192.0.2.10:443/mcp", endpoint.toString())).block(Duration.ofSeconds(5));
            assertEquals(2L, catalog.diagnosticsState().lifecycle().currentVersion());
            final var first = catalog.withSnapshot(snapshot -> snapshot.invoke("orders.lookup",
                    JsonParser.parseString("{\"token\":\"first\"}").getAsJsonObject(),
                    new org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpExecutionContext("first-request", "subject", "rule", 1,
                            Set.of("orders.lookup"), Set.of("orders.lookup"), Instant.now().plusSeconds(10)))).toFuture();
            assertTrue(firstCallEntered.await(5, TimeUnit.SECONDS));

            catalog.accept(event(true, rotationConfig("v2").replace("https://192.0.2.10:443/mcp", endpoint.toString())));
            await(() -> catalog.diagnosticsState().lifecycle().currentVersion() > 2L);
            var second = catalog.withSnapshot(snapshot -> snapshot.invoke("orders.lookup",
                    JsonParser.parseString("{\"token\":\"second\"}").getAsJsonObject(),
                    new org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpExecutionContext("second-request", "subject", "rule", 2,
                            Set.of("orders.lookup"), Set.of("orders.lookup"), Instant.now().plusSeconds(10)))).block(Duration.ofSeconds(5));
            assertEquals("second", second.path("structuredContent").path("token").asText());
            assertEquals("Bearer v2-secret", authorizations.stream().filter(value -> value.endsWith("v2-secret")).findFirst().orElseThrow());

            releaseFirstCall.countDown();
            assertEquals("first", first.get(5, TimeUnit.SECONDS).path("structuredContent").path("token").asText());
            await(() -> catalog.diagnosticsState().lifecycle().currentVersion() == 3L);
            assertTrue(authorizations.stream().anyMatch(value -> "Bearer v1-secret".equals(value)));

            catalog.accept(event(false, "{}"));
            await(() -> catalog.diagnosticsState().lifecycle().clients() == 0L);
            StepVerifier.create(catalog.withSnapshot(snapshot -> Mono.just(new ObjectMapper().createObjectNode())))
                    .expectError(AgentMcpRemoteCatalog.CatalogUnavailableException.class).verify(Duration.ofSeconds(5));
            String diagnostics = String.valueOf(catalog.diagnostics());
            assertTrue(!diagnostics.contains("v1-secret") && !diagnostics.contains("v2-secret"));
        } finally {
            releaseFirstCall.countDown();
            catalog.closeAsync().block(Duration.ofSeconds(5));
            server.stop(0);
        }
    }

    private static HttpServer rotationServer(final SSLContext context, final CountDownLatch firstCallEntered,
                                              final CountDownLatch releaseFirstCall, final List<String> authorizations) throws Exception {
        HttpsServer server = HttpsServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        server.setHttpsConfigurator(new HttpsConfigurator(context));
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
                String method = request.path("method").asText();
                if ("notifications/initialized".equals(method)) {
                    exchange.sendResponseHeaders(202, -1);
                    return;
                }
                if ("initialize".equals(method) || "tools/call".equals(method)) {
                    authorizations.add(exchange.getRequestHeaders().getFirst("Authorization"));
                    if (request.path("params").path("arguments").path("token").asText().equals("first")) {
                        firstCallEntered.countDown();
                        try {
                            assertTrue(releaseFirstCall.await(10, TimeUnit.SECONDS));
                        } catch (InterruptedException error) {
                            Thread.currentThread().interrupt();
                        }
                    }
                }
                var result = json.createObjectNode();
                if ("initialize".equals(method)) {
                    result.put("protocolVersion", "2025-06-18");
                    result.putObject("capabilities").putObject("tools");
                    result.putObject("serverInfo").put("name", "fixture").put("version", "1");
                } else if ("tools/list".equals(method)) {
                    var tool = result.putArray("tools").addObject();
                    tool.put("name", "lookup").put("description", "Lookup");
                    tool.putObject("inputSchema").put("type", "object");
                } else {
                    result.putObject("structuredContent").put("token",
                            request.path("params").path("arguments").path("token").asText());
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
        server.setExecutor(java.util.concurrent.Executors.newFixedThreadPool(4));
        server.start();
        return server;
    }

    private static SSLContext tlsContext(final String name, final String extension) throws Exception {
        Path file = certificates.resolve(name + ".p12");
        String keytool = Path.of(System.getProperty("java.home"), "bin",
                System.getProperty("os.name").startsWith("Windows") ? "keytool.exe" : "keytool").toString();
        Process process = new ProcessBuilder(keytool, "-genkeypair", "-alias", "fixture", "-keyalg", "RSA",
                "-keysize", "2048", "-validity", "2", "-dname", "CN=fixture", "-ext", extension,
                "-storetype", "PKCS12", "-keystore", file.toString(), "-storepass", "changeit", "-noprompt")
                .redirectErrorStream(true).redirectOutput(certificates.resolve(name + "-keytool.log").toFile()).start();
        assertTrue(process.waitFor(45, TimeUnit.SECONDS));
        assertEquals(0, process.exitValue());
        KeyStore store = KeyStore.getInstance("PKCS12");
        try (var input = Files.newInputStream(file)) {
            store.load(input, "changeit".toCharArray());
        }
        KeyManagerFactory keys = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm());
        keys.init(store, "changeit".toCharArray());
        TrustManagerFactory trust = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm());
        trust.init(store);
        SSLContext context = SSLContext.getInstance("TLS");
        context.init(keys.getKeyManagers(), trust.getTrustManagers(), null);
        return context;
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
        ManagedRemoteMcpCatalog catalog = ManagedRemoteMcpCatalog.forRoutes(Set.of(endpoint + "="), Set.of(), target -> {
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
            final long withdrawnVersion = emptyConfiguration ? 2L : 0L;
            await(() -> catalog.diagnosticsState().lifecycle().currentVersion() == withdrawnVersion);
            assertEquals(0, handshakes.get(), "A superseded enable event must not allocate or initialize a remote client");
            assertEquals(0, catalog.diagnosticsState().lifecycle().clients());
            if (emptyConfiguration) {
                catalog.accept(event(false, "{}"));
            }
            await(() -> catalog.diagnosticsState().lifecycle().currentVersion() == 0L
                    && catalog.diagnosticsState().lifecycle().clients() == 0
                    && catalog.diagnosticsState().lifecycle().cleanupFailures().isEmpty());
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
        final ManagedRemoteMcpCatalog catalog = ManagedRemoteMcpCatalog.forRoutes(Set.of(endpoint + "="), Set.of(), target -> {
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
            await(() -> catalog.diagnosticsState().lifecycle().generations() == 0);
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
        final ManagedRemoteMcpCatalog catalog = ManagedRemoteMcpCatalog.forRoutes(Set.of("https://192.0.2.10:443/mcp="), Set.of(), target -> {
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

    private static void warmFixture(final URI endpoint) throws Exception {
        // Warm fixture transport and protocol classes before measuring control-update behavior.
        var ready = HttpRequest.newBuilder(endpoint.resolve("/ready")).timeout(Duration.ofSeconds(30)).GET().build();
        assertEquals(204, HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(30)).build()
                .send(ready, HttpResponse.BodyHandlers.discarding()).statusCode());
        var reference = new RemoteServerBinding.Config("orders", endpoint, "service/orders", "v1");
        var warmup = RemoteServerBinding.resolve(reference, RemoteTransportRouteSet.parse(Set.of(endpoint + "=")),
                target -> new RemoteServerBinding.Credential(target, "fixed-test-token")).newClient();
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

    private static String rotationConfig(final String version) {
        return "{\"aggregation\":{\"revision\":\"" + version + "\",\"servers\":[{\"name\":\"orders\","
                + "\"endpoint\":\"https://192.0.2.10:443/mcp\",\"credentialRef\":\"service/orders\","
                + "\"credentialVersion\":\"" + version + "\"}]}}";
    }

    private static String configWithAdmission(final int global, final int remoteInFlight, final int remoteQueued) {
        return "{\"aggregation\":{\"revision\":\"admission\",\"servers\":[]},\"admission\":{"
                + "\"globalInFlight\":" + global + ",\"globalQueued\":32,\"localInFlight\":16,"
                + "\"localQueued\":8,\"remoteInFlight\":" + remoteInFlight + ",\"remoteQueued\":" + remoteQueued + "}}";
    }

    private static void await(final BooleanSupplier condition) throws InterruptedException {
        long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(5);
        while (!condition.getAsBoolean() && System.nanoTime() < deadline) {
            Thread.sleep(5);
        }
        assertTrue(condition.getAsBoolean());
    }
}
