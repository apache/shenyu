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
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.net.InetSocketAddress;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.time.Instant;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BooleanSupplier;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ManagedRemoteMcpCatalogTest {

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
}
