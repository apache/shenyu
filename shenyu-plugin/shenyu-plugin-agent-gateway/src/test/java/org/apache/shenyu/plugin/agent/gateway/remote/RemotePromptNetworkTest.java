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
import com.google.gson.JsonParser;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;
import org.apache.shenyu.common.dto.AgentGatewayAdmissionConfig;
import org.apache.shenyu.common.dto.AgentGatewayMcpConfig;
import org.apache.shenyu.plugin.agent.gateway.AgentTrafficContext;
import org.apache.shenyu.plugin.agent.gateway.prompt.AgentPromptDefinition;
import org.apache.shenyu.plugin.agent.gateway.prompt.AgentPromptInvocation;
import org.apache.shenyu.plugin.agent.gateway.prompt.AgentPromptProvider;
import org.apache.shenyu.plugin.agent.gateway.prompt.AgentPromptRegistry;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpDispatcher;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpHttpHandler;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpRemoteCatalog.CatalogUnavailableException;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpRequestParser;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpResourceAdmission;
import org.apache.shenyu.plugin.agent.gateway.resource.AgentResourceRegistry;
import org.apache.shenyu.plugin.agent.gateway.security.AgentMcpIdentity;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolRegistry;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.springframework.http.server.reactive.ReactorHttpHandlerAdapter;
import org.springframework.web.server.adapter.WebHttpHandlerBuilder;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;
import reactor.netty.DisposableServer;
import reactor.test.scheduler.VirtualTimeScheduler;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.LongSupplier;

import static org.awaitility.Awaitility.await;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/** Real sockets on both sides of the gateway, using the documented stateless MCP contract. */
class RemotePromptNetworkTest {

    private static final Duration WAIT = Duration.ofSeconds(10);

    private static final HttpClient CLIENT = HttpClient.newBuilder().version(HttpClient.Version.HTTP_1_1).connectTimeout(WAIT).build();

    private final ObjectMapper json = new ObjectMapper();

    private final List<AgentMcpResourceAdmission.ResourceAdmissionController> admissions = new ArrayList<>();

    @AfterEach
    void closeApplicationAdmissions() {
        admissions.forEach(AgentMcpResourceAdmission.ResourceAdmissionController::close);
    }

    private AgentMcpResourceAdmission.ResourceAdmissionController admission() {
        AgentMcpResourceAdmission.ResourceAdmissionController controller = AgentMcpResourceAdmission.create(new AgentGatewayAdmissionConfig(128, 64, 32, 16, 8, 16));
        admissions.add(controller);
        return controller;
    }

    @ParameterizedTest
    @CsvSource({"false,json", "true,json", "false,sse", "true,sse"})
    void actualClientListsAndGetsLocalAndPagedRemotePrompts(final boolean upstreamSse, final String mode) throws Exception {
        try (Fixture first = new Fixture("first", upstreamSse); Fixture second = new Fixture("second", upstreamSse);
                ManagedRemoteMcpCatalog catalog = catalog(first, second, true)) {
            catalog.reload(config(first, second)).block(WAIT);
            AtomicReference<Set<String>> grants = new AtomicReference<>(Set.of("local", "first.summary", "first.next", "second.summary"));
            DisposableServer gateway = gateway(catalog, mode, grants);
            try {
                ObjectNode listed = payload(send(gateway, "prompts/list", "", Map.of(), "owner"));
                assertEquals(4, listed.path("result").path("prompts").size());
                for (String name : List.of("local", "first.summary", "first.next", "second.summary")) {
                    ObjectNode fetched = payload(send(gateway, "prompts/get", name, Map.of("topic", "gateway"), "owner"));
                    String text = fetched.path("result").path("messages").get(0).path("content").path("text").asText();
                    assertEquals(name + ":gateway", text);
                }
                assertEquals(0, payload(send(gateway, "tools/list", "", Map.of(), "owner")).path("result").path("tools").size());
                assertTrue(payload(send(gateway, "server/discover", "", Map.of(), "owner")).path("result").path("capabilities").has("prompts"));
                grants.set(Set.of("local"));
                assertEquals(1, payload(send(gateway, "prompts/list", "", Map.of(), "owner")).path("result").path("prompts").size());
                assertEquals(403, send(gateway, "prompts/get", "first.summary", Map.of("topic", "gateway"), "owner").statusCode());
                assertEquals(401, send(gateway, "prompts/get", "local", Map.of("topic", "gateway"), "untrusted").statusCode());
                for (Fixture fixture : List.of(first, second)) {
                    assertTrue(fixture.wires.stream().allMatch(wire -> wire.authorization().equals("Bearer prompts-service")
                            && wire.subject().isEmpty() && wire.cookie().isEmpty()), fixture.wires.toString());
                    assertTrue(fixture.wires.stream().anyMatch(wire -> "prompts/get".equals(wire.method()) && "summary".equals(wire.name())));
                    assertFalse(fixture.wires.stream().anyMatch(wire -> wire.name().startsWith(fixture.name + ".")));
                }
            } finally {
                gateway.disposeNow(WAIT);
            }
        }
    }

    @Test
    void promptDiscoveryIsOptInAndDoesNotAffectExistingTools() throws Exception {
        try (Fixture first = new Fixture("first", false); Fixture second = new Fixture("second", false);
                ManagedRemoteMcpCatalog catalog = catalog(first, second, false)) {
            catalog.reload(config(first, second)).block(WAIT);
            catalog.withSnapshot(snapshot -> {
                assertTrue(snapshot.promptDefinitions().isEmpty());
                assertTrue(snapshot.definitions().isEmpty());
                return Mono.just(json.createObjectNode());
            }).block(WAIT);
            assertFalse(first.wires.stream().anyMatch(wire -> wire.method().startsWith("prompts/")));
        }
    }

    @Test
    void refreshedAndRemovedDefinitionsAreNotReintroducedByOldGenerations() throws Exception {
        try (Fixture first = new Fixture("first", false); Fixture second = new Fixture("second", false);
                ManagedRemoteMcpCatalog catalog = catalog(first, second, true)) {
            catalog.reload(config(first, second)).block(WAIT);
            first.prompt.set("replacement");
            catalog.reload(config(first, second)).block(WAIT);
            catalog.withSnapshot(snapshot -> {
                assertFalse(snapshot.promptDefinitions().containsKey("first.summary"));
                assertTrue(snapshot.promptDefinitions().containsKey("first.replacement"));
                return Mono.just(json.createObjectNode());
            }).block(WAIT);
            catalog.reload("{}").block(WAIT);
            catalog.withSnapshot(snapshot -> {
                assertTrue(snapshot.promptDefinitions().isEmpty());
                return Mono.just(json.createObjectNode());
            }).block(WAIT);
        }
    }

    @Test
    void missingPromptCapabilityNeverTriggersAnUnsupportedDiscovery() throws Exception {
        try (Fixture first = new Fixture("first", false); Fixture second = new Fixture("second", false);
                ManagedRemoteMcpCatalog catalog = catalog(first, second, true)) {
            first.promptsSupported.set(false);
            second.promptsSupported.set(false);
            catalog.reload(config(first, second)).block(WAIT);
            catalog.withSnapshot(snapshot -> {
                assertTrue(snapshot.promptDefinitions().isEmpty());
                return Mono.just(json.createObjectNode());
            }).block(WAIT);
            assertFalse(first.wires.stream().anyMatch(wire -> wire.method().startsWith("prompts/")));
            assertFalse(second.wires.stream().anyMatch(wire -> wire.method().startsWith("prompts/")));
        }
    }

    @Test
    void cancellingOneActualHttpGetLeavesThePeerAndSessionUsable() throws Exception {
        try (Fixture fixture = new Fixture("first", false)) {
            RequestScopedMcpClient client = RemoteServerBinding.resolve(
                    new RemoteServerBinding.Config("first", fixture.endpoint(), "service", "v1"),
                    Set.of(fixture.endpoint()), reference -> new RemoteServerBinding.Credential(reference, "prompts-service")).newClient();
            try {
                client.initialize().block(WAIT);
                fixture.holdNext.set(true);
                var cancelled = client.getPrompt("summary", Map.of("topic", "cancelled")).toFuture();
                assertTrue(fixture.started.await(10, TimeUnit.SECONDS));
                assertEquals("first.summary:peer", client.getPrompt("summary", Map.of("topic", "peer")).block(WAIT)
                        .path("messages").get(0).path("content").path("text").asText());
                assertTrue(cancelled.cancel(true));
                assertEquals(0, client.pending());
                fixture.release.countDown();
                assertEquals("first.summary:after", client.getPrompt("summary", Map.of("topic", "after")).block(WAIT)
                        .path("messages").get(0).path("content").path("text").asText());
            } finally {
                fixture.release.countDown();
                client.closeGracefully().block(WAIT);
            }
        }
    }

    @Test
    void clientDisconnectReleasesGatewayLeaseAndRemoteAdmissionWithoutCancellingPeer() throws Exception {
        try (Fixture first = new Fixture("first", false); Fixture second = new Fixture("second", false);
                ManagedRemoteMcpCatalog catalog = catalog(first, second, true)) {
            catalog.reload(config(first, second)).block(WAIT);
            DisposableServer gateway = gateway(catalog, "json", new AtomicReference<>(Set.of("first.summary")));
            try {
                first.holdNext.set(true);
                final var cancelled = CLIENT.sendAsync(request(gateway, "prompts/get", "first.summary", Map.of("topic", "cancelled"), "owner"),
                        HttpResponse.BodyHandlers.ofString());
                assertTrue(first.started.await(10, TimeUnit.SECONDS));
                assertEquals("first.summary:peer", payload(send(gateway, "prompts/get", "first.summary", Map.of("topic", "peer"), "owner"))
                        .path("result").path("messages").get(0).path("content").path("text").asText());
                catalog.reload("{}").block(WAIT);
                catalog.withSnapshot(snapshot -> {
                    assertTrue(snapshot.promptDefinitions().isEmpty());
                    return Mono.just(json.createObjectNode());
                }).block(WAIT);
                assertTrue(cancelled.cancel(true));
                AgentMcpResourceAdmission.ResourceAdmissionController limits = (AgentMcpResourceAdmission.ResourceAdmissionController) catalog.admission();
                await().atMost(WAIT).until(() -> catalog.diagnosticsState().lifecycle().references() == 0
                        && catalog.diagnosticsState().lifecycle().pending() == 0
                        && limits.snapshot().stream().allMatch(state -> state.inFlight() == 0 && state.queued() == 0));
            } finally {
                first.release.countDown();
                gateway.disposeNow(WAIT);
            }
        }
    }

    @Test
    void pollingPublishesChangedAndRemovedPromptsWithoutReplayingGets() throws Exception {
        VirtualTimeScheduler timer = VirtualTimeScheduler.create();
        try (Fixture first = new Fixture("first", false); Fixture second = new Fixture("second", false);
                ManagedRemoteMcpCatalog catalog = pollingCatalog(first, second, timer, () -> timer.now(TimeUnit.NANOSECONDS))) {
            catalog.reload(config(first, second)).block(WAIT);
            assertEquals(Set.of("first.summary", "first.next", "second.summary"), promptNames(catalog));
            first.prompt.set("replacement");
            timer.advanceTimeBy(Duration.ofSeconds(60));
            await().atMost(WAIT).until(() -> catalog.refreshTiming().nextRefreshAt() == 120000L);
            assertEquals(Set.of("first.replacement", "first.next", "second.summary"), promptNames(catalog));
            first.promptsSupported.set(false);
            timer.advanceTimeBy(Duration.ofSeconds(60));
            await().atMost(WAIT).until(() -> catalog.refreshTiming().nextRefreshAt() == 180000L);
            assertEquals(Set.of("second.summary"), promptNames(catalog));
            assertFalse(first.wires.stream().anyMatch(wire -> "prompts/get".equals(wire.method())));
            assertEquals(0, catalog.diagnosticsState().lifecycle().references());
        }
    }

    @Test
    void discoveryOutageWithdrawsPromptsAndRecoversOnTheExistingBackoff() throws Exception {
        VirtualTimeScheduler timer = VirtualTimeScheduler.create();
        try (Fixture first = new Fixture("first", false); Fixture second = new Fixture("second", false);
                ManagedRemoteMcpCatalog catalog = pollingCatalog(first, second, timer, () -> timer.now(TimeUnit.NANOSECONDS))) {
            catalog.reload(config(first, second)).block(WAIT);
            AtomicReference<Set<String>> grants = new AtomicReference<>(Set.of("first.summary"));
            DisposableServer gateway = gateway(catalog, "json", grants);
            try {
                assertEquals(200, send(gateway, "prompts/get", "first.summary", Map.of("topic", "before"), "owner").statusCode());
                first.status.set(503);
                timer.advanceTimeBy(Duration.ofSeconds(60));
                await().atMost(WAIT).until(() -> catalog.refreshTiming().nextRefreshAt() == 61000L);
                assertThrows(CatalogUnavailableException.class, () -> promptNames(catalog));
                assertEquals(503, send(gateway, "prompts/get", "first.summary", Map.of("topic", "outage"), "owner").statusCode());
                grants.set(Set.of());
                first.status.set(200);
                timer.advanceTimeBy(Duration.ofSeconds(1));
                await().atMost(WAIT).until(() -> catalog.refreshTiming().nextRefreshAt() == 121000L);
                assertEquals(1, catalog.diagnosticsState().refresh().recoveries());
                assertEquals(0, payload(send(gateway, "prompts/list", "", Map.of(), "owner")).path("result").path("prompts").size());
                assertEquals(403, send(gateway, "prompts/get", "first.summary", Map.of("topic", "revoked"), "owner").statusCode());
                grants.set(Set.of("first.summary"));
                assertEquals(200, send(gateway, "prompts/get", "first.summary", Map.of("topic", "after"), "owner").statusCode());
                assertEquals(2, first.wires.stream().filter(wire -> "prompts/get".equals(wire.method())).count());
            } finally {
                gateway.disposeNow(WAIT);
            }
        }
    }

    @Test
    void expiredPromptSnapshotIsRejectedEvenWhenRefreshTimerHasNotRun() throws Exception {
        VirtualTimeScheduler timer = VirtualTimeScheduler.create();
        AtomicLong clock = new AtomicLong();
        try (Fixture first = new Fixture("first", false); Fixture second = new Fixture("second", false);
                ManagedRemoteMcpCatalog catalog = pollingCatalog(first, second, timer, clock::get)) {
            catalog.reload(config(first, second)).block(WAIT);
            assertEquals(3, promptNames(catalog).size());
            clock.set(Duration.ofSeconds(60).toNanos());
            assertThrows(CatalogUnavailableException.class, () -> promptNames(catalog));
            assertEquals(1, catalog.diagnosticsState().refresh().attempts());
            assertEquals(60000L, catalog.refreshTiming().nextRefreshAt());
            timer.advanceTimeBy(Duration.ofSeconds(60));
            await().atMost(WAIT).until(() -> catalog.refreshTiming().nextRefreshAt() == 120000L);
            assertEquals(3, promptNames(catalog).size());
        }
    }

    @Test
    void removalFencesLatePromptDiscoveryAndCancelsFuturePolling() throws Exception {
        VirtualTimeScheduler timer = VirtualTimeScheduler.create();
        try (Fixture first = new Fixture("first", false); Fixture second = new Fixture("second", false);
                ManagedRemoteMcpCatalog catalog = pollingCatalog(first, second, timer, () -> timer.now(TimeUnit.NANOSECONDS))) {
            catalog.reload(config(first, second)).block(WAIT);
            first.holdDiscovery.set(true);
            timer.advanceTimeBy(Duration.ofSeconds(60));
            assertTrue(first.started.await(10, TimeUnit.SECONDS));
            org.apache.shenyu.common.dto.PluginData disabled = new org.apache.shenyu.common.dto.PluginData();
            disabled.setEnabled(false);
            disabled.setConfig("{}");
            catalog.accept(disabled);
            first.release.countDown();
            await().atMost(WAIT).untilAsserted(() -> assertTrue(promptNames(catalog).isEmpty()));
            assertEquals(0L, catalog.refreshTiming().nextRefreshAt());
            final long discoveries = first.wires.stream().filter(wire -> "prompts/list".equals(wire.method())).count();
            timer.advanceTimeBy(Duration.ofHours(1));
            assertTrue(promptNames(catalog).isEmpty());
            assertEquals(discoveries, first.wires.stream().filter(wire -> "prompts/list".equals(wire.method())).count());
            await().atMost(WAIT).until(() -> catalog.diagnosticsState().lifecycle().clients() == 0);
        }
    }

    private Set<String> promptNames(final ManagedRemoteMcpCatalog catalog) {
        return catalog.withSnapshot(snapshot -> Mono.just(json.valueToTree(snapshot.promptDefinitions())))
                .block(WAIT).properties().stream().map(Map.Entry::getKey).collect(java.util.stream.Collectors.toSet());
    }

    private ManagedRemoteMcpCatalog pollingCatalog(final Fixture first, final Fixture second, final VirtualTimeScheduler timer, final LongSupplier clock) {
        return new ManagedRemoteMcpCatalog(Set.of(first.endpoint(), second.endpoint()), Set.of(),
                reference -> new RemoteServerBinding.Credential(reference, "prompts-service"), false, true,
                Schedulers.newBoundedElastic(1, 16, "agent-mcp-prompt-test"), timer, clock,
                admission());
    }

    private ManagedRemoteMcpCatalog catalog(final Fixture first, final Fixture second, final boolean prompts) {
        return new ManagedRemoteMcpCatalog(Set.of(first.endpoint(), second.endpoint()), Set.of(),
                reference -> new RemoteServerBinding.Credential(reference, "prompts-service"),
                admission(), false, prompts);
    }

    private String config(final Fixture first, final Fixture second) {
        return json.valueToTree(Map.of("aggregation", Map.of("revision", "prompts-v1", "servers", List.of(
                Map.of("name", first.name, "endpoint", first.endpoint().toString(), "credentialRef", "service", "credentialVersion", "v1"),
                Map.of("name", second.name, "endpoint", second.endpoint().toString(), "credentialRef", "service", "credentialVersion", "v1"))))).toString();
    }

    private DisposableServer gateway(final ManagedRemoteMcpCatalog catalog, final String mode, final AtomicReference<Set<String>> grants) {
        AgentPromptProvider provider = new AgentPromptProvider() {
            @Override
            public AgentPromptDefinition definition() {
                return new AgentPromptDefinition(metadata("local"));
            }

            @Override
            public Mono<ObjectNode> get(final AgentPromptInvocation invocation) {
                assertEquals("tenant", invocation.tenant());
                return Mono.just(result("local:" + invocation.arguments().get("topic")));
            }
        };
        AgentMcpDispatcher dispatcher = new AgentMcpDispatcher(new AgentToolRegistry(List.of()), "gateway", "1", catalog,
                new AgentResourceRegistry(List.of()), catalog.admission(), new AgentPromptRegistry(List.of(provider)));
        AgentMcpHttpHandler handler = new AgentMcpHttpHandler(dispatcher, exchange -> "Bearer owner".equals(exchange.getRequest().getHeaders().getFirst("Authorization"))
                ? Mono.just(new AgentMcpIdentity("tenant", "owner", Set.of(), Set.of(), Set.of()).withPromptPermissions(grants.get(), Set.of())) : Mono.empty());
        AgentGatewayMcpConfig rules = AgentGatewayMcpConfig.parse(JsonParser.parseString("{\"allowedPrompts\":[\"local\",\"first.summary\",\"first.next\",\"second.summary\"],"
                + "\"responseMode\":\"" + mode + "\"}"), true);
        ReactorHttpHandlerAdapter adapter = new ReactorHttpHandlerAdapter(WebHttpHandlerBuilder.webHandler(exchange ->
                handler.handle(exchange, rules, new AgentTrafficContext(java.util.UUID.randomUUID().toString(), "mcp", "selector", "rule"), 1)).build());
        return reactor.netty.http.server.HttpServer.create().host("127.0.0.1").port(0).handle(adapter).bindNow(WAIT);
    }

    private HttpResponse<String> send(final DisposableServer gateway, final String method, final String name, final Map<String, String> arguments, final String credential)
            throws IOException, InterruptedException {
        return CLIENT.send(request(gateway, method, name, arguments, credential), HttpResponse.BodyHandlers.ofString());
    }

    private HttpRequest request(final DisposableServer gateway, final String method, final String name, final Map<String, String> arguments, final String credential) {
        ObjectNode request = json.createObjectNode().put("jsonrpc", "2.0").put("id", "same-id").put("method", method);
        ObjectNode params = request.putObject("params");
        params.putObject("_meta").put("io.modelcontextprotocol/protocolVersion", AgentMcpRequestParser.VERSION)
                .putObject("io.modelcontextprotocol/clientCapabilities");
        HttpRequest.Builder builder = HttpRequest.newBuilder(URI.create("http://127.0.0.1:" + gateway.port() + "/agent"))
                .timeout(WAIT).header("Content-Type", "application/json").header("Accept", "application/json, text/event-stream")
                .header("MCP-Protocol-Version", AgentMcpRequestParser.VERSION).header("Mcp-Method", method)
                .header("Authorization", "Bearer " + credential).header("Cookie", "private-client-cookie");
        if ("prompts/get".equals(method)) {
            params.put("name", name);
            params.set("arguments", json.valueToTree(arguments));
            builder.header("Mcp-Name", name);
        }
        return builder.POST(HttpRequest.BodyPublishers.ofString(request.toString())).build();
    }

    private ObjectNode payload(final HttpResponse<String> response) throws IOException {
        assertEquals(200, response.statusCode(), response.body());
        String wire = response.body();
        if (wire.startsWith("event:") || wire.startsWith("data:")) {
            wire = wire.lines().filter(line -> line.startsWith("data:")).map(line -> line.substring(5).stripLeading()).findFirst().orElseThrow();
        }
        return (ObjectNode) json.readTree(wire);
    }

    private static ObjectNode metadata(final String name) {
        ObjectNode metadata = new ObjectMapper().createObjectNode().put("name", name);
        metadata.putArray("arguments").addObject().put("name", "topic").put("required", true);
        return metadata;
    }

    private static ObjectNode result(final String text) {
        ObjectNode result = new ObjectMapper().createObjectNode();
        result.putArray("messages").addObject().put("role", "user").putObject("content").put("type", "text").put("text", text);
        return result;
    }

    private record Wire(String method, String name, String authorization, String subject, String cookie) {
    }

    private static final class Fixture implements AutoCloseable {

        private final ObjectMapper json = new ObjectMapper();

        private final String name;

        private final boolean sse;

        private final HttpServer server;

        private final ExecutorService executor = Executors.newCachedThreadPool();

        private final List<Wire> wires = new CopyOnWriteArrayList<>();

        private final AtomicReference<String> prompt = new AtomicReference<>("summary");

        private final AtomicBoolean holdNext = new AtomicBoolean();

        private final AtomicBoolean holdDiscovery = new AtomicBoolean();

        private final AtomicInteger status = new AtomicInteger(200);

        private final AtomicBoolean promptsSupported = new AtomicBoolean(true);

        private final CountDownLatch started = new CountDownLatch(1);

        private final CountDownLatch release = new CountDownLatch(1);

        private Fixture(final String name, final boolean sse) throws IOException, InterruptedException {
            this.name = name;
            this.sse = sse;
            server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
            server.createContext("/ready", exchange -> {
                try (exchange) {
                    exchange.sendResponseHeaders(204, -1);
                }
            });
            server.createContext("/mcp", this::handle);
            server.setExecutor(executor);
            server.start();
            // Prepare the real client/server HTTP runtime before timing MCP discovery.
            // This is fixture readiness, not a relaxation of any production deadline.
            try {
                HttpRequest ready = HttpRequest.newBuilder(URI.create("http://127.0.0.1:" + server.getAddress().getPort() + "/ready")).timeout(WAIT).GET().build();
                if (CLIENT.send(ready, HttpResponse.BodyHandlers.discarding()).statusCode() != 204) {
                    throw new IOException("Prompt fixture is not ready");
                }
                // Negotiate the fixture's existing protocol before testing prompt discovery.
                // No prompts are listed or resolved by this bootstrap client.
                Mono.usingWhen(Mono.fromSupplier(() -> RemoteServerBinding.resolve(
                                new RemoteServerBinding.Config(name, endpoint(), "service", "fixture-ready"), Set.of(endpoint()),
                                reference -> new RemoteServerBinding.Credential(reference, "prompts-service")).newClient()),
                        probe -> probe.initialize().then(probe.listTools(null)).then(), RequestScopedMcpClient::closeGracefully).block(WAIT);
            } catch (IOException | InterruptedException | RuntimeException error) {
                server.stop(0);
                executor.shutdownNow();
                throw error;
            }
        }

        private URI endpoint() {
            return URI.create("http://127.0.0.1:" + server.getAddress().getPort() + "/mcp");
        }

        private void handle(final HttpExchange exchange) throws IOException {
            try (exchange) {
                if ("DELETE".equals(exchange.getRequestMethod())) {
                    exchange.sendResponseHeaders(200, -1);
                    return;
                }
                if (status.get() != 200) {
                    exchange.sendResponseHeaders(status.get(), -1);
                    return;
                }
                ObjectNode request = (ObjectNode) json.readTree(exchange.getRequestBody());
                String method = request.path("method").asText();
                wires.add(new Wire(method, request.path("params").path("name").asText(), header(exchange, "Authorization"),
                        header(exchange, "X-Spike-Subject"), header(exchange, "Cookie")));
                if ("notifications/initialized".equals(method)) {
                    exchange.sendResponseHeaders(202, -1);
                    return;
                }
                ObjectNode result = json.createObjectNode();
                if ("initialize".equals(method)) {
                    result.put("protocolVersion", "2025-06-18");
                    ObjectNode capabilities = result.putObject("capabilities");
                    if (promptsSupported.get()) {
                        capabilities.putObject("prompts");
                    }
                    capabilities.putObject("tools");
                    result.putObject("serverInfo").put("name", name).put("version", "1");
                } else if ("tools/list".equals(method)) {
                    result.putArray("tools");
                } else if ("prompts/list".equals(method)) {
                    if (holdDiscovery.compareAndSet(true, false)) {
                        started.countDown();
                        try {
                            assertTrue(release.await(10, TimeUnit.SECONDS));
                        } catch (InterruptedException error) {
                            Thread.currentThread().interrupt();
                            throw new IllegalStateException(error);
                        }
                    }
                    boolean next = request.path("params").has("cursor");
                    result.putArray("prompts").add(metadata(next ? "next" : prompt.get()));
                    if (!next && "first".equals(name)) {
                        result.put("nextCursor", "second");
                    }
                } else if ("prompts/get".equals(method)) {
                    if (holdNext.compareAndSet(true, false)) {
                        started.countDown();
                        try {
                            assertTrue(release.await(10, TimeUnit.SECONDS));
                        } catch (InterruptedException error) {
                            Thread.currentThread().interrupt();
                            throw new IllegalStateException(error);
                        }
                    }
                    result = result(name + "." + request.path("params").path("name").asText() + ":" + request.path("params").path("arguments").path("topic").asText());
                } else {
                    throw new IllegalStateException("Unexpected method: " + method);
                }
                ObjectNode response = json.createObjectNode().put("jsonrpc", "2.0");
                response.set("id", request.get("id"));
                response.set("result", result);
                byte[] bytes = (sse ? "event: message\ndata: " + response + "\n\n" : response.toString()).getBytes(StandardCharsets.UTF_8);
                exchange.getResponseHeaders().set("Content-Type", sse ? "text/event-stream" : "application/json");
                exchange.getResponseHeaders().set("MCP-Session-Id", name + "-session");
                exchange.sendResponseHeaders(200, bytes.length);
                exchange.getResponseBody().write(bytes);
            }
        }

        private String header(final HttpExchange exchange, final String name) {
            String value = exchange.getRequestHeaders().getFirst(name);
            return Objects.isNull(value) ? "" : value;
        }

        @Override
        public void close() {
            release.countDown();
            server.stop(0);
            executor.shutdownNow();
        }
    }
}
