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

package org.apache.shenyu.plugin.agent.gateway;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.plugin.agent.gateway.delegation.AgentHttpIdentityDelegator;
import org.apache.shenyu.plugin.agent.gateway.delegation.FileDelegationSigningKeyResolver;
import org.apache.shenyu.plugin.agent.gateway.delegation.JwtHttpIdentityDelegator;
import org.apache.shenyu.plugin.agent.gateway.handler.AgentGatewayPluginDataHandler;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpDispatcher;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpHttpHandler;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpRequestParser;
import org.apache.shenyu.plugin.agent.gateway.security.AgentMcpIdentity;
import org.apache.shenyu.plugin.agent.gateway.security.AgentMcpPrincipal;
import org.apache.shenyu.plugin.agent.gateway.security.AgentMcpPrincipalSecurityResolver;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolRegistry;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.utils.RequestUrlUtils;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.divide.DividePlugin;
import org.apache.shenyu.plugin.divide.handler.DividePluginDataHandler;
import org.apache.shenyu.plugin.divide.handler.DivideUpstreamDataHandler;
import org.apache.shenyu.plugin.httpclient.WebClientPlugin;
import org.apache.shenyu.web.handler.ShenyuWebHandler;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.boot.test.system.CapturedOutput;
import org.springframework.boot.test.system.OutputCaptureExtension;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.http.server.reactive.HttpHandler;
import org.springframework.http.server.reactive.ReactorHttpHandlerAdapter;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.test.web.reactive.server.HttpHandlerConnector;
import org.springframework.web.reactive.function.client.WebClient;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;
import reactor.netty.DisposableServer;
import reactor.netty.http.server.HttpServer;
import reactor.test.StepVerifier;
import reactor.test.publisher.PublisherProbe;

import java.io.IOException;
import java.math.BigDecimal;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.GeneralSecurityException;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.PublicKey;
import java.security.Signature;
import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.Base64;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BiFunction;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AgentHttpDelegationIntegrationTest {

    private static final String PATH = "/api/orders/lookup";

    private static final String TOOL = "orders.lookup";

    private static final String ALICE = "private-subject-alice";

    private static final String TENANT_A = "private-tenant-a";

    private static final String DOMAIN = "http://orders.test";

    private static final String KEY = """
            {"issuer":"shenyu-orders","credentialRef":"orders","credentialVersion":"v1","tool":"orders.lookup",
            "method":"GET","path":"/api/orders/lookup","audience":"orders-api","allowedScopes":["orders.read"]}
            """;

    private static final String RULE = """
            {"trafficType":"mcp","mcp":{"allowedTools":["orders.lookup"],"httpTools":{"orders.lookup":{
            "description":"Look up an order","inputSchema":{"type":"object","properties":{"id":{"type":"string"}},"required":["id"]},
            "requestConfig":{"requestTemplate":{"method":"GET","url":"/api/orders/lookup"},"argsPosition":{"id":"query"}},
            "identityDelegation":{"audience":"orders-api","scopes":["orders.read"],"credentialRef":"orders","credentialVersion":"v1"}}}}}
            """;

    @TempDir
    private Path directory;

    private Path file;

    private KeyPair keys;

    private OrdersApi backend;

    private SelectedGateway gateway;

    private ShenyuWebHandler handler;

    private Mono<Void> credentialReady = Mono.empty();

    private final SelectorData route = new SelectorData();

    private final RuleData divideRule = new RuleData();

    @BeforeEach
    void setUp() throws GeneralSecurityException, IOException {
        keys = KeyPairGenerator.getInstance("Ed25519").generateKeyPair();
        file = Files.createDirectory(directory.resolve("orders")).resolve("v1.json");
        writeKey(json(KEY));
        backend = new OrdersApi();
        AgentHttpIdentityDelegator signer = new JwtHttpIdentityDelegator(new FileDelegationSigningKeyResolver(directory));
        gateway = new SelectedGateway(request -> credentialReady.then(signer.delegate(request)));
        route.setId(UUID.randomUUID().toString());
        divideRule.setId(UUID.randomUUID().toString());
        divideRule.setSelectorId(route.getId());
        divideRule.setHandle("{\"loadBalance\":\"random\",\"retry\":2,\"timeout\":3000}");
        new DividePluginDataHandler().handlerRule(divideRule);
        handler = handler(DOMAIN, WebClient.builder().clientConnector(new HttpHandlerConnector(backend)).build(), false);
    }

    @AfterEach
    void tearDown() {
        new DividePluginDataHandler().removeRule(divideRule);
        UpstreamCacheManager.getInstance().removeByKey(route.getId());
        gateway.clear();
    }

    @Test
    void realHttpBackendAuthenticatesAndAuthorizesTheOrder() {
        DisposableServer server = HttpServer.create().host("127.0.0.1").port(0)
                .handle(new ReactorHttpHandlerAdapter(backend)).bindNow(Duration.ofSeconds(5));
        try {
            handler = handler("http://127.0.0.1:" + server.port(), WebClient.create(), false);
            assertOrder(call(TENANT_A, ALICE, "A"), "A");
            assertDenied(call(TENANT_A, ALICE, "B"));
            assertEquals(2, backend.received.get());
            assertEquals(1, backend.disclosed.get());
        } finally {
            server.disposeNow();
        }
    }

    @Test
    void grantsAndRecordOwnershipAreIndependentOfWireIdentity() {
        assertOrder(call(TENANT_A, ALICE, "A"), "A");
        assertDenied(call(TENANT_A, ALICE, "B"));
        assertDenied(call("private-tenant-b", ALICE, "A"));
        assertDenied(call(TENANT_A, "private-subject-bob", "A"));
        assertEquals(4, backend.received.get());
        assertEquals(1, backend.disclosed.get());

        JsonObject forged = json("{\"id\":\"B\",\"tenant\":\"private-tenant-b\"}");
        assertDenied(dispatch(TENANT_A, ALICE, forged, Set.of(TOOL)).block());
        assertDenied(dispatch(TENANT_A, ALICE, json("{\"id\":\"A\"}"), Set.of()).block());
        assertEquals(4, backend.received.get());
    }

    @Test
    void overlappingIdentitiesHaveDistinctAssertionsAndPrivateResponses() {
        backend.waitForTwo = true;
        var calls = Mono.zip(dispatch(TENANT_A, ALICE, json("{\"id\":\"A\"}"), Set.of(TOOL)),
                dispatch("private-tenant-b", "private-subject-bob", json("{\"id\":\"B\"}"), Set.of(TOOL)))
                .block(Duration.ofSeconds(8));
        assertOrder(calls.getT1(), "A");
        assertOrder(calls.getT2(), "B");
        assertEquals(2, backend.ids.size());
        assertEquals(2, backend.disclosed.get());
    }

    @ParameterizedTest
    @ValueSource(strings = {"audience", "expired", "issuer", "scope", "untrusted-key"})
    void backendRejectsAssertionsOutsideItsTrustPolicy(final String mismatch) throws IOException, GeneralSecurityException {
        JsonObject key = json(KEY);
        switch (mismatch) {
            case "audience" -> {
                key.addProperty("audience", "inventory-api");
                gateway.update(RULE.replace("orders-api", "inventory-api"));
            }
            case "expired" -> backend.clock = Clock.offset(Clock.systemUTC(), Duration.ofMinutes(1));
            case "untrusted-key" -> keys = KeyPairGenerator.getInstance("Ed25519").generateKeyPair();
            case "issuer" -> key.addProperty("issuer", "other-issuer");
            case "scope" -> {
                key.add("allowedScopes", JsonParser.parseString("[\"inventory.read\"]"));
                gateway.update(RULE.replace("orders.read", "inventory.read"));
            }
            default -> throw new AssertionError();
        }
        writeKey(key);
        assertDenied(call(TENANT_A, ALICE, "A"));
        assertEquals(1, backend.received.get());
        assertEquals(0, backend.disclosed.get());
    }

    @Test
    void ruleChangesRevocationAndRemovalTakeEffectOnNextCall() throws IOException {
        assertOrder(call(TENANT_A, ALICE, "A"), "A");
        gateway.update(RULE.replace(PATH, "/api/orders/another"));
        assertReason(call(TENANT_A, ALICE, "A"), "TARGET_MISMATCH");
        gateway.update(RULE);
        JsonObject key = json(KEY);
        key.addProperty("tool", "other.tool");
        writeKey(key);
        assertReason(call(TENANT_A, ALICE, "A"), "TARGET_MISMATCH");
        key = json(KEY);
        key.addProperty("revoked", true);
        writeKey(key);
        assertReason(call(TENANT_A, ALICE, "A"), "REVOKED_OR_EXPIRED");
        Files.delete(file);
        assertReason(call(TENANT_A, ALICE, "A"), "KEY_UNAVAILABLE");
        assertEquals(1, backend.received.get());
        assertEquals(1, backend.disclosed.get());
    }

    @ParameterizedTest
    @ValueSource(strings = {"update", "remove", "selector", "plugin"})
    void invalidatingAnAdmittedRulePreventsThePendingBusinessCall(final String operation) {
        Sinks.Empty<Void> release = Sinks.empty();
        PublisherProbe<Void> waiting = PublisherProbe.of(release.asMono());
        credentialReady = waiting.mono();
        StepVerifier.create(dispatch(TENANT_A, ALICE, json("{\"id\":\"A\"}"), Set.of(TOOL)))
                .then(() -> {
                    waiting.assertWasSubscribed();
                    switch (operation) {
                        case "update" -> gateway.update(RULE.replace("orders-api", "inventory-api"));
                        case "remove" -> new AgentGatewayPluginDataHandler().removeRule(gateway.rule);
                        case "selector" -> BaseDataCache.getInstance().removeSelectData(gateway.selector);
                        case "plugin" -> BaseDataCache.getInstance().removePluginData(gateway.plugin);
                        default -> throw new AssertionError();
                    }
                    assertEquals(Sinks.EmitResult.OK, release.tryEmitEmpty());
                }).assertNext(message -> assertReason(message, "REVOKED_OR_EXPIRED")).verifyComplete();
        assertEquals(0, backend.received.get());
    }

    @Test
    @ExtendWith(OutputCaptureExtension.class)
    void defaultDiagnosticsOmitCallerIdentityAndCredentials(final CapturedOutput output) throws IOException {
        assertOrder(call(TENANT_A, ALICE, "A"), "A");
        JsonObject key = json(KEY);
        key.addProperty("revoked", true);
        writeKey(key);
        assertReason(call(TENANT_A, ALICE, "A"), "REVOKED_OR_EXPIRED");
        String log = output.getAll();
        assertFalse(log.contains(ALICE));
        assertFalse(log.contains(TENANT_A));
        assertFalse(log.contains("caller-secret"));
        assertFalse(log.contains("eyJhbGciOiJFZERTQS"));
        assertFalse(log.contains(Base64.getEncoder().encodeToString(keys.getPrivate().getEncoded())));
    }

    @Test
    @ExtendWith(OutputCaptureExtension.class)
    void transportFailureAfterSendingDoesNotReplayTheOrderRequest(final CapturedOutput output) {
        AtomicInteger attempts = new AtomicInteger();
        WebClient http = WebClient.builder().exchangeFunction(request -> Mono.defer(() -> {
            attempts.incrementAndGet();
            return Mono.error(new TimeoutException("private connector details " + request.headers().getFirst(HttpHeaders.AUTHORIZATION)));
        })).build();
        handler = handler(DOMAIN, http, false);
        assertDenied(call(TENANT_A, ALICE, "A"));
        assertEquals(1, attempts.get());
        String log = output.getAll();
        assertTrue(log.contains("Delegated HTTP transport failed"));
        assertFalse(log.contains("private connector details"));
        assertFalse(log.contains("Bearer eyJ"));
    }

    @Test
    void reroutingAfterUpstreamSelectionIsRejectedBeforeSending() {
        handler = handler(DOMAIN, WebClient.builder().clientConnector(new HttpHandlerConnector(backend)).build(), true);
        assertReason(call(TENANT_A, ALICE, "A"), "TARGET_MISMATCH");
        assertEquals(0, backend.received.get());
    }

    private void writeKey(final JsonObject config) throws IOException {
        config.addProperty("privateKeyPkcs8Base64", Base64.getEncoder().encodeToString(keys.getPrivate().getEncoded()));
        Files.writeString(file, config.toString());
    }

    private ShenyuWebHandler handler(final String domain, final WebClient http, final boolean reroute) {
        DiscoveryUpstreamData upstream = DiscoveryUpstreamData.builder().protocol("http://").url(URI.create(domain).getAuthority()).status(0)
                .props("{\"healthCheckEnabled\":false}").build();
        DiscoverySyncData data = new DiscoverySyncData();
        data.setSelectorId(route.getId());
        data.setUpstreamDataList(List.of(upstream));
        new DivideUpstreamDataHandler().handlerDiscoveryUpstreamData(data);
        DividePlugin divide = new DividePlugin() {
            @Override
            public Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
                return doExecute(exchange, chain, route, divideRule);
            }
        };
        Stage select = new Stage((exchange, chain) -> {
            URI approved = RequestUrlUtils.buildRequestUri(exchange, exchange.getRequiredAttribute(Constants.HTTP_DOMAIN));
            exchange.getAttributes().put(Constants.HTTP_URI, reroute ? URI.create("http://other.test" + PATH) : approved);
            return chain.execute(exchange);
        });
        Stage respond = new Stage((exchange, chain) -> {
            ResponseEntity<Flux<DataBuffer>> entity = exchange.getRequiredAttribute(Constants.CLIENT_RESPONSE_ATTR);
            return exchange.getResponse().writeWith(Objects.requireNonNull(entity.getBody()));
        });
        return new ShenyuWebHandler(List.of(gateway, divide, select, new WebClientPlugin(http, 8192), respond), null, new ShenyuConfig());
    }

    private JsonObject call(final String tenant, final String subject, final String order) {
        JsonObject args = new JsonObject();
        args.addProperty("id", order);
        return dispatch(tenant, subject, args, Set.of(TOOL)).block(Duration.ofSeconds(5));
    }

    private Mono<JsonObject> dispatch(final String tenant, final String subject, final JsonObject args, final Set<String> grants) {
        JsonObject request = json("""
                {"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"orders.lookup","_meta":{
                "io.modelcontextprotocol/protocolVersion":"2026-07-28","io.modelcontextprotocol/clientCapabilities":{"tenant":"private-tenant-b"}}}}
                """);
        request.getAsJsonObject("params").add("arguments", args);
        MockServerHttpRequest http = MockServerHttpRequest.post("/agent").contentType(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.ACCEPT, "application/json, text/event-stream")
                .header("Mcp-Method", "tools/call").header("Mcp-Name", TOOL).header("MCP-Protocol-Version", AgentMcpRequestParser.VERSION)
                .header(HttpHeaders.AUTHORIZATION, "Bearer caller-secret").header("X-Tenant", "private-tenant-b").body(request.toString());
        MockServerWebExchange exchange = MockServerWebExchange.builder(http)
                .principal(new AgentMcpPrincipal(new AgentMcpIdentity(tenant, subject, grants, Set.of()))).build();
        return handler.handle(exchange).then(Mono.defer(exchange.getResponse()::getBodyAsString)).map(wire -> {
            assertFalse(wire.contains(subject));
            assertFalse(wire.contains(tenant));
            assertFalse(wire.contains("caller-secret"));
            assertFalse(wire.contains("eyJ"));
            assertEquals("Bearer caller-secret", exchange.getRequest().getHeaders().getFirst(HttpHeaders.AUTHORIZATION));
            return json(wire);
        });
    }

    private static JsonObject json(final String value) {
        return JsonParser.parseString(value).getAsJsonObject();
    }

    private static void assertOrder(final JsonObject message, final String order) {
        JsonObject result = message.getAsJsonObject("result");
        assertFalse(result.get("isError").getAsBoolean());
        assertEquals(order, result.getAsJsonObject("structuredContent").getAsJsonObject("body").get("orderId").getAsString());
    }

    private static void assertDenied(final JsonObject message) {
        assertTrue(message.has("error") || message.getAsJsonObject("result").get("isError").getAsBoolean());
    }

    private static void assertReason(final JsonObject message, final String reason) {
        assertDenied(message);
        assertTrue(message.toString().contains(reason));
    }

    private static final class SelectedGateway extends AgentGatewayPlugin {

        private final RuleData rule = new RuleData();

        private final PluginData plugin = new PluginData();

        private final SelectorData selector = new SelectorData();

        private final PluginData previous;

        SelectedGateway(final AgentHttpIdentityDelegator delegator) {
            super(new AgentMcpHttpHandler(new AgentMcpDispatcher(new AgentToolRegistry(List.of()), "orders-test", "1"),
                    new AgentMcpPrincipalSecurityResolver()), null, null, delegator);
            previous = BaseDataCache.getInstance().obtainPluginData(named());
            plugin.setName(named());
            plugin.setEnabled(true);
            BaseDataCache.getInstance().cachePluginData(plugin);
            selector.setId(UUID.randomUUID().toString());
            selector.setPluginName(named());
            selector.setContinued(true);
            selector.setEnabled(true);
            selector.setSort(0);
            BaseDataCache.getInstance().cacheSelectData(selector);
            rule.setId(UUID.randomUUID().toString());
            rule.setSelectorId(selector.getId());
            rule.setPluginName(named());
            rule.setEnabled(true);
            rule.setSort(0);
            update(RULE);
        }

        void update(final String settings) {
            rule.setHandle(settings);
            BaseDataCache.getInstance().cacheRuleData(rule);
            new AgentGatewayPluginDataHandler().handlerRule(rule);
        }

        void clear() {
            new AgentGatewayPluginDataHandler().removeRule(rule);
            BaseDataCache cache = BaseDataCache.getInstance();
            cache.removeRuleData(rule);
            cache.removeSelectData(selector);
            cache.removePluginData(plugin);
            if (Objects.nonNull(previous)) {
                cache.cachePluginData(previous);
            }
        }

        @Override
        public Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
            return doExecute(exchange, chain, selector, rule);
        }
    }

    private record Stage(BiFunction<ServerWebExchange, ShenyuPluginChain, Mono<Void>> action) implements ShenyuPlugin {

        @Override
        public int getOrder() {
            return 0;
        }

        @Override
        public Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
            return action.apply(exchange, chain);
        }
    }

    private final class OrdersApi implements HttpHandler {

        private final PublicKey trustedKey = keys.getPublic();

        private final AtomicInteger received = new AtomicInteger();

        private final AtomicInteger disclosed = new AtomicInteger();

        private final Set<String> ids = ConcurrentHashMap.newKeySet();

        private final Map<String, List<String>> owners = Map.of("A", List.of(TENANT_A, ALICE), "B", List.of("private-tenant-b", "private-subject-bob"));

        private final Sinks.Empty<Void> bothArrived = Sinks.empty();

        private boolean waitForTwo;

        private Clock clock = Clock.system(ZoneOffset.UTC);

        @Override
        public Mono<Void> handle(final ServerHttpRequest request, final ServerHttpResponse response) {
            int count = received.incrementAndGet();
            if (count == 2) {
                bothArrived.tryEmitEmpty();
            }
            return (waitForTwo ? bothArrived.asMono() : Mono.<Void>empty()).then(Mono.defer(() -> authorize(request, response)));
        }

        private Mono<Void> authorize(final ServerHttpRequest request, final ServerHttpResponse response) {
            JsonObject claims;
            try {
                claims = verify(request);
            } catch (GeneralSecurityException | RuntimeException e) {
                response.setStatusCode(HttpStatus.UNAUTHORIZED);
                return response.setComplete();
            }
            String id = request.getQueryParams().getFirst("id");
            if (!List.of(claims.get("tenant").getAsString(), claims.get("sub").getAsString()).equals(owners.get(id))) {
                response.setStatusCode(HttpStatus.FORBIDDEN);
                return response.setComplete();
            }
            disclosed.incrementAndGet();
            response.getHeaders().setContentType(MediaType.APPLICATION_JSON);
            return response.writeWith(Mono.just(response.bufferFactory().wrap(("{\"orderId\":\"" + id + "\"}").getBytes(StandardCharsets.UTF_8))));
        }

        private JsonObject verify(final ServerHttpRequest request) throws GeneralSecurityException {
            List<String> auth = request.getHeaders().getOrEmpty(HttpHeaders.AUTHORIZATION);
            if (auth.size() != 1 || !auth.get(0).startsWith("Bearer ") || request.getHeaders().containsKey("X-Tenant")) {
                throw new GeneralSecurityException();
            }
            String[] parts = auth.get(0).substring(7).split("\\.", -1);
            Base64.Decoder decoder = Base64.getUrlDecoder();
            Signature verifier = Signature.getInstance("Ed25519");
            verifier.initVerify(trustedKey);
            verifier.update((parts[0] + "." + parts[1]).getBytes(StandardCharsets.US_ASCII));
            if (parts.length != 3 || !verifier.verify(decoder.decode(parts[2]))) {
                throw new GeneralSecurityException();
            }
            JsonObject header = json(new String(decoder.decode(parts[0]), StandardCharsets.UTF_8));
            JsonObject claims = json(new String(decoder.decode(parts[1]), StandardCharsets.UTF_8));
            Instant instant = clock.instant();
            BigDecimal now = BigDecimal.valueOf(instant.getEpochSecond()).add(BigDecimal.valueOf(instant.getNano(), 9));
            if (!"EdDSA".equals(header.get("alg").getAsString()) || !"orders:v1".equals(header.get("kid").getAsString())
                    || !"shenyu-orders".equals(claims.get("iss").getAsString()) || !"orders-api".equals(claims.get("aud").getAsString())
                    || claims.get("exp").getAsBigDecimal().compareTo(now) <= 0 || claims.get("nbf").getAsBigDecimal().compareTo(now) > 0
                    || !List.of(claims.get("scope").getAsString().split(" ")).contains("orders.read") || !TOOL.equals(claims.get("tool").getAsString())
                    || !HttpMethod.GET.equals(request.getMethod()) || !PATH.equals(request.getURI().getRawPath())
                    || !("GET " + PATH).equals(claims.get("route").getAsString())) {
                throw new GeneralSecurityException();
            }
            ids.add(claims.get("jti").getAsString());
            return claims;
        }
    }
}
