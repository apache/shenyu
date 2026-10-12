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

package org.apache.shenyu.plugin.ai.proxy.enhanced;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.ProxyApiKeyData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.AiProxyHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.ai.proxy.enhanced.cache.AiProxyApiKeyCache;
import org.apache.shenyu.plugin.ai.proxy.enhanced.handler.AiProxyPluginHandler;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiProxyException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocolFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiChat;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProviderFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAi;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAiCompatible;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyConfigService;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyEngine;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiProxyTransport;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.HttpStatus;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

/**
 * Compatibility tests through AiProxyEngine instead of Spring AI clients.
 */
class AiProxyPluginTest {

    private static final String SELECTOR_ID = "selector-test";

    private static final String REQUEST_BODY = "{\"messages\":[{\"role\":\"user\",\"content\":\"Hello\"}],\"unknown\":42}";

    private final AiProxyPluginHandler handler = new AiProxyPluginHandler();

    private final SelectorData selector = new SelectorData();

    private final RuleData rule = new RuleData();

    private final List<AiUpstreamRequest> requests = new ArrayList<>();

    private AiProxyHandle handle;

    private MockServerWebExchange exchange;

    @BeforeEach
    void setUp() {
        selector.setId(SELECTOR_ID);
        handle = new AiProxyHandle();
        handle.setProvider("OpenAI");
        handle.setBaseUrl("https://main.example");
        handle.setApiKey("primary-key");
        handle.setModel("primary-model");
        exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/test").body(REQUEST_BODY));
    }

    @AfterEach
    void cleanup() {
        AiProxyApiKeyCache.getInstance().refresh();
        handler.getSelectorCachedHandle().removeHandle(CacheKeyUtils.INST.getKey(SELECTOR_ID, Constants.DEFAULT_RULE));
    }

    @Test
    void testExecuteSuccess() throws Exception {
        AiProxyPlugin plugin = plugin(request -> Mono.just(response(201, "{\"choices\":[],\"vendor\":true}")));
        StepVerifier.create(execute(plugin)).verifyComplete();
        assertEquals(HttpStatus.CREATED, exchange.getResponse().getStatusCode());
        assertEquals("{\"choices\":[],\"vendor\":true}", exchange.getResponse().getBodyAsString().block());
        assertEquals("retained", exchange.getResponse().getHeaders().getFirst("X-Upstream"));
        JsonNode forwarded = new ObjectMapper().readTree(requests.get(0).getBody());
        assertEquals("primary-model", forwarded.get("model").asText());
        assertEquals(42, forwarded.get("unknown").asInt());
        assertEquals("Hello", forwarded.get("messages").get(0).get("content").asText());
    }

    @ParameterizedTest
    @ValueSource(ints = {400, 401, 429, 500, 503})
    void forwardsUpstreamErrorsAndHeaders(final int status) {
        AiProxyPlugin plugin = plugin(request -> Mono.just(response(status, "{\"error\":\"upstream\",\"vendor\":true}")));
        StepVerifier.create(execute(plugin)).verifyComplete();
        assertEquals(status, exchange.getResponse().getStatusCode().value());
        assertEquals("{\"error\":\"upstream\",\"vendor\":true}", exchange.getResponse().getBodyAsString().block());
        assertEquals(status == 429 || status >= 500 ? 4 : 1, requests.size());
    }

    @Test
    void testExecuteWithDynamicFallback() throws Exception {
        handle.setFallbackConfig(fallback("admin"));
        String body = REQUEST_BODY.substring(0, REQUEST_BODY.length() - 1)
                + ",\"fallbackConfig\":{\"provider\":\"DeepSeek\",\"baseUrl\":\"https://dynamic.example\",\"apiKey\":\"dynamic-key\",\"model\":\"dynamic-model\",\"maxTokens\":12}}";
        exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/test").body(body));
        AiProxyPlugin plugin = plugin(request -> Mono.just(request.getUri().getHost().equals("main.example") ? response(500, "busy") : response(200, "fallback")));
        StepVerifier.create(execute(plugin)).verifyComplete();
        assertEquals(5, requests.size());
        AiUpstreamRequest request = requests.get(4);
        assertEquals("dynamic.example", request.getUri().getHost());
        assertEquals(List.of("Bearer dynamic-key"), request.getHeaders().get("Authorization"));
        JsonNode forwarded = new ObjectMapper().readTree(request.getBody());
        assertEquals("dynamic-model", forwarded.get("model").asText());
        assertEquals(12, forwarded.get("max_tokens").asInt());
        assertFalse(forwarded.has("fallbackConfig"));
        assertEquals("fallback", exchange.getResponse().getBodyAsString().block());
    }

    @Test
    void testExecuteWithAdminFallback() {
        handle.setFallbackConfig(fallback("admin"));
        AiProxyPlugin plugin = plugin(request -> Mono.just(request.getUri().getHost().equals("main.example") ? response(400, "bad") : response(202, "admin")));
        StepVerifier.create(execute(plugin)).verifyComplete();
        assertEquals(2, requests.size());
        assertEquals("admin.example", requests.get(1).getUri().getHost());
        assertEquals(List.of("Bearer admin-key"), requests.get(1).getHeaders().get("Authorization"));
        assertEquals(HttpStatus.ACCEPTED, exchange.getResponse().getStatusCode());
    }

    @Test
    void testExecuteWithValidProxyApiKey() {
        handle.setProxyEnabled("true");
        AiProxyApiKeyCache.getInstance().cache(ProxyApiKeyData.builder().selectorId(SELECTOR_ID).proxyApiKey("proxy-valid")
                .realApiKey("real-key").enabled(true).build());
        exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/test").header(Constants.X_API_KEY, "proxy-valid").body(REQUEST_BODY));
        StepVerifier.create(execute(plugin(request -> Mono.just(response(200, "ok"))))).verifyComplete();
        assertEquals(List.of("Bearer real-key"), requests.get(0).getHeaders().get("Authorization"));
        assertFalse(requests.get(0).getHeaders().containsKey(Constants.X_API_KEY));
        assertEquals("primary-key", handle.getApiKey());
    }

    @Test
    void testExecuteWithInvalidProxyApiKey() {
        handle.setProxyEnabled("true");
        exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/test").header(Constants.X_API_KEY, "invalid").body(REQUEST_BODY));
        StepVerifier.create(execute(plugin(request -> Mono.just(response(200, "unexpected"))))).verifyComplete();
        assertEquals(HttpStatus.UNAUTHORIZED, exchange.getResponse().getStatusCode());
        assertTrue(requests.isEmpty());
    }

    @Test
    void missingProxyKeyIsUnauthorized() {
        handle.setProxyEnabled("true");
        StepVerifier.create(execute(plugin(request -> Mono.just(response(200, "unexpected"))))).verifyComplete();
        assertEquals(HttpStatus.UNAUTHORIZED, exchange.getResponse().getStatusCode());
        assertTrue(requests.isEmpty());
    }

    @Test
    void testRequestBodyTooLarge() {
        exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/test").body("x".repeat(5 * 1024 * 1024 + 1)));
        StepVerifier.create(execute(plugin(request -> Mono.just(response(200, "unexpected"))))).verifyComplete();
        assertEquals(HttpStatus.PAYLOAD_TOO_LARGE, exchange.getResponse().getStatusCode());
        assertTrue(requests.isEmpty());
    }

    @ParameterizedTest
    @ValueSource(strings = {"", "invalid", "{}", "[]"})
    void invalidRequestBodiesAreRejectedBeforeUpstream(final String body) {
        exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/test").body(body));
        StepVerifier.create(execute(plugin(request -> Mono.just(response(200, "unexpected"))))).verifyComplete();
        assertEquals(HttpStatus.BAD_REQUEST, exchange.getResponse().getStatusCode());
        assertTrue(requests.isEmpty());
    }

    @Test
    void streamsWithoutWaitingForCompletionAndCancellationReachesTransport() {
        handle.setStream(true);
        AtomicBoolean cancelled = new AtomicBoolean();
        List<String> received = new ArrayList<>();
        exchange.getResponse().setWriteHandler(body -> body.doOnNext(buffer -> {
            received.add(buffer.toString(StandardCharsets.UTF_8));
            DataBufferUtils.release(buffer);
        }).then());
        AiProxyPlugin plugin = plugin(request -> Mono.just(new AiUpstreamResponse(200, Map.of(), Flux.just(bytes("data: {\"vendor\":true}\n\n"))
                .concatWith(Flux.never()).doOnCancel(() -> cancelled.set(true)))));
        StepVerifier.create(execute(plugin)).thenAwait(Duration.ofMillis(20)).thenCancel().verify(Duration.ofSeconds(3));
        assertEquals(List.of("data: {\"vendor\":true}\n\n"), received);
        assertTrue(cancelled.get());
        assertEquals("text/event-stream", exchange.getResponse().getHeaders().getFirst("Content-Type"));
    }

    @Test
    void explicitClientStreamFalseOverridesSelectorDefault() throws Exception {
        handle.setStream(true);
        exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/test").body(REQUEST_BODY.substring(0, REQUEST_BODY.length() - 1) + ",\"stream\":false}"));
        StepVerifier.create(execute(plugin(request -> Mono.just(response(200, "normal"))))).verifyComplete();
        assertEquals("normal", exchange.getResponse().getBodyAsString().block());
        assertFalse(new ObjectMapper().readTree(requests.get(0).getBody()).get("stream").asBoolean());
    }

    @Test
    void emitsDoneOnceAndPreservesUnknownSseFields() {
        handle.setStream(true);
        StepVerifier.create(execute(plugin(request -> Mono.just(response(200, "data: {\"reasoning_content\":\"thought\",\"usage\":{},\"unknown\":true}\n\ndata: [DONE]\n\n"))))).verifyComplete();
        assertEquals("data: {\"reasoning_content\":\"thought\",\"usage\":{},\"unknown\":true}\n\ndata: [DONE]\n\n",
                exchange.getResponse().getBodyAsString().block());
    }

    @Test
    void errorsAfterCommitmentPropagateWithoutFallbackOrAnotherResponse() {
        handle.setStream(true);
        handle.setFallbackConfig(fallback("admin"));
        List<String> received = new ArrayList<>();
        exchange.getResponse().setWriteHandler(body -> body.doOnNext(buffer -> {
            received.add(buffer.toString(StandardCharsets.UTF_8));
            DataBufferUtils.release(buffer);
        }).then());
        AiProxyPlugin plugin = plugin(request -> Mono.just(new AiUpstreamResponse(200, Map.of(),
                Flux.just(bytes("data: {\"first\":true}\n\n")).concatWith(Flux.error(failure())))));
        StepVerifier.create(execute(plugin)).expectError(AiProxyException.class).verify();
        assertEquals(1, requests.size());
        assertEquals(List.of("data: {\"first\":true}\n\n"), received);
        assertEquals(HttpStatus.OK, exchange.getResponse().getStatusCode());
    }

    @Test
    void mapsConnectionFailureBeforeCommitmentToBadGateway() {
        StepVerifier.create(execute(plugin(request -> Mono.error(failure())))).verifyComplete();
        assertEquals(HttpStatus.BAD_GATEWAY, exchange.getResponse().getStatusCode());
        assertEquals(4, requests.size());
    }

    @Test
    void filtersHopHeadersAndPreservesEndToEndHeaders() {
        AiProxyPlugin plugin = plugin(request -> Mono.just(new AiUpstreamResponse(200, Map.of("Connection", List.of("X-Hop"),
                "X-Hop", List.of("remove"), "Transfer-Encoding", List.of("chunked"), "X-End", List.of("keep")), Flux.just(bytes("ok")))));
        StepVerifier.create(execute(plugin)).verifyComplete();
        assertEquals("keep", exchange.getResponse().getHeaders().getFirst("X-End"));
        assertFalse(exchange.getResponse().getHeaders().containsKey("Connection"));
        assertFalse(exchange.getResponse().getHeaders().containsKey("X-Hop"));
        assertFalse(exchange.getResponse().getHeaders().containsKey("Transfer-Encoding"));
    }

    @Test
    void testGetOrderAndNamed() {
        AiProxyPlugin plugin = plugin(request -> Mono.empty());
        assertEquals(PluginEnum.AI_PROXY.getCode(), plugin.getOrder());
        assertEquals(PluginEnum.AI_PROXY.getName(), plugin.named());
    }

    private AiProxyPlugin plugin(final AiProxyTransport transport) {
        handler.getSelectorCachedHandle().cachedHandle(CacheKeyUtils.INST.getKey(SELECTOR_ID, Constants.DEFAULT_RULE), handle);
        AiProxyEngine engine = new AiProxyEngine(new AiProxyProtocolFactory(List.of(new OpenAiChat())),
                new AiProxyProviderFactory(List.of(new OpenAi(), new OpenAiCompatible("DeepSeek"))), request -> {
                    requests.add(request);
                    return transport.execute(request);
                }, Duration.ZERO);
        return new AiProxyPlugin(new AiProxyConfigService(), engine, handler);
    }

    private Mono<Void> execute(final AiProxyPlugin plugin) {
        return plugin.doExecute(exchange, mock(ShenyuPluginChain.class), selector, rule);
    }

    private AiUpstreamResponse response(final int status, final String body) {
        return new AiUpstreamResponse(status, Map.of("X-Upstream", List.of("retained")), Flux.just(bytes(body)));
    }

    private byte[] bytes(final String value) {
        return value.getBytes(StandardCharsets.UTF_8);
    }

    private AiProxyHandle.FallbackConfig fallback(final String prefix) {
        AiProxyHandle.FallbackConfig config = new AiProxyHandle.FallbackConfig();
        config.setProvider("DeepSeek");
        config.setBaseUrl("https://" + prefix + ".example");
        config.setApiKey(prefix + "-key");
        config.setModel(prefix + "-model");
        return config;
    }

    private AiProxyException failure() {
        return new AiProxyException(AiProxyException.Kind.TRANSPORT, "fixture failure", null);
    }
}
