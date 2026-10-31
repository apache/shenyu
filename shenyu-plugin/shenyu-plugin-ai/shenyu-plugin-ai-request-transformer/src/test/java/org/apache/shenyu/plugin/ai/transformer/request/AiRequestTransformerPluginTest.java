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

package org.apache.shenyu.plugin.ai.transformer.request;

import com.google.gson.JsonSyntaxException;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.AiRequestTransformerHandle;
import org.apache.shenyu.plugin.ai.common.spring.ai.registry.AiModelFactoryRegistry;
import org.apache.shenyu.plugin.ai.transformer.request.cache.ChatClientCache;
import org.apache.shenyu.plugin.ai.transformer.request.handler.AiRequestTransformerPluginHandler;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.utils.RequestUrlUtils;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.MockedStatic;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.core.io.buffer.DefaultDataBufferFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.codec.HttpMessageReader;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.reactive.function.server.HandlerStrategies;
import org.springframework.web.reactive.function.server.ServerRequest;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.RETURNS_DEEP_STUBS;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class AiRequestTransformerPluginTest {

    private AiModelFactoryRegistry aiModelFactoryRegistry;

    private ChatClientCache chatClientCache;

    private ShenyuPluginChain chain;

    private AiRequestTransformerPlugin plugin;

    @BeforeEach
    void setUp() {

        aiModelFactoryRegistry = mock(AiModelFactoryRegistry.class);
        chatClientCache = mock(ChatClientCache.class);
        chain = mock(ShenyuPluginChain.class);
        plugin = new AiRequestTransformerPlugin(aiModelFactoryRegistry);
    }

    @Test
    void testDoExecuteWithMissingConfigurations() {

        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/test").build());
        RuleData rule = new RuleData();
        rule.setId("test-request-rule-id");
        rule.setSelectorId("test-selector-id");

        when(chain.execute(exchange)).thenReturn(Mono.empty());

        SelectorData selector = new SelectorData();
        StepVerifier.create(plugin.doExecute(exchange, chain, selector, rule))
                .verifyComplete();

        verify(chain).execute(exchange);
    }

    @ParameterizedTest
    @ValueSource(strings = {
            "[{\"generated\":\"array\",\"nested\":[[1,2],true,null],\"number\":12345678901234567890},1,true,null]",
            "[]",
            "{\n  \"generated\": \"object\",\n  \"items\": [1,true],\n  \"missing\": null\n}",
            "{\"a\":[[1,2]]}"
    })
    void testDoExecuteWithValidConfigurations(final String body) {
        AiRequestTransformerHandle handle = new AiRequestTransformerHandle();
        handle.setProvider("TEST_PROVIDER");
        handle.setBaseUrl("http://test.com");
        handle.setApiKey("test-api-key");
        handle.setModel("test-model");

        ChatClient mockClient = mock(ChatClient.class, RETURNS_DEEP_STUBS);
        String aiResponse = "POST /test HTTP/1.1\nContent-Type: application/json\n\n" + body;
        when(mockClient.prompt().user(anyString()).stream().content()).thenReturn(Flux.just(aiResponse));
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/test")
                .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .body("[{\"original\":true}]"));
        RuleData rule = new RuleData();
        rule.setId("test-json-body-rule-id");
        rule.setSelectorId("test-selector-id");
        when(chatClientCache.getClient(rule.getId())).thenReturn(mockClient);
        List<HttpMessageReader<?>> readers = HandlerStrategies.withDefaults().messageReaders();
        AtomicReference<String> downstreamBody = new AtomicReference<>();
        when(chain.execute(any(ServerWebExchange.class))).thenAnswer(invocation ->
                ServerRequest.create(invocation.getArgument(0), readers).bodyToMono(String.class)
                        .doOnNext(downstreamBody::set).then());

        String cacheKey = CacheKeyUtils.INST.getKey(rule);
        AiRequestTransformerPluginHandler.CACHED_HANDLE.get().cachedHandle(cacheKey, handle);
        try (MockedStatic<ChatClientCache> mockedCache = mockStatic(ChatClientCache.class)) {
            mockedCache.when(ChatClientCache::getInstance).thenReturn(chatClientCache);
            StepVerifier.create(plugin.doExecute(exchange, chain, new SelectorData(), rule)).verifyComplete();

            assertEquals(body, downstreamBody.get());
            verify(chain).execute(any(ServerWebExchange.class));
        } finally {
            AiRequestTransformerPluginHandler.CACHED_HANDLE.get().removeHandle(cacheKey);
        }
    }

    @Test
    void testModelFailureProceedsWithoutTransformation() {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/test")
                .contentType(MediaType.APPLICATION_JSON).body("{}"));
        RuleData rule = new RuleData();
        rule.setId("model-failure-rule-id");
        rule.setSelectorId("test-selector-id");
        AtomicReference<String> downstreamBody = new AtomicReference<>();
        ShenyuPluginChain readingChain = downstream -> DataBufferUtils.join(downstream.getRequest().getBody())
                .doOnNext(buffer -> {
                    downstreamBody.set(buffer.toString(StandardCharsets.UTF_8));
                    DataBufferUtils.release(buffer);
                }).then();

        invokeWithModel(exchange, Flux.error(new IllegalStateException("model failure")), readingChain, rule,
            result -> StepVerifier.create(result).verifyComplete());

        assertEquals("{}", downstreamBody.get());
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void testOneShotRequestBodyIsReadOnceAndReplayed(final boolean modelFails) {
        String original = "{\"original\":true}";
        AtomicInteger subscriptions = new AtomicInteger();
        Flux<DataBuffer> oneShotBody = Flux.defer(() -> subscriptions.getAndIncrement() == 0
                ? Flux.just(new DefaultDataBufferFactory().wrap(original.getBytes(StandardCharsets.UTF_8)))
                : Flux.error(new IllegalStateException("request body already consumed")));
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/test")
                .contentType(MediaType.APPLICATION_JSON).body(oneShotBody));
        AtomicReference<String> downstreamBody = new AtomicReference<>();
        ShenyuPluginChain readingChain = downstream -> DataBufferUtils.join(downstream.getRequest().getBody())
                .doOnNext(buffer -> {
                    downstreamBody.set(buffer.toString(StandardCharsets.UTF_8));
                    DataBufferUtils.release(buffer);
                }).then();
        Flux<String> model = modelFails ? Flux.error(new IllegalStateException("model unavailable"))
                : Flux.just("POST /test HTTP/1.1\nContent-Type: application/json\n\n{\"transformed\":true}");
        RuleData rule = new RuleData();
        rule.setId("one-shot-body-rule-id");
        rule.setSelectorId("test-selector-id");

        invokeWithModel(exchange, model, readingChain, rule,
            result -> StepVerifier.create(result).verifyComplete());

        assertEquals(modelFails ? original : "{\"transformed\":true}", downstreamBody.get());
        assertEquals(1, subscriptions.get());
    }

    @Test
    void testEmptyRequestBodyIsReplacedByGeneratedBody() {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/test")
                .contentType(MediaType.APPLICATION_JSON).build());
        RuleData rule = new RuleData();
        rule.setId("empty-body-rule-id");
        rule.setSelectorId("test-selector-id");
        AtomicReference<String> downstreamBody = new AtomicReference<>();
        ShenyuPluginChain readingChain = downstream -> DataBufferUtils.join(downstream.getRequest().getBody())
                .map(buffer -> {
                    try {
                        return buffer.toString(StandardCharsets.UTF_8);
                    } finally {
                        DataBufferUtils.release(buffer);
                    }
                })
                .defaultIfEmpty("")
                .doOnNext(downstreamBody::set).then();

        invokeWithModel(exchange, Flux.just("POST /test HTTP/1.1\nContent-Type: application/json\n\n{\"generated\":true}"),
            readingChain, rule, result -> StepVerifier.create(result).verifyComplete());

        assertEquals("{\"generated\":true}", downstreamBody.get());
    }

    @Test
    void testEmptyRequestBodyIsForwardedWhenTheModelFails() {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/test")
                .contentType(MediaType.APPLICATION_JSON).build());
        RuleData rule = new RuleData();
        rule.setId("empty-body-failure-rule-id");
        rule.setSelectorId("test-selector-id");
        AtomicReference<String> downstreamBody = new AtomicReference<>();
        ShenyuPluginChain readingChain = downstream -> DataBufferUtils.join(downstream.getRequest().getBody())
                .map(buffer -> {
                    try {
                        return buffer.toString(StandardCharsets.UTF_8);
                    } finally {
                        DataBufferUtils.release(buffer);
                    }
                })
                .defaultIfEmpty("")
                .doOnNext(downstreamBody::set).then();

        invokeWithModel(exchange, Flux.error(new IllegalStateException("model failure")), readingChain, rule,
            result -> StepVerifier.create(result).verifyComplete());

        assertEquals("", downstreamBody.get());
    }

    @Test
    void testFormRequestBodyIsReplacedByGeneratedFormBody() {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/test")
                .contentType(MediaType.APPLICATION_FORM_URLENCODED).body("a=1"));
        RuleData rule = new RuleData();
        rule.setId("form-body-rule-id");
        rule.setSelectorId("test-selector-id");
        AtomicReference<String> downstreamBody = new AtomicReference<>();
        ShenyuPluginChain readingChain = downstream -> DataBufferUtils.join(downstream.getRequest().getBody())
                .doOnNext(buffer -> {
                    downstreamBody.set(buffer.toString(StandardCharsets.UTF_8));
                    DataBufferUtils.release(buffer);
                }).then();

        invokeWithModel(exchange, Flux.just("POST /test HTTP/1.1\nContent-Type: application/x-www-form-urlencoded\n\n{\"b\":\"2\"}"),
            readingChain, rule, result -> StepVerifier.create(result).verifyComplete());

        assertEquals("b=2", downstreamBody.get());
    }

    @Test
    void testOversizedDeclaredBodySkipsTransformationWithoutReadingIt() {
        AtomicInteger subscriptions = new AtomicInteger();
        Flux<DataBuffer> body = Flux.defer(() -> {
            subscriptions.incrementAndGet();
            return Flux.just(new DefaultDataBufferFactory().wrap(new byte[1]));
        });
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/test")
                .contentType(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.CONTENT_LENGTH, String.valueOf(5 * 1024 * 1024 + 1))
                .body(body));
        RuleData rule = new RuleData();
        rule.setId("oversized-body-rule-id");
        rule.setSelectorId("test-selector-id");
        when(chain.execute(exchange)).thenReturn(Mono.empty());

        invokeWithModel(exchange, Flux.just("POST /test HTTP/1.1\nContent-Type: application/json\n\n{}"), chain, rule,
            result -> StepVerifier.create(result).verifyComplete());

        verify(chain).execute(exchange);
        assertEquals(0, subscriptions.get());
    }

    @Test
    void testConvertBodyJsonWithMalformedJson() {
        String aiResponse = "POST /test HTTP/1.1\nContent-Type: application/json\n\n[{\"broken\":}]";
        assertThrows(JsonSyntaxException.class, () -> AiRequestTransformerPlugin.convertBodyJson(aiResponse));
    }

    @ParameterizedTest
    @CsvSource({
        ", http://upstream.example/rewritten",
        "/forced, http://upstream.example/forced"
    })
    void testRewriteRequestRouting(final String rewriteUri, final String expectedOutboundUri) {
        AiRequestTransformerHandle handle = new AiRequestTransformerHandle();
        handle.setProvider("TEST_PROVIDER");
        handle.setBaseUrl("http://test.com");
        handle.setApiKey("test-api-key");
        handle.setModel("test-model");

        ChatClient mockClient = mock(ChatClient.class, RETURNS_DEEP_STUBS);
        String aiResponse = "POST /rewritten HTTP/1.1\nContent-Type: application/json\n\n{}";
        when(mockClient.prompt().user(anyString()).stream().content()).thenReturn(Flux.just(aiResponse));
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("http://localhost/original")
                .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .body("{}"));
        ShenyuContext context = new ShenyuContext();
        context.setPath("/original");
        context.setRealUrl("/original");
        exchange.getAttributes().put(Constants.CONTEXT, context);
        if (Objects.nonNull(rewriteUri)) {
            exchange.getAttributes().put(Constants.REWRITE_URI, rewriteUri);
        }
        RuleData rule = new RuleData();
        rule.setId("test-routing-rule-id");
        rule.setSelectorId("test-selector-id");
        when(chatClientCache.getClient(rule.getId())).thenReturn(mockClient);
        AtomicReference<ServerWebExchange> downstreamExchange = new AtomicReference<>();
        when(chain.execute(any(ServerWebExchange.class))).thenAnswer(invocation -> {
            downstreamExchange.set(invocation.getArgument(0));
            return Mono.empty();
        });

        String cacheKey = CacheKeyUtils.INST.getKey(rule);
        AiRequestTransformerPluginHandler.CACHED_HANDLE.get().cachedHandle(cacheKey, handle);
        try (MockedStatic<ChatClientCache> mockedCache = mockStatic(ChatClientCache.class)) {
            mockedCache.when(ChatClientCache::getInstance).thenReturn(chatClientCache);
            StepVerifier.create(plugin.doExecute(exchange, chain, new SelectorData(), rule)).verifyComplete();

            ServerWebExchange downstream = downstreamExchange.get();
            assertEquals(URI.create("http://localhost/rewritten"), downstream.getRequest().getURI());
            assertEquals(URI.create(expectedOutboundUri), RequestUrlUtils.buildRequestUri(downstream, "http://upstream.example"));
            assertEquals("/rewritten", context.getPath());
            assertEquals("/rewritten", context.getRealUrl());
        } finally {
            AiRequestTransformerPluginHandler.CACHED_HANDLE.get().removeHandle(cacheKey);
        }
    }

    @Test
    void testExtractHeadersFromAiResponse() {

        String aiResponse = "HTTP/1.1 / 200 OK\nContent-Type: application/json\nAuthorization: Bearer token\n\n{\"key\":\"value\"}";
        HttpHeaders headers = AiRequestTransformerPlugin.extractHeadersFromAiResponse(aiResponse);
        assertEquals("application/json", headers.getFirst(HttpHeaders.CONTENT_TYPE));
        assertEquals("Bearer token", headers.getFirst(HttpHeaders.AUTHORIZATION));
    }

    @Test
    void testRewriteRequestPath() {

        String aiResponse = "HTTP/1.1 / 200 OK\nContent-Type: application/json\nAuthorization: Bearer token\n\n{\"key\":\"value\"}";
        String result = AiRequestTransformerPlugin.extractRequestPathFromAiResponse(aiResponse);
        assertEquals("/", result);
    }

    @Test
    void testRewriteRequestPathWithQueryString() {
        String aiResponse = "POST /rewritten?new=2 HTTP/1.1\nContent-Type: application/json\n\n{\"ai\":\"query\"}";

        URI forwardedUri = forwardAndCaptureRequestUri(aiResponse, "/original?old=1");

        assertEquals("/rewritten", forwardedUri.getPath());
        assertEquals("new=2", forwardedUri.getQuery());
    }

    @Test
    void testRewriteRequestPathKeepsPercentEncodedQueryString() {
        String aiResponse = "POST /rewritten?q=a%20b&n=1 HTTP/1.1\nContent-Type: application/json\n\n{\"ai\":\"query\"}";

        URI forwardedUri = forwardAndCaptureRequestUri(aiResponse, "/original");

        assertEquals("/rewritten", forwardedUri.getPath());
        assertEquals("q=a%20b&n=1", forwardedUri.getRawQuery());
    }

    @Test
    void testRewriteRequestPathKeepsOriginalTargetOnMalformedQueryString() {
        String aiResponse = "POST /rewritten?bad=%zz HTTP/1.1\nContent-Type: application/json\n\n{\"ai\":\"query\"}";

        URI forwardedUri = forwardAndCaptureRequestUri(aiResponse, "/original?old=1");

        assertEquals("/original", forwardedUri.getPath());
        assertEquals("old=1", forwardedUri.getQuery());
    }

    /**
     * Runs the plugin with a mocked model answer and returns the request URI the plugin chain received.
     *
     * @param aiResponse  the mocked model answer, a full HTTP request message
     * @param originalUri the URI of the incoming request
     * @return the URI handed to the downstream plugin chain
     */
    private URI forwardAndCaptureRequestUri(final String aiResponse, final String originalUri) {
        AiRequestTransformerHandle handle = new AiRequestTransformerHandle();
        handle.setProvider("TEST_PROVIDER");
        handle.setBaseUrl("http://test.com");
        handle.setApiKey("test-api-key");
        handle.setModel("test-model");

        ChatClient mockClient = mock(ChatClient.class, RETURNS_DEEP_STUBS);
        when(mockClient.prompt().user(anyString()).stream().content()).thenReturn(Flux.just(aiResponse));
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post(originalUri)
                .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .body("{\"original\":true}"));
        RuleData rule = new RuleData();
        rule.setId("test-request-target-rule-id");
        rule.setSelectorId("test-selector-id");
        when(chatClientCache.getClient(rule.getId())).thenReturn(mockClient);
        AtomicReference<URI> downstreamUri = new AtomicReference<>();
        when(chain.execute(any(ServerWebExchange.class))).thenAnswer(invocation -> {
            downstreamUri.set(invocation.getArgument(0, ServerWebExchange.class).getRequest().getURI());
            return Mono.empty();
        });

        String cacheKey = CacheKeyUtils.INST.getKey(rule);
        AiRequestTransformerPluginHandler.CACHED_HANDLE.get().cachedHandle(cacheKey, handle);
        try (MockedStatic<ChatClientCache> mockedCache = mockStatic(ChatClientCache.class)) {
            mockedCache.when(ChatClientCache::getInstance).thenReturn(chatClientCache);
            StepVerifier.create(plugin.doExecute(exchange, chain, new SelectorData(), rule)).verifyComplete();
        } finally {
            AiRequestTransformerPluginHandler.CACHED_HANDLE.get().removeHandle(cacheKey);
        }

        return downstreamUri.get();
    }

    /**
     * Runs the plugin with a mocked model answer and lets the caller subscribe to the result.
     *
     * @param exchange        the incoming request exchange
     * @param model           the mocked model output stream
     * @param downstreamChain the plugin chain invoked after the transformation
     * @param rule            the rule that selects the cached AI handle
     * @param assertion       subscribes to and asserts on the plugin result
     */
    private void invokeWithModel(final MockServerWebExchange exchange, final Flux<String> model,
                                 final ShenyuPluginChain downstreamChain, final RuleData rule,
                                 final Consumer<Mono<Void>> assertion) {
        AiRequestTransformerHandle handle = new AiRequestTransformerHandle();
        handle.setProvider("TEST_PROVIDER");
        handle.setBaseUrl("http://test.invalid");
        handle.setApiKey("synthetic-audit-key");
        handle.setModel("audit-model");

        ChatClient mockClient = mock(ChatClient.class, RETURNS_DEEP_STUBS);
        when(mockClient.prompt().user(anyString()).stream().content()).thenReturn(model);
        when(chatClientCache.getClient(rule.getId())).thenReturn(mockClient);

        String cacheKey = CacheKeyUtils.INST.getKey(rule);
        AiRequestTransformerPluginHandler.CACHED_HANDLE.get().cachedHandle(cacheKey, handle);
        try (MockedStatic<ChatClientCache> mockedCache = mockStatic(ChatClientCache.class)) {
            mockedCache.when(ChatClientCache::getInstance).thenReturn(chatClientCache);
            assertion.accept(plugin.doExecute(exchange, downstreamChain, new SelectorData(), rule));
        } finally {
            AiRequestTransformerPluginHandler.CACHED_HANDLE.get().removeHandle(cacheKey);
        }
    }
}
