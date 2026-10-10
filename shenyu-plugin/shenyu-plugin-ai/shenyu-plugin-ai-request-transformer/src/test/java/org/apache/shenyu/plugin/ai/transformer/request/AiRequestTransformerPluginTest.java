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
import java.util.List;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;

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
        plugin = new AiRequestTransformerPlugin(HandlerStrategies.withDefaults().messageReaders(), aiModelFactoryRegistry);
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
}
