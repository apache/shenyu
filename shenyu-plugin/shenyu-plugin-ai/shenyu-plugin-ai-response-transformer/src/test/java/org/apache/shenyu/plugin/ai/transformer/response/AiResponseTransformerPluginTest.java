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

package org.apache.shenyu.plugin.ai.transformer.response;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.AiModelProviderEnum;
import org.apache.shenyu.common.dto.convert.plugin.AiResponseTransformerConfig;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.common.spring.ai.AiModelFactory;
import org.apache.shenyu.plugin.ai.common.spring.ai.registry.AiModelFactoryRegistry;
import org.apache.shenyu.plugin.ai.transformer.response.support.CachedRequestBodyDecorator;
import org.apache.shenyu.plugin.ai.transformer.response.template.AiResponseTransformerTemplate;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockedConstruction;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.ai.chat.model.ChatModel;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.http.server.reactive.MockServerHttpResponse;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

import com.fasterxml.jackson.databind.ObjectMapper;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.RETURNS_DEEP_STUBS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.lenient;

/**
 * Test for AiResponseTransformerPlugin.
 */
@ExtendWith(MockitoExtension.class)
class AiResponseTransformerPluginTest {

    @Mock
    private AiModelFactoryRegistry aiModelFactoryRegistry;

    @Mock
    private AiModelFactory aiModelFactory;

    @Mock
    private ChatModel chatModel;

    @Mock
    private ShenyuPluginChain chain;

    private AiResponseTransformerPlugin plugin;

    private ServerWebExchange exchange;

    @BeforeEach
    void setUp() {
        // Set up the configuration in Singleton
        AiResponseTransformerConfig config = new AiResponseTransformerConfig();
        config.setBaseUrl("http://test.com");
        config.setApiKey("test-key");
        config.setProvider("test-provider");
        Singleton.INST.single(AiResponseTransformerConfig.class, config);
        
        plugin = new AiResponseTransformerPlugin(Collections.emptyList(), aiModelFactoryRegistry);
        
        // Create test request and response
        final MockServerHttpRequest request = MockServerHttpRequest
                .method(HttpMethod.POST, "/test")
                .header("Content-Type", "application/json")
                .body("{\"test\":\"data\"}");
        
        MockServerHttpResponse response = new MockServerHttpResponse();
        response.setStatusCode(HttpStatus.OK);
        response.getHeaders().setContentType(MediaType.APPLICATION_JSON);
        
        exchange = mock(ServerWebExchange.class);
        lenient().when(exchange.getRequest()).thenReturn(request);
        lenient().when(exchange.getResponse()).thenReturn(response);
    }

    @Test
    void testGetOrder() {
        assertEquals(PluginEnum.AI_RESPONSE_TRANSFORMER.getCode(), plugin.getOrder());
    }

    @Test
    void testNamed() {
        assertEquals(PluginEnum.AI_RESPONSE_TRANSFORMER.getName(), plugin.named());
    }

    @Test
    void testDoExecute() {
        RuleData ruleData = new RuleData();
        ruleData.setId("test-response-rule-id");
        ruleData.setSelectorId("test-selector-id");
        
        // Mock the factory registry and factory
        lenient().when(aiModelFactoryRegistry.getFactory(AiModelProviderEnum.getByName("test-provider")))
                .thenReturn(aiModelFactory);
        lenient().when(aiModelFactory.createAiModel(any(AiCommonConfig.class)))
                .thenReturn(chatModel);
        
        // Mock exchange mutate for the full plugin flow
        ServerWebExchange.Builder builder = mock(ServerWebExchange.Builder.class);
        lenient().when(exchange.mutate()).thenReturn(builder);
        lenient().when(builder.request(any(ServerHttpRequest.class))).thenReturn(builder);
        lenient().when(builder.response(any())).thenReturn(builder);
        lenient().when(builder.build()).thenReturn(exchange);
        
        // Mock the chain to return empty Mono
        when(chain.execute(exchange)).thenReturn(Mono.empty());

        // Execute plugin - this should succeed with proper configuration
        SelectorData selectorData = mock(SelectorData.class);
        Mono<Void> result = plugin.doExecute(exchange, chain, selectorData, ruleData);
        
        // Verify execution result
        StepVerifier.create(result)
                .verifyComplete();

        ArgumentCaptor<ServerHttpRequest> requestCaptor = ArgumentCaptor.forClass(ServerHttpRequest.class);
        verify(builder).request(requestCaptor.capture());
        assertTrue(requestCaptor.getValue() instanceof CachedRequestBodyDecorator);
    }

    @Test
    void testGzipInputStreamClosedWhenDecompressionFails() throws IOException {
        MockServerHttpResponse response = (MockServerHttpResponse) exchange.getResponse();
        response.getHeaders().set(HttpHeaders.CONTENT_ENCODING, "gzip");
        AiResponseTransformerTemplate template = mock(AiResponseTransformerTemplate.class);
        ChatClient chatClient = mock(ChatClient.class);
        when(template.assembleMessage(exchange)).thenReturn(Mono.empty());
        AiResponseTransformerPlugin.AiResponseTransformerDecorator decorator =
                new AiResponseTransformerPlugin.AiResponseTransformerDecorator(exchange, template, chatClient);
        DataBuffer responseBody = response.bufferFactory().wrap("gzip".getBytes(StandardCharsets.UTF_8));

        try (MockedConstruction<GZIPInputStream> gzipStreams = mockConstruction(GZIPInputStream.class,
                (gzipInputStream, context) -> when(gzipInputStream.read(any(byte[].class))).thenThrow(new IOException()))) {
            StepVerifier.create(decorator.writeWith(Mono.just(responseBody)))
                    .verifyComplete();

            assertEquals(1, gzipStreams.constructed().size());
            verify(gzipStreams.constructed().get(0)).close();
        }
    }

    @ParameterizedTest
    @ValueSource(ints = {0, 1, 2})
    void testEmptyResponseIsCommitted(final int mode) {
        MockServerHttpResponse response = (MockServerHttpResponse) exchange.getResponse();
        response.setStatusCode(HttpStatus.NO_CONTENT);
        response.getHeaders().set("X-Original", "retained");
        AiResponseTransformerTemplate template = mock(AiResponseTransformerTemplate.class);
        ChatClient client = mock(ChatClient.class);
        AiResponseTransformerPlugin.AiResponseTransformerDecorator decorator =
                new AiResponseTransformerPlugin.AiResponseTransformerDecorator(exchange, template, client);
        Mono<Void> completion;
        if (mode == 0) {
            completion = decorator.writeWith(Flux.empty());
        } else if (mode == 1) {
            completion = decorator.writeAndFlushWith(Flux.empty());
        } else {
            completion = decorator.writeAndFlushWith(Flux.just(Flux.empty(), Flux.empty()));
        }
        StepVerifier.create(completion).verifyComplete();
        assertTrue(response.isCommitted());
        assertEquals(HttpStatus.NO_CONTENT, response.getStatusCode());
        assertEquals("retained", response.getHeaders().getFirst("X-Original"));
        verifyNoInteractions(template, client);
    }

    @Test
    void testWriteAndFlushWithTransformsResponse() {
        MockServerHttpResponse response = (MockServerHttpResponse) exchange.getResponse();
        AiResponseTransformerTemplate template = mock(AiResponseTransformerTemplate.class);
        when(template.assembleMessage(exchange)).thenReturn(Mono.just("{\"response\":{\"body\":\"\"}}"));
        ChatClient chatClient = mock(ChatClient.class, RETURNS_DEEP_STUBS);
        when(chatClient.prompt().user(anyString()).stream().content())
                .thenReturn(Flux.just("HTTP/1.1 200 OK\nContent-Type: application/json\n\n{\"transformed\":true}"));
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        when(context.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult());
        SpringBeanUtils.getInstance().setApplicationContext(context);
        AiResponseTransformerPlugin.AiResponseTransformerDecorator decorator =
                new AiResponseTransformerPlugin.AiResponseTransformerDecorator(exchange, template, chatClient);
        DataBuffer first = response.bufferFactory().wrap("orig".getBytes(StandardCharsets.UTF_8));
        DataBuffer second = response.bufferFactory().wrap("inal".getBytes(StandardCharsets.UTF_8));

        StepVerifier.create(decorator.writeAndFlushWith(Flux.just(Flux.just(first), Flux.just(second))))
                .verifyComplete();

        StepVerifier.create(response.getBodyAsString())
                .expectNext("{\"transformed\":true}")
                .verifyComplete();
    }

    @Test
    void testGeneratedStatusIsApplied() {
        MockServerHttpResponse response = (MockServerHttpResponse) exchange.getResponse();
        response.setStatusCode(HttpStatus.OK);
        AiResponseTransformerTemplate template = mock(AiResponseTransformerTemplate.class);
        when(template.assembleMessage(exchange)).thenReturn(Mono.just("{\"response\":{\"body\":\"\"}}"));
        ChatClient chatClient = mock(ChatClient.class, RETURNS_DEEP_STUBS);
        when(chatClient.prompt().user(anyString()).stream().content())
                .thenReturn(Flux.just("HTTP/1.1 404 Not Found\nContent-Type: application/json\n\n{\"error\":\"missing\"}"));
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        when(context.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult());
        SpringBeanUtils.getInstance().setApplicationContext(context);
        AiResponseTransformerPlugin.AiResponseTransformerDecorator decorator =
                new AiResponseTransformerPlugin.AiResponseTransformerDecorator(exchange, template, chatClient);
        DataBuffer responseBody = response.bufferFactory().wrap("original".getBytes(StandardCharsets.UTF_8));

        StepVerifier.create(decorator.writeWith(Mono.just(responseBody))).verifyComplete();

        assertEquals(HttpStatus.NOT_FOUND, response.getStatusCode());
        assertEquals("{\"error\":\"missing\"}", response.getBodyAsString().block());
    }

    @Test
    void testGeneratedSuccessStatusReplacesErrorStatus() {
        MockServerHttpResponse response = (MockServerHttpResponse) exchange.getResponse();
        response.setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
        AiResponseTransformerTemplate template = mock(AiResponseTransformerTemplate.class);
        when(template.assembleMessage(exchange)).thenReturn(Mono.just("{\"response\":{\"body\":\"\"}}"));
        ChatClient chatClient = mock(ChatClient.class, RETURNS_DEEP_STUBS);
        when(chatClient.prompt().user(anyString()).stream().content())
                .thenReturn(Flux.just("HTTP/1.1 201 Created\nContent-Type: application/json\n\n{\"created\":true}"));
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        when(context.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult());
        SpringBeanUtils.getInstance().setApplicationContext(context);
        AiResponseTransformerPlugin.AiResponseTransformerDecorator decorator =
                new AiResponseTransformerPlugin.AiResponseTransformerDecorator(exchange, template, chatClient);
        DataBuffer responseBody = response.bufferFactory().wrap("failed".getBytes(StandardCharsets.UTF_8));

        StepVerifier.create(decorator.writeWith(Mono.just(responseBody))).verifyComplete();

        assertEquals(HttpStatus.CREATED, response.getStatusCode());
        assertEquals("{\"created\":true}", response.getBodyAsString().block());
    }

    @Test
    void testUpstreamStatusRetainedWhenGeneratedStatusIsInvalid() {
        MockServerHttpResponse response = (MockServerHttpResponse) exchange.getResponse();
        response.setStatusCode(HttpStatus.ACCEPTED);
        AiResponseTransformerTemplate template = mock(AiResponseTransformerTemplate.class);
        when(template.assembleMessage(exchange)).thenReturn(Mono.just("{\"response\":{\"body\":\"\"}}"));
        ChatClient chatClient = mock(ChatClient.class, RETURNS_DEEP_STUBS);
        when(chatClient.prompt().user(anyString()).stream().content())
                .thenReturn(Flux.just("HTTP/1.1 999 Not Real\nContent-Type: application/json\n\n{\"ok\":true}"));
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        when(context.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult());
        SpringBeanUtils.getInstance().setApplicationContext(context);
        AiResponseTransformerPlugin.AiResponseTransformerDecorator decorator =
                new AiResponseTransformerPlugin.AiResponseTransformerDecorator(exchange, template, chatClient);
        DataBuffer responseBody = response.bufferFactory().wrap("original".getBytes(StandardCharsets.UTF_8));

        StepVerifier.create(decorator.writeWith(Mono.just(responseBody))).verifyComplete();

        assertEquals(HttpStatus.ACCEPTED, response.getStatusCode());
        assertEquals("{\"ok\":true}", response.getBodyAsString().block());
    }

    @Test
    void testUpstreamStatusRetainedWhenGeneratedStatusIsOmitted() {
        MockServerHttpResponse response = (MockServerHttpResponse) exchange.getResponse();
        response.setStatusCode(HttpStatus.ACCEPTED);
        AiResponseTransformerTemplate template = mock(AiResponseTransformerTemplate.class);
        when(template.assembleMessage(exchange)).thenReturn(Mono.just("{\"response\":{\"body\":\"\"}}"));
        ChatClient chatClient = mock(ChatClient.class, RETURNS_DEEP_STUBS);
        when(chatClient.prompt().user(anyString()).stream().content())
                .thenReturn(Flux.just("Content-Type: application/json\n\n{\"ok\":true}"));
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        when(context.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult());
        SpringBeanUtils.getInstance().setApplicationContext(context);
        AiResponseTransformerPlugin.AiResponseTransformerDecorator decorator =
                new AiResponseTransformerPlugin.AiResponseTransformerDecorator(exchange, template, chatClient);
        DataBuffer responseBody = response.bufferFactory().wrap("original".getBytes(StandardCharsets.UTF_8));

        StepVerifier.create(decorator.writeWith(Mono.just(responseBody))).verifyComplete();

        assertEquals(HttpStatus.ACCEPTED, response.getStatusCode());
        assertEquals("{\"ok\":true}", response.getBodyAsString().block());
    }

    @Test
    void testNoContentStatusDropsBody() {
        MockServerHttpResponse response = (MockServerHttpResponse) exchange.getResponse();
        response.setStatusCode(HttpStatus.OK);
        AiResponseTransformerTemplate template = mock(AiResponseTransformerTemplate.class);
        when(template.assembleMessage(exchange)).thenReturn(Mono.just("{\"response\":{\"body\":\"\"}}"));
        ChatClient chatClient = mock(ChatClient.class, RETURNS_DEEP_STUBS);
        when(chatClient.prompt().user(anyString()).stream().content())
                .thenReturn(Flux.just("HTTP/1.1 204 No Content\nContent-Type: application/json\n\n"));
        AiResponseTransformerPlugin.AiResponseTransformerDecorator decorator =
                new AiResponseTransformerPlugin.AiResponseTransformerDecorator(exchange, template, chatClient);
        DataBuffer responseBody = response.bufferFactory().wrap("original".getBytes(StandardCharsets.UTF_8));

        StepVerifier.create(decorator.writeWith(Mono.just(responseBody))).verifyComplete();

        assertEquals(HttpStatus.NO_CONTENT, response.getStatusCode());
        assertNull(response.getHeaders().getFirst(HttpHeaders.CONTENT_TYPE));
        assertEquals("", response.getBodyAsString().block());
    }

    @Test
    void testOriginalHeadersRemainWhenTransformedBodyIsInvalid() {
        MockServerHttpResponse response = (MockServerHttpResponse) exchange.getResponse();
        response.getHeaders().set("X-Original", "original");
        AiResponseTransformerTemplate template = mock(AiResponseTransformerTemplate.class);
        when(template.assembleMessage(exchange)).thenReturn(Mono.just("{\"response\":{\"body\":\"\"}}"));
        ChatClient chatClient = mock(ChatClient.class, RETURNS_DEEP_STUBS);
        when(chatClient.prompt().user(anyString()).stream().content())
                .thenReturn(Flux.just("HTTP/1.1 200 OK\nX-New: transformed"));
        AiResponseTransformerPlugin.AiResponseTransformerDecorator decorator =
                new AiResponseTransformerPlugin.AiResponseTransformerDecorator(exchange, template, chatClient);
        DataBuffer responseBody = response.bufferFactory().wrap("original".getBytes(StandardCharsets.UTF_8));

        StepVerifier.create(decorator.writeWith(Mono.just(responseBody))).verifyComplete();

        assertEquals("original", response.getHeaders().getFirst("X-Original"));
        assertNull(response.getHeaders().getFirst("X-New"));
        assertEquals("original", response.getBodyAsString().block());
    }

    @Test
    void testGzipFallbackOnModelErrorKeepsOriginalCompressedBytes() throws IOException {
        MockServerHttpResponse response = (MockServerHttpResponse) exchange.getResponse();
        response.getHeaders().set(HttpHeaders.CONTENT_ENCODING, "gzip");
        AiResponseTransformerTemplate template = mock(AiResponseTransformerTemplate.class);
        when(template.assembleMessage(exchange)).thenReturn(Mono.just("{\"response\":{\"body\":\"\"}}"));
        ChatClient chatClient = mock(ChatClient.class, RETURNS_DEEP_STUBS);
        when(chatClient.prompt().user(anyString()).stream().content())
                .thenReturn(Flux.error(new IllegalStateException("model unavailable")));
        registerResultBean();
        AiResponseTransformerPlugin.AiResponseTransformerDecorator decorator =
                new AiResponseTransformerPlugin.AiResponseTransformerDecorator(exchange, template, chatClient);
        byte[] gzipBytes = gzip("{\"customer\":\"alice\"}");

        StepVerifier.create(decorator.writeWith(Mono.just(response.bufferFactory().wrap(gzipBytes))))
                .verifyComplete();

        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertEquals("gzip", response.getHeaders().getFirst(HttpHeaders.CONTENT_ENCODING));
        assertArrayEquals(gzipBytes, bodyBytes(response));
        assertEquals("{\"customer\":\"alice\"}", gunzip(bodyBytes(response)));
    }

    @Test
    void testGzipFallbackOnInvalidGeneratedBodyKeepsOriginalCompressedBytes() throws IOException {
        MockServerHttpResponse response = (MockServerHttpResponse) exchange.getResponse();
        response.getHeaders().set(HttpHeaders.CONTENT_ENCODING, "gzip");
        AiResponseTransformerTemplate template = mock(AiResponseTransformerTemplate.class);
        when(template.assembleMessage(exchange)).thenReturn(Mono.just("{\"response\":{\"body\":\"\"}}"));
        ChatClient chatClient = mock(ChatClient.class, RETURNS_DEEP_STUBS);
        when(chatClient.prompt().user(anyString()).stream().content())
                .thenReturn(Flux.just("I cannot transform this response."));
        registerResultBean();
        AiResponseTransformerPlugin.AiResponseTransformerDecorator decorator =
                new AiResponseTransformerPlugin.AiResponseTransformerDecorator(exchange, template, chatClient);
        byte[] gzipBytes = gzip("{\"customer\":\"alice\"}");

        StepVerifier.create(decorator.writeWith(Mono.just(response.bufferFactory().wrap(gzipBytes))))
                .verifyComplete();

        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertEquals("gzip", response.getHeaders().getFirst(HttpHeaders.CONTENT_ENCODING));
        assertArrayEquals(gzipBytes, bodyBytes(response));
        assertEquals("{\"customer\":\"alice\"}", gunzip(bodyBytes(response)));
    }

    @Test
    void testUncompressedFallbackOnModelErrorKeepsOriginalBytes() {
        AiResponseTransformerTemplate template = mock(AiResponseTransformerTemplate.class);
        when(template.assembleMessage(exchange)).thenReturn(Mono.just("{\"response\":{\"body\":\"\"}}"));
        ChatClient chatClient = mock(ChatClient.class, RETURNS_DEEP_STUBS);
        when(chatClient.prompt().user(anyString()).stream().content())
                .thenReturn(Flux.error(new IllegalStateException("model unavailable")));
        registerResultBean();
        AiResponseTransformerPlugin.AiResponseTransformerDecorator decorator =
                new AiResponseTransformerPlugin.AiResponseTransformerDecorator(exchange, template, chatClient);
        MockServerHttpResponse response = (MockServerHttpResponse) exchange.getResponse();
        byte[] originalBytes = "{\"customer\":\"alice\"}".getBytes(StandardCharsets.UTF_8);

        StepVerifier.create(decorator.writeWith(Mono.just(response.bufferFactory().wrap(originalBytes))))
                .verifyComplete();

        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertNull(response.getHeaders().getFirst(HttpHeaders.CONTENT_ENCODING));
        assertArrayEquals(originalBytes, bodyBytes(response));
    }

    @Test
    void testGzipUpstreamTransformedBodyDropsEncodingHeader() throws IOException {
        MockServerHttpResponse response = (MockServerHttpResponse) exchange.getResponse();
        response.getHeaders().set(HttpHeaders.CONTENT_ENCODING, "gzip");
        AiResponseTransformerTemplate template = mock(AiResponseTransformerTemplate.class);
        when(template.assembleMessage(exchange)).thenReturn(Mono.just("{\"response\":{\"body\":\"\"}}"));
        ChatClient chatClient = mock(ChatClient.class, RETURNS_DEEP_STUBS);
        // the model echoes the upstream encoding header even though its body is plain text
        when(chatClient.prompt().user(anyString()).stream().content())
                .thenReturn(Flux.just("HTTP/1.1 200 OK\nContent-Type: application/json\nContent-Encoding: gzip\n\n{\"customer\":\"alice\"}"));
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        when(context.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult());
        SpringBeanUtils.getInstance().setApplicationContext(context);
        AiResponseTransformerPlugin.AiResponseTransformerDecorator decorator =
                new AiResponseTransformerPlugin.AiResponseTransformerDecorator(exchange, template, chatClient);

        StepVerifier.create(decorator.writeWith(Mono.just(response.bufferFactory().wrap(gzip("{\"customer\":\"alice\"}")))))
                .verifyComplete();

        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertNull(response.getHeaders().getFirst(HttpHeaders.CONTENT_ENCODING));
        assertEquals("{\"customer\":\"alice\"}", new String(bodyBytes(response), StandardCharsets.UTF_8));
    }

    private static void registerResultBean() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        lenient().when(context.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult());
        SpringBeanUtils.getInstance().setApplicationContext(context);
    }

    private static byte[] gzip(final String value) throws IOException {
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        try (GZIPOutputStream gos = new GZIPOutputStream(bos)) {
            gos.write(value.getBytes(StandardCharsets.UTF_8));
        }
        return bos.toByteArray();
    }

    private static String gunzip(final byte[] bytes) throws IOException {
        try (GZIPInputStream gis = new GZIPInputStream(new ByteArrayInputStream(bytes))) {
            return new String(gis.readAllBytes(), StandardCharsets.UTF_8);
        }
    }

    private static byte[] bodyBytes(final MockServerHttpResponse response) {
        return DataBufferUtils.join(response.getBody()).map(dataBuffer -> {
            byte[] bytes = new byte[dataBuffer.readableByteCount()];
            dataBuffer.read(bytes);
            DataBufferUtils.release(dataBuffer);
            return bytes;
        }).block();
    }

    @Test
    void testExtractBodyFromAiResponse() {
        String aiResponse = "HTTP/1.1 200 OK\nContent-Type: application/json\n\n{\"status\":\"success\"}";
        String body = AiResponseTransformerPlugin.extractBodyFromAiResponse(aiResponse);
        assertEquals("{\"status\":\"success\"}", body);
    }

    @Test
    void testExtractBodyFromAiResponseWithCodeBlock() {
        // Test response wrapped in code blocks
        String aiResponse = "```\nHTTP/1.1 200 OK\nContent-Type: application/json\n\n{\"status\":\"success\"}\n```";
        String body = AiResponseTransformerPlugin.extractBodyFromAiResponse(aiResponse);
        assertEquals("{\"status\":\"success\"}", body);
    }

    @Test
    void testExtractBodyFromAiResponseWithCodeBlockAndExtraContent() {
        // Test response wrapped in code blocks with extra content
        String aiResponse = "Here is the response:\n```\nHTTP/1.1 200 OK\nContent-Type: application/json\n\n{\"status\":\"success\"}\n```\nEnd of response";
        String body = AiResponseTransformerPlugin.extractBodyFromAiResponse(aiResponse);
        assertEquals("{\"status\":\"success\"}", body);
    }

    @Test
    void testExtractBodyFromAiResponseWithJsonOnly() {
        // Test pure JSON response (without HTTP headers)
        String aiResponse = "{\"status\":\"success\",\"data\":{\"message\":\"test\"}}";
        String body = AiResponseTransformerPlugin.extractBodyFromAiResponse(aiResponse);
        assertEquals("{\"status\":\"success\",\"data\":{\"message\":\"test\"}}", body);
    }

    @Test
    void testExtractBodyFromAiResponseWithJsonOnlyInCodeBlock() {
        // Test pure JSON response wrapped in code blocks
        String aiResponse = "```\n{\"status\":\"success\",\"data\":{\"message\":\"test\"}}\n```";
        String body = AiResponseTransformerPlugin.extractBodyFromAiResponse(aiResponse);
        assertEquals("{\"status\":\"success\",\"data\":{\"message\":\"test\"}}", body);
    }

    @Test
    void testExtractHeadersFromAiResponse() {
        String aiResponse = "HTTP/1.1 200 OK\nContent-Type: application/json\nCache-Control: no-cache\n\n{\"status\":\"success\"}";
        HttpHeaders headers = AiResponseTransformerPlugin.extractHeadersFromAiResponse(aiResponse);
        assertEquals("application/json", headers.getFirst("Content-Type"));
        assertEquals("no-cache", headers.getFirst("Cache-Control"));
    }

    @Test
    void testExtractHeadersFromAiResponseWithCodeBlock() {
        // Test header extraction from response wrapped in code blocks
        String aiResponse = "```\nHTTP/1.1 200 OK\nContent-Type: application/json\nCache-Control: no-cache\n\n{\"status\":\"success\"}\n```";
        HttpHeaders headers = AiResponseTransformerPlugin.extractHeadersFromAiResponse(aiResponse);
        assertEquals("application/json", headers.getFirst("Content-Type"));
        assertEquals("no-cache", headers.getFirst("Cache-Control"));
    }

    @Test
    void testExtractBodyFromAiResponseWithNoBody() {
        String aiResponse = "HTTP/1.1 204 No Content\nContent-Type: application/json\n\n";
        String body = AiResponseTransformerPlugin.extractBodyFromAiResponse(aiResponse);
        assertNull(body);
    }

    @Test
    void testExtractHeadersFromAiResponseWithNoHeaders() {
        String aiResponse = "HTTP/1.1 200 OK\n\n{\"status\":\"success\"}";
        HttpHeaders headers = AiResponseTransformerPlugin.extractHeadersFromAiResponse(aiResponse);
        assertNotNull(headers);
        assertEquals(0, headers.size());
    }

    @Test
    void testExtractBodyFromAiResponseWithInvalidJson() {
        // Test invalid JSON format
        String aiResponse = "HTTP/1.1 200 OK\nContent-Type: application/json\n\n{invalid json}";
        String body = AiResponseTransformerPlugin.extractBodyFromAiResponse(aiResponse);
        assertNull(body);
    }

    @Test
    void testExtractBodyFromAiResponseWithEmptyResponse() {
        String aiResponse = "";
        String body = AiResponseTransformerPlugin.extractBodyFromAiResponse(aiResponse);
        assertNull(body);
    }

    @Test
    void testExtractBodyFromAiResponseWithNullResponse() {
        String body = AiResponseTransformerPlugin.extractBodyFromAiResponse(null);
        assertNull(body);
    }

    @Test
    void testExtractStatusFromAiResponse() {
        assertEquals(HttpStatus.NOT_FOUND.value(),
                AiResponseTransformerPlugin.extractStatusFromAiResponse("HTTP/1.1 404 Not Found\nContent-Type: application/json\n\n{}"));
        assertEquals(HttpStatus.NO_CONTENT.value(),
                AiResponseTransformerPlugin.extractStatusFromAiResponse("HTTP/1.1 204 No Content\n\n"));
        assertEquals(HttpStatus.OK.value(),
                AiResponseTransformerPlugin.extractStatusFromAiResponse("HTTP/1.0 200 OK\nContent-Type: application/json\n\n{}"));
    }

    @Test
    void testExtractStatusFromAiResponseWithCodeBlock() {
        assertEquals(HttpStatus.CREATED.value(), AiResponseTransformerPlugin
                .extractStatusFromAiResponse("```\nHTTP/1.1 201 Created\nContent-Type: application/json\n\n{\"id\":1}\n```"));
    }

    @Test
    void testExtractStatusFromAiResponseWithInvalidOrMissingStatusLine() {
        assertNull(AiResponseTransformerPlugin.extractStatusFromAiResponse("HTTP/1.1 999 Not Real\nContent-Type: application/json\n\n{}"));
        assertNull(AiResponseTransformerPlugin.extractStatusFromAiResponse("HTTP/1.1 abc OK\nContent-Type: application/json\n\n{}"));
        assertNull(AiResponseTransformerPlugin.extractStatusFromAiResponse("Content-Type: application/json\n\n{}"));
        assertNull(AiResponseTransformerPlugin.extractStatusFromAiResponse("{\"status\":\"success\"}"));
        assertNull(AiResponseTransformerPlugin.extractStatusFromAiResponse(""));
        assertNull(AiResponseTransformerPlugin.extractStatusFromAiResponse(null));
    }

    @Test
    void testExtractStatusFromAiResponseIgnoresStatusLookingHeaderValue() {
        assertNull(AiResponseTransformerPlugin
                .extractStatusFromAiResponse("Content-Type: application/json\nX-Debug: HTTP/1.1 500 Internal Server Error\n\n{}"));
    }

    @Test
    void testExtractHeadersFromAiResponseWithHttp10StatusLine() {
        HttpHeaders headers = AiResponseTransformerPlugin
                .extractHeadersFromAiResponse("HTTP/1.0 200 OK\nContent-Type: application/json\n\n{\"status\":\"success\"}");
        assertEquals("application/json", headers.getFirst("Content-Type"));
    }

    @Test
    void testExtractHeadersFromAiResponseWithEmptyResponse() {
        String aiResponse = "";
        HttpHeaders headers = AiResponseTransformerPlugin.extractHeadersFromAiResponse(aiResponse);
        assertNotNull(headers);
        assertEquals(0, headers.size());
    }

    @Test
    void testExtractHeadersFromAiResponseWithNullResponse() {
        HttpHeaders headers = AiResponseTransformerPlugin.extractHeadersFromAiResponse(null);
        assertNotNull(headers);
        assertEquals(0, headers.size());
    }

    @Test
    void testExtractBodyFromAiResponseWithArrayJson() {
        // Test JSON array format
        String aiResponse = "HTTP/1.1 200 OK\nContent-Type: application/json\n\n[{\"id\":1,\"name\":\"test\"}]";
        String body = AiResponseTransformerPlugin.extractBodyFromAiResponse(aiResponse);
        assertEquals("[{\"id\":1,\"name\":\"test\"}]", body);
    }

    @Test
    void testExtractBodyFromAiResponseWithArrayJsonInCodeBlock() {
        // Test JSON array wrapped in code blocks
        String aiResponse = "```\nHTTP/1.1 200 OK\nContent-Type: application/json\n\n[{\"id\":1,\"name\":\"test\"}]\n```";
        String body = AiResponseTransformerPlugin.extractBodyFromAiResponse(aiResponse);
        assertEquals("[{\"id\":1,\"name\":\"test\"}]", body);
    }

    @Test
    void testExtractBodyFromAiResponseReusesObjectMapper() {
        // initialize the plugin class (and its static mapper) before the constructor instrumentation is installed
        AiResponseTransformerPlugin.extractBodyFromAiResponse("{\"status\":\"success\"}");

        try (MockedConstruction<ObjectMapper> mockedMappers = mockConstruction(ObjectMapper.class)) {
            String body = AiResponseTransformerPlugin
                    .extractBodyFromAiResponse("HTTP/1.1 200 OK\nContent-Type: application/json\n\n{\"status\":\"success\"}");
            assertEquals("{\"status\":\"success\"}", body);
            assertEquals(0, mockedMappers.constructed().size());
        }
    }
}
