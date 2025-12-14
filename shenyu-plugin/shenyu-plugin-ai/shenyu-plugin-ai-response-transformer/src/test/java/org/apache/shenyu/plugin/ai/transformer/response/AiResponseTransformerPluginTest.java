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
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.ai.chat.model.ChatModel;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.http.server.reactive.MockServerHttpResponse;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
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
        SelectorData selectorData = mock(SelectorData.class);
        RuleData ruleData = mock(RuleData.class);
        
        // Mock the factory registry and factory
        lenient().when(aiModelFactoryRegistry.getFactory(AiModelProviderEnum.getByName("test-provider")))
                .thenReturn(aiModelFactory);
        lenient().when(aiModelFactory.createAiModel(any(AiCommonConfig.class)))
                .thenReturn(chatModel);
        
        // Mock exchange mutate for the full plugin flow
        ServerWebExchange.Builder builder = mock(ServerWebExchange.Builder.class);
        lenient().when(exchange.mutate()).thenReturn(builder);
        lenient().when(builder.response(any())).thenReturn(builder);
        lenient().when(builder.build()).thenReturn(exchange);
        
        // Mock the chain to return empty Mono
        when(chain.execute(exchange)).thenReturn(Mono.empty());

        // Execute plugin - this should succeed with proper configuration
        Mono<Void> result = plugin.doExecute(exchange, chain, selectorData, ruleData);
        
        // Verify execution result
        StepVerifier.create(result)
                .verifyComplete();
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
}
