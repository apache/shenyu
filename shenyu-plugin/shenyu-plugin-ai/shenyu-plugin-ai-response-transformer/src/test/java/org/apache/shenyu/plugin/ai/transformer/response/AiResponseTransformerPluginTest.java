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
import org.apache.shenyu.plugin.ai.common.spring.ai.AiModelFactory;
import org.apache.shenyu.plugin.ai.common.spring.ai.registry.AiModelFactoryRegistry;
import org.apache.shenyu.plugin.ai.transformer.response.handler.AiResponseTransformerPluginHandler;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.ai.chat.model.ChatModel;
import org.springframework.ai.chat.model.ChatResponse;
import org.springframework.ai.chat.model.Generation;
import org.springframework.ai.chat.prompt.Prompt;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DefaultDataBufferFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.http.server.reactive.MockServerHttpResponse;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.adapter.DefaultServerWebExchange;
import org.springframework.web.server.session.DefaultWebSessionManager;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

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
    private ChatClient chatClient;

    @Mock
    private ShenyuPluginChain chain;

    private AiResponseTransformerPlugin plugin;

    private ServerWebExchange exchange;

    @BeforeEach
    void setUp() {
        plugin = new AiResponseTransformerPlugin(Collections.emptyList(), aiModelFactoryRegistry);
        
        // 创建测试用的请求和响应
        MockServerHttpRequest request = MockServerHttpRequest
                .method(HttpMethod.POST, "/test")
                .header("Content-Type", "application/json")
                .body("{\"test\":\"data\"}");
        
        MockServerHttpResponse response = new MockServerHttpResponse();
        response.setStatusCode(HttpStatus.OK);
        response.getHeaders().setContentType(MediaType.APPLICATION_JSON);
        
        exchange = new DefaultServerWebExchange(
                request, 
                response, 
                new DefaultWebSessionManager()
        );
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
        // 模拟AI模型工厂
        when(aiModelFactoryRegistry.getFactory(any())).thenReturn(aiModelFactory);
        when(aiModelFactory.createAiModel(any())).thenReturn(chatModel);
        
        // 模拟ChatClient
        ChatResponse mockResponse = mock(ChatResponse.class);
        Generation mockGeneration = mock(Generation.class);
        when(mockGeneration.getContent()).thenReturn("{\"status\":\"success\",\"data\":{\"message\":\"transformed\"}}");
        when(mockResponse.getResult()).thenReturn(mockGeneration);
        when(chatClient.prompt()).thenReturn(chatClient);
        when(chatClient.user(anyString())).thenReturn(chatClient);
        when(chatClient.call()).thenReturn(mockResponse);
        
        // 模拟插件链执行
        when(chain.execute(any(ServerWebExchange.class))).thenReturn(Mono.empty());
        
        SelectorData selectorData = mock(SelectorData.class);
        RuleData ruleData = mock(RuleData.class);
        
        // 执行插件
        Mono<Void> result = plugin.doExecute(exchange, chain, selectorData, ruleData);
        
        // 验证执行结果
        StepVerifier.create(result)
                .verifyComplete();
    }

    @Test
    void testExtractBodyFromAiResponse() {
        String aiResponse = "HTTP/1.1 200 OK\nContent-Type: application/json\n\n{\"status\":\"success\"}";
        String body = plugin.extractBodyFromAiResponse(aiResponse);
        assertEquals("{\"status\":\"success\"}", body);
    }

    @Test
    void testExtractHeadersFromAiResponse() {
        String aiResponse = "HTTP/1.1 200 OK\nContent-Type: application/json\nCache-Control: no-cache\n\n{\"status\":\"success\"}";
        HttpHeaders headers = plugin.extractHeadersFromAiResponse(aiResponse);
        assertEquals("application/json", headers.getFirst("Content-Type"));
        assertEquals("no-cache", headers.getFirst("Cache-Control"));
    }

    @Test
    void testExtractBodyFromAiResponseWithNoBody() {
        String aiResponse = "HTTP/1.1 204 No Content\nContent-Type: application/json\n\n";
        String body = plugin.extractBodyFromAiResponse(aiResponse);
        assertEquals("", body);
    }

    @Test
    void testExtractHeadersFromAiResponseWithNoHeaders() {
        String aiResponse = "HTTP/1.1 200 OK\n\n{\"status\":\"success\"}";
        HttpHeaders headers = plugin.extractHeadersFromAiResponse(aiResponse);
        assertNotNull(headers);
        assertEquals(0, headers.size());
    }
} 