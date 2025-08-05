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
import org.apache.shenyu.plugin.ai.common.spring.ai.registry.AiModelFactoryRegistry;
import org.apache.shenyu.plugin.ai.transformer.response.cache.ChatClientCache;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.http.server.reactive.MockServerHttpResponse;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.adapter.DefaultServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
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
        // 简化测试，只测试基本功能
        SelectorData selectorData = mock(SelectorData.class);
        RuleData ruleData = mock(RuleData.class);
        
        // 配置chain mock
        when(chain.execute(exchange)).thenReturn(Mono.empty());
        
        // 执行插件
        Mono<Void> result = plugin.doExecute(exchange, chain, selectorData, ruleData);
        
        // 验证执行结果
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
    void testExtractHeadersFromAiResponse() {
        String aiResponse = "HTTP/1.1 200 OK\nContent-Type: application/json\nCache-Control: no-cache\n\n{\"status\":\"success\"}";
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
} 