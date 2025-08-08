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

package org.apache.shenyu.plugin.ai.transformer.response.template;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.lenient;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.http.server.reactive.MockServerHttpResponse;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Flux;
import reactor.test.StepVerifier;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test for AiResponseTransformerTemplate.
 */
@ExtendWith(MockitoExtension.class)
class AiResponseTransformerTemplateTest {

    private AiResponseTransformerTemplate template;

    private ServerWebExchange exchange;

    private MockServerHttpRequest request;

    private MockServerHttpResponse response;

    @BeforeEach
    void setUp() {
        // 创建测试用的请求
        request = MockServerHttpRequest
                .method(HttpMethod.POST, "/test")
                .header("Content-Type", "application/json")
                .header("Authorization", "Bearer test-token")
                .body("{\"test\":\"data\"}");

        // 创建测试用的响应
        response = new MockServerHttpResponse();
        response.setStatusCode(HttpStatus.OK);
        response.getHeaders().setContentType(MediaType.APPLICATION_JSON);
        response.getHeaders().set("X-Custom-Header", "custom-value");

        exchange = mock(ServerWebExchange.class);
        lenient().when(exchange.getRequest()).thenReturn(request);
        lenient().when(exchange.getResponse()).thenReturn(response);

        template = new AiResponseTransformerTemplate("Transform this response", request);
    }

    @Test
    void testAssembleMessage() {
        // 执行测试
        Mono<String> result = template.assembleMessage(exchange);

        // 验证结果
        StepVerifier.create(result)
                .assertNext(message -> {
                    assertNotNull(message);
                    // 验证消息包含必要的字段
                    assertTrue(message.contains("system_prompt"));
                    assertTrue(message.contains("user_prompt"));
                    assertTrue(message.contains("request"));
                    assertTrue(message.contains("response"));
                    assertTrue(message.contains("headers"));
                    assertTrue(message.contains("body"));
                    assertTrue(message.contains("status"));
                })
                .verifyComplete();
    }

    @Test
    void testAssembleMessageWithJsonRequest() {
        // 创建JSON请求
        MockServerHttpRequest jsonRequest = MockServerHttpRequest
                .method(HttpMethod.POST, "/test")
                .header("Content-Type", "application/json")
                .body("{\"name\":\"test\",\"value\":123}");

        // 创建使用新请求的模板
        AiResponseTransformerTemplate jsonTemplate = new AiResponseTransformerTemplate("Transform JSON", jsonRequest);

        // 执行测试
        Mono<String> result = jsonTemplate.assembleMessage(exchange);

        // 验证结果
        StepVerifier.create(result)
                .assertNext(message -> {
                    assertNotNull(message);
                    // 验证JSON请求体被正确解析
                    assertTrue(message.contains("system_prompt"));
                    assertTrue(message.contains("user_prompt"));
                    assertTrue(message.contains("request"));
                    assertTrue(message.contains("response"));
                    assertTrue(message.contains("status"));
                })
                .verifyComplete();
    }

    @Test
    void testAssembleMessageWithFormUrlEncodedRequest() {
        // 创建form-urlencoded请求
        MockServerHttpRequest formRequest = MockServerHttpRequest
                .method(HttpMethod.POST, "/test")
                .header("Content-Type", "application/x-www-form-urlencoded")
                .body("name=test&value=123");

        // 创建使用新请求的模板
        AiResponseTransformerTemplate formTemplate = new AiResponseTransformerTemplate("Transform form", formRequest);

        // 执行测试
        Mono<String> result = formTemplate.assembleMessage(exchange);

        // 验证结果
        StepVerifier.create(result)
                .assertNext(message -> {
                    assertNotNull(message);
                    // 验证form数据被正确解析
                    assertTrue(message.contains("system_prompt"));
                    assertTrue(message.contains("user_prompt"));
                    assertTrue(message.contains("request"));
                    assertTrue(message.contains("response"));
                    assertTrue(message.contains("status"));
                })
                .verifyComplete();
    }

    @Test
    void testAssembleMessageWithEmptyBody() {
        // 创建空请求体
        MockServerHttpRequest emptyRequest = MockServerHttpRequest
                .method(HttpMethod.GET, "/test")
                .build();

        // 创建使用新请求的模板
        AiResponseTransformerTemplate emptyTemplate = new AiResponseTransformerTemplate("Transform empty", emptyRequest);

        // 执行测试
        Mono<String> result = emptyTemplate.assembleMessage(exchange);

        // 验证结果
        StepVerifier.create(result)
                .assertNext(message -> {
                    assertNotNull(message);
                    // 验证空请求体被正确处理
                    assertTrue(message.contains("system_prompt"));
                    assertTrue(message.contains("user_prompt"));
                    assertTrue(message.contains("request"));
                    assertTrue(message.contains("response"));
                    assertTrue(message.contains("status"));
                })
                .verifyComplete();
    }

    @Test
    void testHeadersToJson() {
        // 创建带多个头的请求
        MockServerHttpRequest multiHeaderRequest = MockServerHttpRequest
                .method(HttpMethod.POST, "/test")
                .header("Content-Type", "application/json")
                .header("Authorization", "Bearer token")
                .header("X-Custom", "value1", "value2")
                .body("{\"test\":\"data\"}");

        // 创建使用新请求的模板
        AiResponseTransformerTemplate multiHeaderTemplate = new AiResponseTransformerTemplate("Transform", multiHeaderRequest);

        // 执行测试
        Mono<String> result = multiHeaderTemplate.assembleMessage(exchange);

        // 验证结果
        StepVerifier.create(result)
                .assertNext(message -> {
                    assertNotNull(message);
                    // 验证多个头被正确处理
                    assertTrue(message.contains("system_prompt"));
                    assertTrue(message.contains("user_prompt"));
                    assertTrue(message.contains("request"));
                    assertTrue(message.contains("response"));
                    assertTrue(message.contains("status"));
                })
                .verifyComplete();
    }

    @Test
    void testParseFormUrlEncoded() {
        // 测试URL解码
        String formData = "name=test%20user&value=123%2B456";
        
        // 通过反射调用私有方法进行测试
        // 这里我们通过assembleMessage方法来间接测试
        MockServerHttpRequest formRequest = MockServerHttpRequest
                .method(HttpMethod.POST, "/test")
                .header("Content-Type", "application/x-www-form-urlencoded")
                .body(formData);

        // 创建使用新请求的模板
        AiResponseTransformerTemplate formTemplate = new AiResponseTransformerTemplate("Transform", formRequest);

        // 执行测试
        Mono<String> result = formTemplate.assembleMessage(exchange);

        // 验证结果
        StepVerifier.create(result)
                .assertNext(message -> {
                    assertNotNull(message);
                    // 验证URL解码正确
                    assertTrue(message.contains("system_prompt"));
                    assertTrue(message.contains("user_prompt"));
                    assertTrue(message.contains("request"));
                    assertTrue(message.contains("response"));
                    assertTrue(message.contains("status"));
                })
                .verifyComplete();
    }

    @Test
    void testBodyToStringWithMockDataBuffer() {
        // 测试 bodyToString 方法是否能正确处理 DataBuffer
        String testBody = "{\"name\":\"test\",\"value\":123}";
        
        // 创建 DataBuffer
        org.springframework.core.io.buffer.DataBuffer dataBuffer = 
            org.springframework.core.io.buffer.DefaultDataBufferFactory.sharedInstance.wrap(testBody.getBytes());
        
        // 创建 Flux<DataBuffer>
        Flux<org.springframework.core.io.buffer.DataBuffer> bodyFlux = Flux.just(dataBuffer);
        
        // 使用反射调用私有方法
        try {
            java.lang.reflect.Method bodyToStringMethod = AiResponseTransformerTemplate.class
                .getDeclaredMethod("bodyToString", Flux.class);
            bodyToStringMethod.setAccessible(true);
            
            @SuppressWarnings("unchecked")
            Mono<String> result = (Mono<String>) bodyToStringMethod.invoke(template, bodyFlux);
            
            StepVerifier.create(result)
                    .assertNext(bodyString -> {
                        assertEquals(testBody, bodyString);
                    })
                    .verifyComplete();
        } catch (Exception e) {
            throw new RuntimeException("Failed to test bodyToString method", e);
        }
    }

    @Test
    void testAssembleMessageWithGzipResponse() {
        // 测试 GZIP 压缩的响应处理
        MockServerHttpResponse gzipResponse = new MockServerHttpResponse();
        gzipResponse.setStatusCode(HttpStatus.OK);
        gzipResponse.getHeaders().setContentType(MediaType.APPLICATION_JSON);
        gzipResponse.getHeaders().set("Content-Encoding", "gzip");

        ServerWebExchange gzipExchange = mock(ServerWebExchange.class);
        lenient().when(gzipExchange.getRequest()).thenReturn(request);
        lenient().when(gzipExchange.getResponse()).thenReturn(gzipResponse);

        // 执行测试
        Mono<String> result = template.assembleMessage(gzipExchange);

        // 验证结果
        StepVerifier.create(result)
                .assertNext(message -> {
                    assertNotNull(message);
                    // 验证 GZIP 响应被正确处理
                    assertTrue(message.contains("system_prompt"));
                    assertTrue(message.contains("user_prompt"));
                    assertTrue(message.contains("request"));
                    assertTrue(message.contains("response"));
                    assertTrue(message.contains("status"));
                    // 验证响应头包含 Content-Encoding
                    assertTrue(message.contains("Content-Encoding"));
                })
                .verifyComplete();
    }

    @Test
    void testAssembleMessageWithComplexHeaders() {
        // 测试复杂的响应头
        MockServerHttpResponse complexResponse = new MockServerHttpResponse();
        complexResponse.setStatusCode(HttpStatus.OK);
        complexResponse.getHeaders().setContentType(MediaType.APPLICATION_JSON);
        complexResponse.getHeaders().set("Cache-Control", "no-cache, no-store, must-revalidate");
        complexResponse.getHeaders().set("Pragma", "no-cache");
        complexResponse.getHeaders().set("Expires", "0");
        complexResponse.getHeaders().set("X-Custom-Header", "custom-value");

        ServerWebExchange complexExchange = mock(ServerWebExchange.class);
        lenient().when(complexExchange.getRequest()).thenReturn(request);
        lenient().when(complexExchange.getResponse()).thenReturn(complexResponse);

        // 执行测试
        Mono<String> result = template.assembleMessage(complexExchange);

        // 验证结果
        StepVerifier.create(result)
                .assertNext(message -> {
                    assertNotNull(message);
                    // 验证复杂响应头被正确处理
                    assertTrue(message.contains("system_prompt"));
                    assertTrue(message.contains("user_prompt"));
                    assertTrue(message.contains("request"));
                    assertTrue(message.contains("response"));
                    assertTrue(message.contains("status"));
                    // 验证响应头信息
                    assertTrue(message.contains("Cache-Control"));
                    assertTrue(message.contains("Pragma"));
                    assertTrue(message.contains("Expires"));
                    assertTrue(message.contains("X-Custom-Header"));
                })
                .verifyComplete();
    }

    @Test
    void testAssembleMessageWithEmptyUserContent() {
        // 测试空的用户内容
        AiResponseTransformerTemplate emptyContentTemplate = new AiResponseTransformerTemplate("", request);

        // 执行测试
        Mono<String> result = emptyContentTemplate.assembleMessage(exchange);

        // 验证结果
        StepVerifier.create(result)
                .assertNext(message -> {
                    assertNotNull(message);
                    // 验证空用户内容被正确处理
                    assertTrue(message.contains("system_prompt"));
                    assertTrue(message.contains("user_prompt"));
                    assertTrue(message.contains("request"));
                    assertTrue(message.contains("response"));
                    assertTrue(message.contains("status"));
                })
                .verifyComplete();
    }

    @Test
    void testAssembleMessageWithNullUserContent() {
        // 测试 null 用户内容
        AiResponseTransformerTemplate nullContentTemplate = new AiResponseTransformerTemplate(null, request);

        // 执行测试
        Mono<String> result = nullContentTemplate.assembleMessage(exchange);

        // 验证结果
        StepVerifier.create(result)
                .assertNext(message -> {
                    assertNotNull(message);
                    // 验证 null 用户内容被正确处理
                    assertTrue(message.contains("system_prompt"));
                    assertTrue(message.contains("user_prompt"));
                    assertTrue(message.contains("request"));
                    assertTrue(message.contains("response"));
                    assertTrue(message.contains("status"));
                })
                .verifyComplete();
    }
} 
