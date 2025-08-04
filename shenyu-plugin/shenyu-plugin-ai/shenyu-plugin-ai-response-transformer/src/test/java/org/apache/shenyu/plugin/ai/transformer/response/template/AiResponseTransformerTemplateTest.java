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
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DefaultDataBufferFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.http.server.reactive.MockServerHttpResponse;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.adapter.DefaultServerWebExchange;
import org.springframework.web.server.session.DefaultWebSessionManager;
import reactor.core.publisher.Flux;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

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

        exchange = new DefaultServerWebExchange(
                request,
                response,
                new DefaultWebSessionManager()
        );

        template = new AiResponseTransformerTemplate("Transform this response", request);
    }

    @Test
    void testAssembleMessage() {
        // 执行测试
        Flux<String> result = template.assembleMessage(exchange);

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

        AiResponseTransformerTemplate jsonTemplate = new AiResponseTransformerTemplate("Transform JSON", jsonRequest);

        // 执行测试
        Flux<String> result = jsonTemplate.assembleMessage(exchange);

        // 验证结果
        StepVerifier.create(result)
                .assertNext(message -> {
                    assertNotNull(message);
                    // 验证JSON请求体被正确解析
                    assertTrue(message.contains("\"name\":\"test\""));
                    assertTrue(message.contains("\"value\":123"));
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

        AiResponseTransformerTemplate formTemplate = new AiResponseTransformerTemplate("Transform form", formRequest);

        // 执行测试
        Flux<String> result = formTemplate.assembleMessage(exchange);

        // 验证结果
        StepVerifier.create(result)
                .assertNext(message -> {
                    assertNotNull(message);
                    // 验证form数据被正确解析
                    assertTrue(message.contains("\"name\":\"test\""));
                    assertTrue(message.contains("\"value\":\"123\""));
                })
                .verifyComplete();
    }

    @Test
    void testAssembleMessageWithEmptyBody() {
        // 创建空请求体
        MockServerHttpRequest emptyRequest = MockServerHttpRequest
                .method(HttpMethod.GET, "/test")
                .build();

        AiResponseTransformerTemplate emptyTemplate = new AiResponseTransformerTemplate("Transform empty", emptyRequest);

        // 执行测试
        Flux<String> result = emptyTemplate.assembleMessage(exchange);

        // 验证结果
        StepVerifier.create(result)
                .assertNext(message -> {
                    assertNotNull(message);
                    // 验证空请求体被正确处理
                    assertTrue(message.contains("\"body\":\"\""));
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

        AiResponseTransformerTemplate multiHeaderTemplate = new AiResponseTransformerTemplate("Transform", multiHeaderRequest);

        // 执行测试
        Flux<String> result = multiHeaderTemplate.assembleMessage(exchange);

        // 验证结果
        StepVerifier.create(result)
                .assertNext(message -> {
                    assertNotNull(message);
                    // 验证多个头被正确处理
                    assertTrue(message.contains("Content-Type"));
                    assertTrue(message.contains("Authorization"));
                    assertTrue(message.contains("X-Custom"));
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

        AiResponseTransformerTemplate formTemplate = new AiResponseTransformerTemplate("Transform", formRequest);

        // 执行测试
        Flux<String> result = formTemplate.assembleMessage(exchange);

        // 验证结果
        StepVerifier.create(result)
                .assertNext(message -> {
                    assertNotNull(message);
                    // 验证URL解码正确
                    assertTrue(message.contains("test user")); // 空格被解码
                    assertTrue(message.contains("123+456")); // +被解码
                })
                .verifyComplete();
    }

    private void assertTrue(boolean condition) {
        if (!condition) {
            throw new AssertionError("Expected true but was false");
        }
    }
} 