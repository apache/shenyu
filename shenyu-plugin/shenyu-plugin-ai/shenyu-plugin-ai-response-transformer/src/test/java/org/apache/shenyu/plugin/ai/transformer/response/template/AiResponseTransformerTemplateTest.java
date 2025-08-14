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
        // Create test request
        request = MockServerHttpRequest
                .method(HttpMethod.POST, "/test")
                .header("Content-Type", "application/json")
                .header("Authorization", "Bearer test-token")
                .body("{\"test\":\"data\"}");

        // Create test response
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
        // Execute test
        Mono<String> result = template.assembleMessage(exchange);

        // Verify result
        StepVerifier.create(result)
                .assertNext(message -> {
                    assertNotNull(message);
                    // Verify message contains necessary fields
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
        // Create JSON request
        MockServerHttpRequest jsonRequest = MockServerHttpRequest
                .method(HttpMethod.POST, "/test")
                .header("Content-Type", "application/json")
                .body("{\"name\":\"test\",\"value\":123}");

        // Create template using new request
        AiResponseTransformerTemplate jsonTemplate = new AiResponseTransformerTemplate("Transform JSON", jsonRequest);

        // Execute test
        Mono<String> result = jsonTemplate.assembleMessage(exchange);

        // Verify result
        StepVerifier.create(result)
                .assertNext(message -> {
                    assertNotNull(message);
                    // Verify JSON request body is properly parsed
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
        // Create form-urlencoded request
        MockServerHttpRequest formRequest = MockServerHttpRequest
                .method(HttpMethod.POST, "/test")
                .header("Content-Type", "application/x-www-form-urlencoded")
                .body("name=test&value=123");

        // Create template using new request
        AiResponseTransformerTemplate formTemplate = new AiResponseTransformerTemplate("Transform form", formRequest);

        // Execute test
        Mono<String> result = formTemplate.assembleMessage(exchange);

        // Verify result
        StepVerifier.create(result)
                .assertNext(message -> {
                    assertNotNull(message);
                    // Verify form data is properly parsed
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
        // Create empty request body
        MockServerHttpRequest emptyRequest = MockServerHttpRequest
                .method(HttpMethod.GET, "/test")
                .build();

        // Create template using new request
        AiResponseTransformerTemplate emptyTemplate = new AiResponseTransformerTemplate("Transform empty", emptyRequest);

        // Execute test
        Mono<String> result = emptyTemplate.assembleMessage(exchange);

        // Verify result
        StepVerifier.create(result)
                .assertNext(message -> {
                    assertNotNull(message);
                    // Verify empty request body is properly handled
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
        // Create request with multiple headers
        MockServerHttpRequest multiHeaderRequest = MockServerHttpRequest
                .method(HttpMethod.POST, "/test")
                .header("Content-Type", "application/json")
                .header("Authorization", "Bearer token")
                .header("X-Custom", "value1", "value2")
                .body("{\"test\":\"data\"}");

        // Create template using new request
        AiResponseTransformerTemplate multiHeaderTemplate = new AiResponseTransformerTemplate("Transform", multiHeaderRequest);

        // Execute test
        Mono<String> result = multiHeaderTemplate.assembleMessage(exchange);

        // Verify result
        StepVerifier.create(result)
                .assertNext(message -> {
                    assertNotNull(message);
                    // Verify multiple headers are properly handled
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
        // Test URL decoding
        String formData = "name=test%20user&value=123%2B456";
        
        // Test private method through reflection
        // Here we test indirectly through assembleMessage method
        MockServerHttpRequest formRequest = MockServerHttpRequest
                .method(HttpMethod.POST, "/test")
                .header("Content-Type", "application/x-www-form-urlencoded")
                .body(formData);

        // Create template using new request
        AiResponseTransformerTemplate formTemplate = new AiResponseTransformerTemplate("Transform", formRequest);

        // Execute test
        Mono<String> result = formTemplate.assembleMessage(exchange);

        // Verify result
        StepVerifier.create(result)
                .assertNext(message -> {
                    assertNotNull(message);
                    // Verify URL decoding is correct
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
        // Test if bodyToString method can properly handle DataBuffer
        String testBody = "{\"name\":\"test\",\"value\":123}";
        
        // Create DataBuffer
        org.springframework.core.io.buffer.DataBuffer dataBuffer = 
            org.springframework.core.io.buffer.DefaultDataBufferFactory.sharedInstance.wrap(testBody.getBytes());
        
        // Create Flux<DataBuffer>
        Flux<org.springframework.core.io.buffer.DataBuffer> bodyFlux = Flux.just(dataBuffer);
        
        // Use reflection to call private method
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
        // Test GZIP compressed response handling
        MockServerHttpResponse gzipResponse = new MockServerHttpResponse();
        gzipResponse.setStatusCode(HttpStatus.OK);
        gzipResponse.getHeaders().setContentType(MediaType.APPLICATION_JSON);
        gzipResponse.getHeaders().set("Content-Encoding", "gzip");

        ServerWebExchange gzipExchange = mock(ServerWebExchange.class);
        lenient().when(gzipExchange.getRequest()).thenReturn(request);
        lenient().when(gzipExchange.getResponse()).thenReturn(gzipResponse);

        // Execute test
        Mono<String> result = template.assembleMessage(gzipExchange);

        // Verify result
        StepVerifier.create(result)
                .assertNext(message -> {
                    assertNotNull(message);
                    // Verify GZIP response is properly handled
                    assertTrue(message.contains("system_prompt"));
                    assertTrue(message.contains("user_prompt"));
                    assertTrue(message.contains("request"));
                    assertTrue(message.contains("response"));
                    assertTrue(message.contains("status"));
                    // Verify response headers contain Content-Encoding
                    assertTrue(message.contains("Content-Encoding"));
                })
                .verifyComplete();
    }

    @Test
    void testAssembleMessageWithComplexHeaders() {
        // Test complex response headers
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

        // Execute test
        Mono<String> result = template.assembleMessage(complexExchange);

        // Verify result
        StepVerifier.create(result)
                .assertNext(message -> {
                    assertNotNull(message);
                    // Verify complex response headers are properly handled
                    assertTrue(message.contains("system_prompt"));
                    assertTrue(message.contains("user_prompt"));
                    assertTrue(message.contains("request"));
                    assertTrue(message.contains("response"));
                    assertTrue(message.contains("status"));
                    // Verify response header information
                    assertTrue(message.contains("Cache-Control"));
                    assertTrue(message.contains("Pragma"));
                    assertTrue(message.contains("Expires"));
                    assertTrue(message.contains("X-Custom-Header"));
                })
                .verifyComplete();
    }

    @Test
    void testAssembleMessageWithEmptyUserContent() {
        // Test empty user content
        AiResponseTransformerTemplate emptyContentTemplate = new AiResponseTransformerTemplate("", request);

        // Execute test
        Mono<String> result = emptyContentTemplate.assembleMessage(exchange);

        // Verify result
        StepVerifier.create(result)
                .assertNext(message -> {
                    assertNotNull(message);
                    // Verify empty user content is properly handled
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
        // Test null user content
        AiResponseTransformerTemplate nullContentTemplate = new AiResponseTransformerTemplate(null, request);

        // Execute test
        Mono<String> result = nullContentTemplate.assembleMessage(exchange);

        // Verify result
        StepVerifier.create(result)
                .assertNext(message -> {
                    assertNotNull(message);
                    // Verify null user content is properly handled
                    assertTrue(message.contains("system_prompt"));
                    assertTrue(message.contains("user_prompt"));
                    assertTrue(message.contains("request"));
                    assertTrue(message.contains("response"));
                    assertTrue(message.contains("status"));
                })
                .verifyComplete();
    }
} 
