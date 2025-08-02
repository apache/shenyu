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

package org.apache.shenyu.plugin.ai.transformer.request.template;

import org.junit.jupiter.api.Test;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AiRequestTransformerTemplateTest {

    @Test
    void testAssembleMessageWithJsonBody() {

        String jsonBody = "{\"key\":\"value\"}";

        ServerHttpRequest request = MockServerHttpRequest.post("/")
                .contentType(MediaType.APPLICATION_JSON)
                .body(jsonBody);

        String userContent = "Test user content";
        AiRequestTransformerTemplate template = new AiRequestTransformerTemplate(userContent, request);

        Mono<String> result = template.assembleMessage();

        StepVerifier.create(result)
                .assertNext(message -> {
                    assertNotNull(message, "Message should not be null");
                    assertTrue(message.contains("system_prompt"), "Message should contain system_prompt");
                    assertTrue(message.contains("user_prompt"), "Message should contain user_prompt");
                    assertTrue(message.contains("key"), "Message should contain JSON body key");
                })
                .verifyComplete();
    }

    @Test
    void testAssembleMessageWithFormUrlEncodedBody() {

        String formBody = "key1=value1&key2=value2";
        String userContent = "Test user content";
        ServerHttpRequest request = MockServerHttpRequest.post("/")
                .contentType(MediaType.APPLICATION_FORM_URLENCODED)
                .body(formBody);

        AiRequestTransformerTemplate template = new AiRequestTransformerTemplate(userContent, request);

        Mono<String> result = template.assembleMessage();

        StepVerifier.create(result)
                .assertNext(message -> {
                    assertNotNull(message, "Message should not be null");
                    assertTrue(message.contains("system_prompt"), "Message should contain system_prompt");
                    assertTrue(message.contains("user_prompt"), "Message should contain user_prompt");
                    assertTrue(message.contains("key1"), "Message should contain form data key1");
                    assertTrue(message.contains("value1"), "Message should contain form data value1");
                })
                .verifyComplete();
    }

    @Test
    void testAssembleMessageWithEmptyBody() {

        String userContent = "Test user content";
        ServerHttpRequest request = MockServerHttpRequest.post("/")
                .contentType(MediaType.APPLICATION_JSON)
                .body("");
        AiRequestTransformerTemplate template = new AiRequestTransformerTemplate(userContent, request);

        Mono<String> result = template.assembleMessage();

        StepVerifier.create(result)
                .assertNext(message -> {
                    assertNotNull(message, "Message should not be null");
                    assertTrue(message.contains("system_prompt"), "Message should contain system_prompt");
                    assertTrue(message.contains("user_prompt"), "Message should contain user_prompt");
                })
                .verifyComplete();
    }

    @Test
    void testAssembleMessageWithHeaders() {

        String userContent = "Test user content";
        ServerHttpRequest request = MockServerHttpRequest.post("/")
                .header(HttpHeaders.AUTHORIZATION, "Bearer token")
                .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .body("");

        AiRequestTransformerTemplate template = new AiRequestTransformerTemplate(userContent, request);

        Mono<String> result = template.assembleMessage();

        StepVerifier.create(result)
                .assertNext(message -> {
                    assertNotNull(message, "Message should not be null");
                    assertTrue(message.contains("system_prompt"), "Message should contain system_prompt");
                    assertTrue(message.contains("user_prompt"), "Message should contain user_prompt");
                    assertTrue(message.contains("Authorization"), "Message should contain Authorization header");
                    assertTrue(message.contains("Bearer token"), "Message should contain Authorization value");
                })
                .verifyComplete();
    }
}
