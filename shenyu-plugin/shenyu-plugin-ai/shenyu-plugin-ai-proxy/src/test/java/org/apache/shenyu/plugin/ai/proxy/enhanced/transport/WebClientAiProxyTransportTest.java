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

package org.apache.shenyu.plugin.ai.proxy.enhanced.transport;

import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.junit.jupiter.api.Test;
import org.springframework.core.io.buffer.DefaultDataBufferFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.web.reactive.function.client.ClientRequest;
import org.springframework.web.reactive.function.client.ClientResponse;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;

class WebClientAiProxyTransportTest {

    @Test
    void testExecuteForwardsRequestAndStreamsResponse() {
        final AtomicReference<ClientRequest> capturedRequest = new AtomicReference<>();
        final Flux<String> chunks = Flux.just("data: first\n\n", "data: second\n\n");
        final WebClient webClient = WebClient.builder()
                .exchangeFunction(request -> {
                    capturedRequest.set(request);
                    return Mono.just(ClientResponse.create(HttpStatus.OK)
                            .header(HttpHeaders.CONTENT_TYPE, MediaType.TEXT_EVENT_STREAM_VALUE)
                            .body(chunks.map(chunk -> DefaultDataBufferFactory.sharedInstance.wrap(
                                    chunk.getBytes(StandardCharsets.UTF_8))))
                            .build());
                })
                .build();
        final WebClientAiProxyTransport transport = new WebClientAiProxyTransport(webClient);

        final AiUpstreamResponse response = transport.execute(createRequest()).block();

        assertEquals(HttpStatus.OK.value(), response.getStatusCode());
        assertEquals(MediaType.TEXT_EVENT_STREAM_VALUE,
                response.getHeaders().get(HttpHeaders.CONTENT_TYPE).get(0));
        assertEquals("POST", capturedRequest.get().method().name());
        assertEquals("Bearer test-key", capturedRequest.get().headers().getFirst(HttpHeaders.AUTHORIZATION));
        StepVerifier.create(response.getBody().map(bytes -> new String(bytes, StandardCharsets.UTF_8)))
                .expectNext("data: first\n\n", "data: second\n\n")
                .verifyComplete();
    }

    @Test
    void testExecutePreservesErrorStatusAndBody() {
        final WebClient webClient = WebClient.builder()
                .exchangeFunction(request -> Mono.just(ClientResponse.create(HttpStatus.BAD_REQUEST)
                        .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                        .body("{\"error\":\"bad request\"}")
                        .build()))
                .build();
        final WebClientAiProxyTransport transport = new WebClientAiProxyTransport(webClient);

        final AiUpstreamResponse response = transport.execute(createRequest()).block();

        assertEquals(HttpStatus.BAD_REQUEST.value(), response.getStatusCode());
        StepVerifier.create(response.getBody().map(bytes -> new String(bytes, StandardCharsets.UTF_8)))
                .expectNext("{\"error\":\"bad request\"}")
                .verifyComplete();
    }

    private AiUpstreamRequest createRequest() {
        final AiUpstreamRequest request = new AiUpstreamRequest();
        request.setMethod("POST");
        request.setUri(URI.create("https://api.openai.com/v1/chat/completions"));
        request.setHeaders(Map.of(
                HttpHeaders.AUTHORIZATION, List.of("Bearer test-key"),
                HttpHeaders.CONTENT_TYPE, List.of(MediaType.APPLICATION_JSON_VALUE)));
        request.setBody("{}".getBytes(StandardCharsets.UTF_8));
        return request;
    }
}
