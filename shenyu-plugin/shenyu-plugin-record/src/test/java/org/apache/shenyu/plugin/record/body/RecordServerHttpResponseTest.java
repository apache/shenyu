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

package org.apache.shenyu.plugin.record.body;

import org.apache.shenyu.plugin.record.collector.HttpRecordCollector;
import org.apache.shenyu.plugin.record.config.HttpRecordCollectConfig;
import org.apache.shenyu.plugin.record.entity.ShenyuHttpRequestRecord;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DefaultDataBufferFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatusCode;
import org.springframework.http.MediaType;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public final class RecordServerHttpResponseTest {

    private DefaultDataBufferFactory bufferFactory;

    @BeforeEach
    public void setUp() {
        bufferFactory = new DefaultDataBufferFactory();
        HttpRecordCollectConfig.INSTANCE.setRecordConfig(new HttpRecordCollectConfig.RecordConfig());
    }

    @AfterEach
    public void tearDown() {
        HttpRecordCollectConfig.INSTANCE.setRecordConfig(new HttpRecordCollectConfig.RecordConfig());
    }

    @Test
    public void testWriteWithJsonResponseCapturesStatusAndHeaders() {
        ServerHttpResponse mockResponse = mock(ServerHttpResponse.class);
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        when(mockResponse.getHeaders()).thenReturn(headers);
        when(mockResponse.getStatusCode()).thenReturn(HttpStatusCode.valueOf(200));
        when(mockResponse.writeWith(any())).thenAnswer(invocation -> {
            Flux<? extends DataBuffer> body = invocation.getArgument(0);
            return body.then(Mono.empty());
        });

        ServerWebExchange exchange = mock(ServerWebExchange.class);
        when(exchange.getResponse()).thenReturn(mockResponse);

        ShenyuHttpRequestRecord record = new ShenyuHttpRequestRecord();
        TestableRecordServerHttpResponse response = new TestableRecordServerHttpResponse(
                mockResponse, record, exchange);

        String responseBody = "{\"result\":\"ok\"}";
        DataBuffer dataBuffer = bufferFactory.wrap(responseBody.getBytes());
        Mono<Void> result = response.writeWith(Flux.just(dataBuffer));
        StepVerifier.create(result).expectSubscription().verifyComplete();

        assertEquals(200, record.getStatus());
        assertTrue(record.getResponseHeaders().containsKey("Content-Type"));
        assertEquals(responseBody, record.getResponseBody());
        assertEquals("done collect", record.getTaskId());
    }

    @Test
    public void testWriteWithBinaryResponseCollectsWithoutBody() {
        ServerHttpResponse mockResponse = mock(ServerHttpResponse.class);
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.IMAGE_PNG);
        when(mockResponse.getHeaders()).thenReturn(headers);
        when(mockResponse.getStatusCode()).thenReturn(HttpStatusCode.valueOf(200));
        when(mockResponse.writeWith(any())).thenAnswer(invocation -> {
            Flux<? extends DataBuffer> body = invocation.getArgument(0);
            return body.then(Mono.empty());
        });

        ServerWebExchange exchange = mock(ServerWebExchange.class);
        when(exchange.getResponse()).thenReturn(mockResponse);

        ShenyuHttpRequestRecord record = new ShenyuHttpRequestRecord();
        TestableRecordServerHttpResponse response = new TestableRecordServerHttpResponse(
                mockResponse, record, exchange);

        byte[] imageData = new byte[]{(byte) 0x89, (byte) 0x50, (byte) 0x4E, (byte) 0x47};
        DataBuffer dataBuffer = bufferFactory.wrap(imageData);
        Mono<Void> result = response.writeWith(Flux.just(dataBuffer));
        StepVerifier.create(result).expectSubscription().verifyComplete();

        assertEquals(200, record.getStatus());
        assertNull(record.getResponseBody());
        assertEquals("done collect", record.getTaskId());
    }

    @Test
    public void testWriteWithTruncatedBody() {
        HttpRecordCollectConfig.RecordConfig config = new HttpRecordCollectConfig.RecordConfig();
        config.setMaxBodySize(10);
        HttpRecordCollectConfig.INSTANCE.setRecordConfig(config);

        ServerHttpResponse mockResponse = mock(ServerHttpResponse.class);
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        when(mockResponse.getHeaders()).thenReturn(headers);
        when(mockResponse.getStatusCode()).thenReturn(HttpStatusCode.valueOf(200));
        when(mockResponse.writeWith(any())).thenAnswer(invocation -> {
            Flux<? extends DataBuffer> body = invocation.getArgument(0);
            return body.then(Mono.empty());
        });

        ServerWebExchange exchange = mock(ServerWebExchange.class);
        when(exchange.getResponse()).thenReturn(mockResponse);

        ShenyuHttpRequestRecord record = new ShenyuHttpRequestRecord();
        TestableRecordServerHttpResponse response = new TestableRecordServerHttpResponse(
                mockResponse, record, exchange);

        String longBody = "this is a very long response body that exceeds the max size limit";
        DataBuffer dataBuffer = bufferFactory.wrap(longBody.getBytes());
        Mono<Void> result = response.writeWith(Flux.just(dataBuffer));
        StepVerifier.create(result).expectSubscription().verifyComplete();

        assertTrue(record.getResponseBody().startsWith("[TRUNCATED]"));
    }

    private static class TestableRecordServerHttpResponse extends RecordServerHttpResponse {

        TestableRecordServerHttpResponse(final ServerHttpResponse delegate,
                                          final ShenyuHttpRequestRecord record,
                                          final ServerWebExchange exchange) {
            super(delegate, record, new TestHttpRecordCollector(), exchange);
        }
    }

    private static class TestHttpRecordCollector extends HttpRecordCollector {

        @Override
        public void collect(final ShenyuHttpRequestRecord record) {
            record.setTaskId("done collect");
        }
    }
}