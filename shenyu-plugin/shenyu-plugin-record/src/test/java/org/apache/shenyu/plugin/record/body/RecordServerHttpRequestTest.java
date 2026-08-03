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

import org.apache.shenyu.plugin.record.config.HttpRecordCollectConfig;
import org.apache.shenyu.plugin.record.entity.ShenyuHttpRequestRecord;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DefaultDataBufferFactory;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import reactor.core.publisher.Flux;

import java.nio.charset.StandardCharsets;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

public final class RecordServerHttpRequestTest {

    private DefaultDataBufferFactory bufferFactory;

    @BeforeEach
    public void setUp() {
        bufferFactory = new DefaultDataBufferFactory();
        HttpRecordCollectConfig.INSTANCE.setRecordConfig(new HttpRecordCollectConfig.RecordConfig());
    }

    @Test
    public void testGetBodyWithPostRequest() {
        String jsonBody = "{\"key\":\"value\"}";
        MockServerHttpRequest mockRequest = MockServerHttpRequest.post("http://localhost/test")
                .contentType(org.springframework.http.MediaType.APPLICATION_JSON)
                .body(jsonBody);
        ShenyuHttpRequestRecord record = new ShenyuHttpRequestRecord();
        RecordServerHttpRequest recordRequest =
                new RecordServerHttpRequest(mockRequest, record);

        Flux<DataBuffer> bodyFlux = recordRequest.getBody();
        StringBuilder sb = new StringBuilder();
        bodyFlux.doOnNext(dataBuffer -> {
            byte[] bytes = new byte[dataBuffer.readableByteCount()];
            dataBuffer.read(bytes);
            sb.append(new String(bytes, StandardCharsets.UTF_8));
        }).blockLast();

        assertEquals(jsonBody, sb.toString());
    }

    @Test
    public void testGetBodyTruncatesOversizedBody() {
        HttpRecordCollectConfig.RecordConfig config = new HttpRecordCollectConfig.RecordConfig();
        config.setMaxBodySize(10);
        HttpRecordCollectConfig.INSTANCE.setRecordConfig(config);

        String bodyContent = "this is a very long body that exceeds the max size limit";
        MockServerHttpRequest mockRequest = MockServerHttpRequest.post("http://localhost/test")
                .contentType(org.springframework.http.MediaType.APPLICATION_JSON)
                .body(bodyContent);
        ShenyuHttpRequestRecord record = new ShenyuHttpRequestRecord();
        RecordServerHttpRequest recordRequest =
                new RecordServerHttpRequest(mockRequest, record);

        Flux<DataBuffer> bodyFlux = recordRequest.getBody();
        bodyFlux.doOnNext(DataBuffer::readableByteCount).blockLast();

        assertTrue(record.getRequestBody().startsWith("[TRUNCATED]"));

        HttpRecordCollectConfig.INSTANCE.setRecordConfig(new HttpRecordCollectConfig.RecordConfig());
    }

    @Test
    public void testDelegatePreservesOriginalRequestProperties() {
        MockServerHttpRequest mockRequest = MockServerHttpRequest.get("http://localhost/test?param=value")
                .header("X-Custom", "test-value")
                .build();
        ShenyuHttpRequestRecord record = new ShenyuHttpRequestRecord();
        RecordServerHttpRequest recordRequest =
                new RecordServerHttpRequest(mockRequest, record);

        assertEquals("GET", recordRequest.getMethod().name());
        assertEquals("/test", recordRequest.getURI().getPath());
        assertEquals("param=value", recordRequest.getURI().getQuery());
        assertEquals("test-value", recordRequest.getHeaders().get("X-Custom").get(0));
    }

    @Test
    public void testGetBodyWithEmptyBody() {
        MockServerHttpRequest mockRequest = MockServerHttpRequest.get("http://localhost/test").build();
        ShenyuHttpRequestRecord record = new ShenyuHttpRequestRecord();
        RecordServerHttpRequest recordRequest =
                new RecordServerHttpRequest(mockRequest, record);

        ServerHttpRequest delegate = recordRequest.getDelegate();
        assertEquals(mockRequest, delegate);
    }

    @Test
    public void testGetBodyCapturesRequestBodyViaDoFinally() {
        String bodyContent = "hello";
        MockServerHttpRequest mockRequest = MockServerHttpRequest.post("http://localhost/test")
                .contentType(org.springframework.http.MediaType.APPLICATION_JSON)
                .body(bodyContent);
        ShenyuHttpRequestRecord record = new ShenyuHttpRequestRecord();
        RecordServerHttpRequest recordRequest =
                new RecordServerHttpRequest(mockRequest, record);

        recordRequest.getBody().blockLast();

        String capturedBody = record.getRequestBody();
        assertEquals(bodyContent, capturedBody);
    }
}