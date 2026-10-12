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

package org.apache.shenyu.plugin.base.support;

import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.core.io.buffer.DataBufferFactory;
import org.springframework.core.io.buffer.PooledDataBuffer;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test cases for ResponseDecorator.
 */
public final class ResponseDecoratorTest {

    @BeforeEach
    public void setUp() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        when(context.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult());
        SpringBeanUtils.getInstance().setApplicationContext(context);
    }

    @Test
    public void testJoinedBufferIsReleasedOnce() {
        byte[] content = "body".getBytes(StandardCharsets.UTF_8);
        PooledDataBuffer buffer = mock(PooledDataBuffer.class);
        DataBufferFactory bufferFactory = mock(DataBufferFactory.class);
        when(buffer.factory()).thenReturn(bufferFactory);
        when(bufferFactory.join(any())).thenReturn(buffer);
        when(buffer.readableByteCount()).thenReturn(content.length);
        when(buffer.read(any(byte[].class))).thenAnswer(invocation -> {
            System.arraycopy(content, 0, invocation.getArgument(0), 0, content.length);
            return buffer;
        });
        when(buffer.isAllocated()).thenReturn(true);
        when(buffer.release()).thenReturn(true);
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/").build());

        StepVerifier.create(new ResponseDecorator(exchange, String::toUpperCase).writeWith(Mono.just(buffer)))
                .verifyComplete();

        verify(buffer, times(1)).release();
    }
}
