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

package org.apache.shenyu.plugin.shadow.dispatch;

import io.netty.buffer.PooledByteBufAllocator;
import org.apache.shenyu.plugin.shadow.capture.ShadowBodyCapture;
import org.junit.jupiter.api.Test;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.NettyDataBufferFactory;
import org.springframework.core.io.buffer.PooledDataBuffer;
import org.springframework.web.reactive.function.client.ClientResponse;
import reactor.core.publisher.Flux;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * test case for {@link ShadowDispatcher#consume(ClientResponse, ShadowBodyCapture)}.
 */
public final class ShadowResponseConsumeTest {

    @Test
    public void testCopiesAndReleasesEveryBuffer() {
        NettyDataBufferFactory factory = new NettyDataBufferFactory(PooledByteBufAllocator.DEFAULT);
        DataBuffer first = factory.wrap("pay".getBytes(StandardCharsets.UTF_8));
        DataBuffer second = factory.wrap("load".getBytes(StandardCharsets.UTF_8));
        ClientResponse response = mock(ClientResponse.class);
        when(response.bodyToFlux(DataBuffer.class)).thenReturn(Flux.just(first, second));
        ShadowBodyCapture capture = new ShadowBodyCapture(64);

        StepVerifier.create(ShadowDispatcher.consume(response, capture)).verifyComplete();

        assertEquals("payload", new String(capture.bytes(), StandardCharsets.UTF_8));
        assertFalse(((PooledDataBuffer) first).isAllocated());
        assertFalse(((PooledDataBuffer) second).isAllocated());
    }
}
