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

package org.apache.shenyu.plugin.param.mapping.strategy;

import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.JsonPath;
import io.netty.buffer.ByteBuf;
import io.netty.buffer.EmptyByteBuf;
import io.netty.buffer.PooledByteBufAllocator;
import org.apache.shenyu.common.dto.convert.rule.impl.ParamMappingRuleHandle;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.core.io.buffer.DataBufferFactory;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.core.io.buffer.DefaultDataBufferFactory;
import org.springframework.core.io.buffer.NettyDataBuffer;
import org.springframework.core.io.buffer.NettyDataBufferFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.codec.ServerCodecConfigurer;
import org.springframework.http.server.reactive.ServerHttpResponseDecorator;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.Disposable;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Pooled-buffer ownership of JSON parameter mapping output.
 */
public final class JsonOperatorBodyLifecycleTest {

    private final List<ByteBuf> allocations = new ArrayList<>();

    private final NettyDataBufferFactory factory = new NettyDataBufferFactory(PooledByteBufAllocator.DEFAULT) {
        @Override
        public NettyDataBuffer allocateBuffer(final int initialCapacity) {
            NettyDataBuffer buffer = super.allocateBuffer(initialCapacity);
            allocations.add(buffer.getNativeBuffer());
            return buffer;
        }
    };

    private final JsonOperator operator = new JsonOperator(ServerCodecConfigurer.create().getReaders());

    @AfterEach
    public void releaseTestBuffers() {
        allocations.stream().filter(buffer -> buffer.refCnt() > 0 && !(buffer instanceof EmptyByteBuf)).forEach(ByteBuf::release);
    }

    @Test
    public void testShortCircuitDoesNotAllocateMappedBody() {
        StepVerifier.create(operator.apply(exchange(StandardCharsets.UTF_8), downstream -> {
            downstream.getResponse().setStatusCode(HttpStatus.FORBIDDEN);
            return downstream.getResponse().setComplete();
        }, rule())).verifyComplete();

        assertBuffersReleased();
        assertTrue(allocations.isEmpty(), "An unread mapped body must not allocate a pooled buffer");
    }

    @Test
    public void testCancellationBeforeBodyDemandDoesNotAllocate() {
        ServerWebExchange downstream = mapRequest(operator);

        StepVerifier.create(downstream.getRequest().getBody(), 0).thenCancel().verify();

        assertBuffersReleased();
        assertTrue(allocations.isEmpty());
    }

    @Test
    public void testCancellationWhileDownstreamDoesNotReadBody() {
        AtomicInteger calls = new AtomicInteger();
        AtomicBoolean cancelled = new AtomicBoolean();
        Disposable subscription = operator.apply(exchange(StandardCharsets.UTF_8), downstream -> {
            calls.incrementAndGet();
            return Mono.<Void>never().doOnCancel(() -> cancelled.set(true));
        }, rule()).subscribe();
        subscription.dispose();

        assertEquals(1, calls.get());
        assertTrue(cancelled.get());
        assertBuffersReleased();
        assertTrue(allocations.isEmpty());
    }

    @Test
    public void testCancellationAfterConsumptionReleasesBuffer() {
        ServerWebExchange downstream = mapRequest(operator);
        StepVerifier.create(downstream.getRequest().getBody(), 0)
                .thenRequest(1)
                .assertNext(buffer -> {
                    try {
                        assertEquals("shenyu", JsonPath.parse(buffer.toString(StandardCharsets.UTF_8)).read("$.added"));
                    } finally {
                        DataBufferUtils.release(buffer);
                    }
                }).thenCancel().verify();

        assertEquals(1, allocations.size());
        assertBuffersReleased();
    }

    @ParameterizedTest
    @ValueSource(strings = {"UTF-8", "UTF-16LE", "ISO-8859-1"})
    public void testForwardingPreservesMappingCharsetAndHeaders(final String charsetName) {
        Charset charset = Charset.forName(charsetName);
        ServerWebExchange original = exchange(charset);
        StepVerifier.create(operator.apply(original, downstream -> readBody(downstream, charset).doOnNext(body -> {
            DocumentContext mapped = JsonPath.parse(body);
            assertEquals("café", mapped.read("$.realName"));
            assertEquals("shenyu", mapped.read("$.added"));
            assertFalse(body.contains("age"));
            assertEquals(original.getRequest().getHeaders().getContentType(), downstream.getRequest().getHeaders().getContentType());
            assertEquals("kept", downstream.getRequest().getHeaders().getFirst("X-Test"));
            long contentLength = downstream.getRequest().getHeaders().getContentLength();
            if (contentLength >= 0) {
                assertEquals(body.getBytes(charset).length, contentLength);
            } else {
                assertEquals("chunked", downstream.getRequest().getHeaders().getFirst(HttpHeaders.TRANSFER_ENCODING));
            }
        }).then(), rule())).verifyComplete();

        assertEquals(1, allocations.size());
        assertBuffersReleased();
    }

    @Test
    public void testRepeatedReadsUseFreshBuffersWithoutRepeatingMapping() {
        AtomicInteger mappings = new AtomicInteger();
        JsonOperator countingOperator = new JsonOperator(ServerCodecConfigurer.create().getReaders()) {
            @Override
            public String operation(final String jsonValue, final ParamMappingRuleHandle handle) {
                mappings.incrementAndGet();
                return super.operation(jsonValue, handle);
            }
        };
        ServerWebExchange downstream = mapRequest(countingOperator);
        String first = readBody(downstream, StandardCharsets.UTF_8).block(Duration.ofSeconds(5));
        String second = readBody(downstream, StandardCharsets.UTF_8).block(Duration.ofSeconds(5));

        assertEquals(first, second);
        assertEquals("shenyu", JsonPath.parse(second).read("$.added"));
        assertEquals(1, mappings.get());
        assertEquals(2, allocations.size());
        assertBuffersReleased();
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    public void testDownstreamErrorPreservedAndBuffersReleased(final boolean readFirst) {
        IllegalStateException failure = new IllegalStateException("downstream failed");
        StepVerifier.create(operator.apply(exchange(StandardCharsets.UTF_8), downstream -> {
            Mono<Void> result = Mono.error(failure);
            return readFirst ? readBody(downstream, StandardCharsets.UTF_8).then(result) : result;
        }, rule())).expectErrorSatisfies(error -> assertSame(failure, error)).verify();

        assertBuffersReleased();
    }

    private ServerWebExchange mapRequest(final JsonOperator jsonOperator) {
        AtomicReference<ServerWebExchange> mapped = new AtomicReference<>();
        StepVerifier.create(jsonOperator.apply(exchange(StandardCharsets.UTF_8), downstream -> {
            mapped.set(downstream);
            return Mono.empty();
        }, rule())).verifyComplete();
        return mapped.get();
    }

    private ServerWebExchange exchange(final Charset charset) {
        byte[] bytes = "{\"name\":\"café\",\"age\":18}".getBytes(charset);
        MockServerWebExchange original = MockServerWebExchange.from(MockServerHttpRequest.post("/mapped")
                .contentType(new MediaType("application", "json", charset)).contentLength(bytes.length).header("X-Test", "kept")
                .body(Flux.just(new DefaultDataBufferFactory().wrap(bytes))));
        return original.mutate().response(new ServerHttpResponseDecorator(original.getResponse()) {
            @Override
            public DataBufferFactory bufferFactory() {
                return factory;
            }
        }).build();
    }

    private ParamMappingRuleHandle rule() {
        ParamMappingRuleHandle.ParamMapInfo add = new ParamMappingRuleHandle.ParamMapInfo();
        add.setPath("$");
        add.setKey("added");
        add.setValue("shenyu");
        ParamMappingRuleHandle.ParamMapInfo rename = new ParamMappingRuleHandle.ParamMapInfo();
        rename.setPath("$");
        rename.setKey("name");
        rename.setValue("realName");
        ParamMappingRuleHandle handle = new ParamMappingRuleHandle();
        handle.setAddParameterKeys(Collections.singletonList(add));
        handle.setReplaceParameterKeys(Collections.singletonList(rename));
        handle.setRemoveParameterKeys(Collections.singleton("$.age"));
        return handle;
    }

    private Mono<String> readBody(final ServerWebExchange exchange, final Charset charset) {
        return DataBufferUtils.join(exchange.getRequest().getBody()).map(buffer -> {
            try {
                return buffer.toString(charset);
            } finally {
                DataBufferUtils.release(buffer);
            }
        });
    }

    private void assertBuffersReleased() {
        allocations.stream().filter(buffer -> !(buffer instanceof EmptyByteBuf)).forEach(buffer -> assertEquals(0, buffer.refCnt()));
    }
}
