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

package org.apache.shenyu.plugin.cache;

import io.netty.buffer.PooledByteBufAllocator;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.CacheRuleHandle;
import org.apache.shenyu.common.dto.convert.selector.CacheUpstream;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.cache.cache.ApplicationConfigCache;
import org.apache.shenyu.plugin.cache.handler.CachePluginDataHandler;
import org.apache.shenyu.plugin.cache.utils.CacheUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.context.ApplicationContext;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.core.io.buffer.NettyDataBuffer;
import org.springframework.core.io.buffer.NettyDataBufferFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.http.server.reactive.MockServerHttpResponse;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.Disposable;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Verifies that request bodies and non-GET requests bypass response caching.
 */
class CacheRequestEligibilityTest {

    private static final String SELECTOR = "cache-6506-selector";

    private final CachePlugin plugin = new CachePlugin();

    private final SelectorData selector = new SelectorData();

    private final RuleData rule = new RuleData();

    private ICache cache;

    @BeforeEach
    void setUp() {
        selector.setId(SELECTOR);
        selector.setHandle("{\"cacheType\":\"memory\",\"url\":\"memory://eligibility\"}");
        rule.setSelectorId(SELECTOR);
        rule.setId("cache-6506-rule");
        CachePluginDataHandler.CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(rule), new CacheRuleHandle());
        cache = ApplicationConfigCache.getInstance().init(SELECTOR, GsonUtils.getInstance().fromJson(selector.getHandle(), CacheUpstream.class));
        ApplicationContext context = mock(ApplicationContext.class);
        when(context.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult());
        SpringBeanUtils.getInstance().setApplicationContext(context);
    }

    @AfterEach
    void tearDown() {
        ApplicationConfigCache.getInstance().invalidateCache(SELECTOR);
        CachePluginDataHandler.CACHED_HANDLE.get().removeHandle(CacheKeyUtils.INST.getKey(rule));
        SpringBeanUtils.getInstance().setApplicationContext(null);
    }

    @Test
    void testDifferentPostBodiesStayIndependent() {
        for (String body : new String[] {"{\"sku\":\"A\"}", "{\"sku\":\"B\"}"}) {
            MockServerWebExchange exchange = request("POST", body);
            StepVerifier.create(execute(exchange, this::echo)).verifyComplete();
            assertEquals(body, exchange.getResponse().getBodyAsString().block());
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"POST", "PUT", "PATCH", "DELETE", "HEAD", "OPTIONS"})
    void testNonGetRequestsBypassCache(final String method) {
        MockServerWebExchange exchange = request(method, "fresh body");
        seed(exchange, "cached body");

        StepVerifier.create(execute(exchange, this::echo)).verifyComplete();

        assertEquals("fresh body", exchange.getResponse().getBodyAsString().block());
        assertArrayEquals("cached body".getBytes(StandardCharsets.UTF_8), cachedBody(exchange));
    }

    @ParameterizedTest
    @ValueSource(strings = {"POST", "PUT", "PATCH", "DELETE", "HEAD", "OPTIONS"})
    void testNonGetMissDoesNotWriteCache(final String method) {
        MockServerWebExchange exchange = request(method, "fresh body");

        StepVerifier.create(execute(exchange, this::echo)).verifyComplete();

        assertEquals("fresh body", exchange.getResponse().getBodyAsString().block());
        assertNull(cache.getData(CacheUtils.responseKey(exchange, selector.getId(), rule.getId())).block());
        assertNull(cache.getData(CacheUtils.contentTypeKey(exchange)).block());
    }

    @ParameterizedTest
    @ValueSource(strings = {"none", "length", "chunked"})
    void testGetWithBodyBypassesCache(final String framing) {
        NettyDataBuffer first = buffer("first ");
        NettyDataBuffer second = buffer("second");
        MockServerHttpRequest.BodyBuilder builder = MockServerHttpRequest.method(HttpMethod.GET, URI.create("http://gateway.example/quote"));
        if ("length".equals(framing)) {
            builder.header(HttpHeaders.CONTENT_LENGTH, "12");
        } else if ("chunked".equals(framing)) {
            builder.header(HttpHeaders.TRANSFER_ENCODING, "chunked");
        }
        MockServerWebExchange exchange = MockServerWebExchange.from(builder.body(Flux.just(first, second)));
        seed(exchange, "cached body");
        try {
            StepVerifier.create(execute(exchange, this::echo)).verifyComplete();

            assertEquals("first second", exchange.getResponse().getBodyAsString().block());
            assertEquals(0, first.getNativeBuffer().refCnt());
            assertEquals(0, second.getNativeBuffer().refCnt());
            assertArrayEquals("cached body".getBytes(StandardCharsets.UTF_8), cachedBody(exchange));
        } finally {
            release(first);
            release(second);
        }
    }

    @Test
    void testBodylessGetStillCachesAndHits() {
        AtomicInteger calls = new AtomicInteger();
        ShenyuPluginChain chain = downstream -> {
            calls.incrementAndGet();
            downstream.getResponse().getHeaders().setContentType(MediaType.APPLICATION_JSON);
            return downstream.getResponse().writeWith(Mono.just(downstream.getResponse().bufferFactory().wrap("cached".getBytes(StandardCharsets.UTF_8))));
        };
        MockServerWebExchange first = request("GET", null);
        StepVerifier.create(execute(first, chain)).verifyComplete();
        MockServerWebExchange second = request("GET", null);
        StepVerifier.create(execute(second, chain)).verifyComplete();

        assertEquals(1, calls.get());
        assertEquals("cached", first.getResponse().getBodyAsString().block());
        assertEquals("cached", second.getResponse().getBodyAsString().block());
    }

    @Test
    void testBodyReadErrorIsNotHiddenByCacheHit() {
        IllegalStateException failure = new IllegalStateException("request body failed");
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.method(HttpMethod.GET, URI.create("http://gateway.example/quote"))
                .body(Flux.error(failure)));
        seed(exchange, "cached body");

        StepVerifier.create(execute(exchange, downstream -> Mono.error(new AssertionError("Body errors must not execute downstream"))))
                .expectErrorMatches(error -> error == failure).verify();
    }

    @ParameterizedTest
    @ValueSource(strings = {"complete", "error", "cancel"})
    void testPeekedBodyIsReleasedWhenDownstreamDoesNotReadIt(final String termination) {
        NettyDataBuffer first = buffer("request body");
        AtomicBoolean cancelled = new AtomicBoolean();
        AtomicBoolean invoked = new AtomicBoolean();
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.method(HttpMethod.GET, URI.create("http://gateway.example/quote"))
                .body(Flux.<DataBuffer>just(first).concatWith(Flux.never()).doOnCancel(() -> cancelled.set(true))));
        IllegalStateException failure = new IllegalStateException("downstream rejected request");
        ShenyuPluginChain chain = downstream -> {
            invoked.set(true);
            if ("error".equals(termination)) {
                return Mono.error(failure);
            }
            return "cancel".equals(termination) ? Mono.never() : Mono.empty();
        };
        try {
            if ("error".equals(termination)) {
                StepVerifier.create(execute(exchange, chain)).expectErrorMatches(error -> error == failure).verify();
            } else if ("cancel".equals(termination)) {
                Disposable subscription = execute(exchange, chain).subscribe();
                try {
                    assertTrue(invoked.get());
                } finally {
                    subscription.dispose();
                }
            } else {
                StepVerifier.create(execute(exchange, chain)).verifyComplete();
            }
            assertTrue(cancelled.get());
            assertEquals(0, first.getNativeBuffer().refCnt());
        } finally {
            release(first);
        }
    }

    private Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        return plugin.doExecute(exchange, chain, selector, rule);
    }

    private Mono<Void> echo(final ServerWebExchange exchange) {
        return DataBufferUtils.join(exchange.getRequest().getBody()).map(buffer -> {
            byte[] bytes = new byte[buffer.readableByteCount()];
            try {
                buffer.read(bytes);
            } finally {
                DataBufferUtils.release(buffer);
            }
            return bytes;
        }).defaultIfEmpty(new byte[0]).flatMap(bytes -> {
            exchange.getResponse().getHeaders().setContentType(MediaType.APPLICATION_JSON);
            return exchange.getResponse().writeWith(Mono.just(exchange.getResponse().bufferFactory().wrap(bytes)));
        });
    }

    private void seed(final ServerWebExchange exchange, final String body) {
        MockServerHttpResponse response = new MockServerHttpResponse();
        response.getHeaders().setContentType(MediaType.APPLICATION_JSON);
        byte[] packet = CachedResponse.capture(response, body.getBytes(StandardCharsets.UTF_8)).orElseThrow().encode();
        cache.cacheData(CacheUtils.responseKey(exchange, selector.getId(), rule.getId()), packet, 60).block();
    }

    private byte[] cachedBody(final ServerWebExchange exchange) {
        return CachedResponse.decode(cache.getData(CacheUtils.responseKey(exchange, selector.getId(), rule.getId())).block()).orElseThrow().getBody();
    }

    private static MockServerWebExchange request(final String method, final String body) {
        MockServerHttpRequest.BodyBuilder builder = MockServerHttpRequest.method(HttpMethod.valueOf(method), URI.create("http://gateway.example/quote"));
        return MockServerWebExchange.from(Objects.isNull(body) ? builder.build() : builder.body(body));
    }

    private static NettyDataBuffer buffer(final String body) {
        return new NettyDataBufferFactory(PooledByteBufAllocator.DEFAULT).allocateBuffer().write(body.getBytes(StandardCharsets.UTF_8));
    }

    private static void release(final NettyDataBuffer buffer) {
        if (buffer.getNativeBuffer().refCnt() > 0) {
            buffer.release();
        }
    }
}
