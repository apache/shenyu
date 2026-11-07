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

package org.apache.shenyu.plugin.httpclient;

import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.locks.LockSupport;
import java.util.function.IntConsumer;
import java.util.function.IntFunction;

import io.netty.buffer.PooledByteBufAllocator;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.RetryEnum;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.RequestUrlUtils;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.utils.LoadbalancerUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.core.io.buffer.NettyDataBuffer;
import org.springframework.core.io.buffer.NettyDataBufferFactory;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpRequestDecorator;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.Disposable;
import reactor.core.publisher.Flux;
import reactor.core.publisher.FluxSink;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;
import reactor.util.context.Context;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

/**
 * Tests the lifecycle of the request-body aggregation used for retry replay.
 *
 * <p>The body publisher must be subscribed exactly once, its pooled buffers must be released when
 * the logical request is cancelled, and retries must still receive the complete body after an
 * individual attempt timed out.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class RequestBodyAggregationCancellationTest {

    private static final Duration VERIFY_TIMEOUT = Duration.ofSeconds(10);

    private ShenyuPluginChain chain;

    private NettyDataBufferFactory bufferFactory;

    @BeforeEach
    void setUp() {
        final ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setApplicationContext(context);
        when(context.getBean(ShenyuResult.class)).thenReturn(mock(ShenyuResult.class));
        chain = mock(ShenyuPluginChain.class);
        when(chain.execute(any())).thenReturn(Mono.empty());
        bufferFactory = new NettyDataBufferFactory(PooledByteBufAllocator.DEFAULT);
    }

    @Test
    void cancelsBodyAggregationBeforeFirstChunk() {
        final AtomicBoolean sourceSubscribed = new AtomicBoolean();
        final AtomicBoolean sourceCancelled = new AtomicBoolean();
        final Flux<DataBuffer> body = Flux.create(sink -> {
            sourceSubscribed.set(true);
            sink.onCancel(() -> sourceCancelled.set(true));
        });
        final RecordingPlugin plugin = new RecordingPlugin(attempt -> Mono.just("success"));
        final ServerWebExchange exchange = createExchange(body, 30000L);

        final Disposable subscription = plugin.execute(exchange, chain).subscribe();

        assertTrue(sourceSubscribed.get(), "the aggregation should subscribe to the request body");
        subscription.dispose();
        assertTrue(sourceCancelled.get(), "logical cancellation should cancel the request body publisher");
    }

    @Test
    void doesNotAggregateBodyWithoutRetry() {
        final NettyDataBuffer chunk = pooledBuffer("abc");
        final RecordingPlugin plugin = new RecordingPlugin(attempt -> Mono.just("success"));
        final ServerWebExchange exchange = createExchange(Flux.just(chunk), 30000L);
        exchange.getAttributes().put(Constants.HTTP_RETRY, 0);

        StepVerifier.create(plugin.execute(exchange, chain))
                .expectComplete()
                .verify(VERIFY_TIMEOUT);

        assertEquals(Collections.singletonList("abc"), plugin.getCapturedBodies());
        assertNull(exchange.getAttribute(Constants.CACHED_REQUEST_BODY),
                "body must not be aggregated when retry is disabled");
        awaitRefCnt(chunk, 0);
    }

    @Test
    void preservesSubscriberContextWhenRetryIsEnabled() {
        assertSubscriberContextVisible(1);
    }

    @Test
    void preservesSubscriberContextWhenRetryIsDisabled() {
        assertSubscriberContextVisible(0);
    }

    private void assertSubscriberContextVisible(final int retryTimes) {
        final AtomicReference<String> observedContext = new AtomicReference<>();
        final NettyDataBuffer chunk = pooledBuffer("abc");
        final Flux<DataBuffer> body = Flux.deferContextual(contextView -> {
            observedContext.set(contextView.get("review-body-context"));
            return Flux.just(chunk);
        });
        final RecordingPlugin plugin = new RecordingPlugin(attempt -> Mono.just("success"));
        final ServerWebExchange exchange = createExchange(body, 30000L);
        exchange.getAttributes().put(Constants.HTTP_RETRY, retryTimes);

        StepVerifier.create(plugin.execute(exchange, chain)
                        .contextWrite(Context.of("review-body-context", "tenant-A")))
                .expectComplete()
                .verify(VERIFY_TIMEOUT);

        assertEquals("tenant-A", observedContext.get());
        awaitRefCnt(chunk, 0);
    }

    @Test
    void releasesPartiallyAggregatedPooledBufferOnCancellation() {
        final NettyDataBuffer chunk = pooledBuffer("abc");
        final AtomicBoolean sourceCancelled = new AtomicBoolean();
        final Flux<DataBuffer> body = Flux.create(sink -> {
            sink.onCancel(() -> sourceCancelled.set(true));
            sink.next(chunk);
        });
        final RecordingPlugin plugin = new RecordingPlugin(attempt -> Mono.just("success"));
        final ServerWebExchange exchange = createExchange(body, 30000L);

        final Disposable subscription = plugin.execute(exchange, chain).subscribe();

        awaitRefCnt(chunk, 1);
        subscription.dispose();
        assertTrue(sourceCancelled.get(), "logical cancellation should cancel the request body publisher");
        awaitRefCnt(chunk, 0);
    }

    @Test
    void releasesPooledBuffersAfterSuccessfulAggregation() {
        final NettyDataBuffer first = pooledBuffer("abc");
        final NettyDataBuffer second = pooledBuffer("def");
        final RecordingPlugin plugin = new RecordingPlugin(attempt -> Mono.just("success"));
        final ServerWebExchange exchange = createExchange(Flux.just(first, second), 30000L);

        StepVerifier.create(plugin.execute(exchange, chain))
                .expectComplete()
                .verify(VERIFY_TIMEOUT);

        assertEquals(Collections.singletonList("abcdef"), plugin.getCapturedBodies());
        awaitRefCnt(first, 0);
        awaitRefCnt(second, 0);
    }

    @Test
    void replaysCachedBodyAfterCompletionAndToleratesRepeatedDispose() {
        final NettyDataBuffer chunk = pooledBuffer("abc");
        final RecordingPlugin plugin = new RecordingPlugin(attempt -> Mono.just("success"));
        final ServerWebExchange exchange = createExchange(Flux.just(chunk), 30000L);

        StepVerifier.create(plugin.execute(exchange, chain))
                .expectComplete()
                .verify(VERIFY_TIMEOUT);
        // The first execution disposed the holder on completion; subscribing the cached body again
        // must still replay it and the repeated dispose must be a no-op.
        StepVerifier.create(plugin.execute(exchange, chain))
                .expectComplete()
                .verify(VERIFY_TIMEOUT);

        assertEquals(Arrays.asList("abc", "abc"), plugin.getCapturedBodies());
        awaitRefCnt(chunk, 0);
    }

    @Test
    void replaysPooledBodyOnCurrentRetry() {
        final NettyDataBuffer first = pooledBuffer("abc");
        final NettyDataBuffer second = pooledBuffer("def");
        final RecordingPlugin plugin = new RecordingPlugin(attempt -> attempt == 1
                ? Mono.error(new TimeoutException("simulated attempt timeout"))
                : Mono.just("success"));
        final ServerWebExchange exchange = createExchange(Flux.just(first, second), 30000L);

        StepVerifier.create(plugin.execute(exchange, chain))
                .expectComplete()
                .verify(VERIFY_TIMEOUT);

        assertEquals(Arrays.asList("abcdef", "abcdef"), plugin.getCapturedBodies(),
                "the current retry must receive the complete body");
        awaitRefCnt(first, 0);
        awaitRefCnt(second, 0);
    }

    @Test
    void replaysPooledBodyOnFailoverRetry() {
        final NettyDataBuffer first = pooledBuffer("abc");
        final NettyDataBuffer second = pooledBuffer("def");
        final RecordingPlugin plugin = new RecordingPlugin(attempt -> attempt == 1
                ? Mono.error(new TimeoutException("simulated attempt timeout"))
                : Mono.just("success"));
        final ServerWebExchange exchange = createExchange(Flux.just(first, second), 30000L);
        exchange.getAttributes().put(Constants.RETRY_STRATEGY, RetryEnum.FAILOVER.getName());
        exchange.getAttributes().put(Constants.DIVIDE_SELECTOR_ID, "selector-1");
        exchange.getAttributes().put(Constants.LOAD_BALANCE, "roundRobin");

        final Upstream standby = Upstream.builder().url("localhost:8081").build();
        final UpstreamCacheManager cacheManager = mock(UpstreamCacheManager.class);
        when(cacheManager.findUpstreamListBySelectorId(anyString())).thenReturn(Collections.singletonList(standby));

        try (MockedStatic<UpstreamCacheManager> cacheMock = mockStatic(UpstreamCacheManager.class);
             MockedStatic<LoadbalancerUtils> lbMock = mockStatic(LoadbalancerUtils.class);
             MockedStatic<RequestUrlUtils> urlMock = mockStatic(RequestUrlUtils.class)) {
            cacheMock.when(UpstreamCacheManager::getInstance).thenReturn(cacheManager);
            lbMock.when(() -> LoadbalancerUtils.getForExchange(any(), anyString(), any())).thenReturn(standby);
            urlMock.when(() -> RequestUrlUtils.buildRequestUri(any(), anyString()))
                    .thenReturn(URI.create("http://localhost:8081/test"));

            StepVerifier.create(plugin.execute(exchange, chain))
                    .expectComplete()
                    .verify(VERIFY_TIMEOUT);
        }

        assertEquals(Arrays.asList("abcdef", "abcdef"), plugin.getCapturedBodies(),
                "the failover attempt must receive the complete body");
        awaitRefCnt(first, 0);
        awaitRefCnt(second, 0);
    }

    @Test
    void keepsAggregationAliveWhenAttemptTimesOutBeforeRetry() throws Exception {
        final NettyDataBuffer first = pooledBuffer("abc");
        final NettyDataBuffer second = pooledBuffer("def");
        final AtomicBoolean sourceCancelled = new AtomicBoolean();
        final AtomicReference<FluxSink<DataBuffer>> bodySink = new AtomicReference<>();
        final CountDownLatch bodySubscribed = new CountDownLatch(1);
        final Flux<DataBuffer> body = Flux.create(sink -> {
            bodySink.set(sink);
            sink.onCancel(() -> sourceCancelled.set(true));
            sink.next(first);
            bodySubscribed.countDown();
        });
        final CountDownLatch retryBodySubscribed = new CountDownLatch(1);
        final RecordingPlugin plugin = new RecordingPlugin(
                attempt -> attempt == 1 ? Mono.never() : Mono.just("success"),
                attempt -> {
                    if (attempt > 1) {
                        retryBodySubscribed.countDown();
                    }
                });
        final ServerWebExchange exchange = createExchange(body, 500L);

        final CompletableFuture<Void> completion = new CompletableFuture<>();
        final Disposable subscription = plugin.execute(exchange, chain)
                .subscribe(ignored -> { }, completion::completeExceptionally, () -> completion.complete(null));
        try {
            assertTrue(bodySubscribed.await(5, TimeUnit.SECONDS), "the first attempt should subscribe to the body");
            // The first attempt times out while the body is still being aggregated. The retry then
            // subscribes before this test finishes the body, so the aggregation must survive.
            assertTrue(retryBodySubscribed.await(5, TimeUnit.SECONDS), "the retry should subscribe to the body");
            bodySink.get().next(second);
            bodySink.get().complete();
            completion.get(5, TimeUnit.SECONDS);
        } finally {
            subscription.dispose();
        }

        assertEquals(Collections.singletonList("abcdef"), plugin.getCapturedBodies(),
                "the retry must receive the body aggregated after the first attempt timed out");
        assertFalse(sourceCancelled.get(),
                "an attempt timeout must not cancel the aggregation needed by the following retry");
        awaitRefCnt(first, 0);
        awaitRefCnt(second, 0);
    }

    private ServerWebExchange createExchange(final Flux<DataBuffer> body, final long timeout) {
        final MockServerHttpRequest request = MockServerHttpRequest.post("/test").build();
        final ServerWebExchange exchange = MockServerWebExchange.from(request);
        final ServerHttpRequest decoratedRequest = new ServerHttpRequestDecorator(exchange.getRequest()) {
            @Override
            public Flux<DataBuffer> getBody() {
                return body;
            }
        };
        final ServerWebExchange result = exchange.mutate().request(decoratedRequest).build();
        result.getAttributes().put(Constants.CONTEXT, mock(ShenyuContext.class));
        result.getAttributes().put(Constants.HTTP_URI, URI.create("http://localhost/test"));
        result.getAttributes().put(Constants.HTTP_TIME_OUT, timeout);
        result.getAttributes().put(Constants.HTTP_RETRY, 1);
        return result;
    }

    private NettyDataBuffer pooledBuffer(final String content) {
        final NettyDataBuffer buffer = bufferFactory.allocateBuffer(content.length());
        buffer.write(content.getBytes(StandardCharsets.UTF_8));
        return buffer;
    }

    private static void awaitRefCnt(final NettyDataBuffer buffer, final int expected) {
        final long deadline = System.nanoTime() + Duration.ofSeconds(5).toNanos();
        while (buffer.getNativeBuffer().refCnt() != expected && System.nanoTime() < deadline) {
            LockSupport.parkNanos(Duration.ofMillis(10).toNanos());
        }
        assertEquals(expected, buffer.getNativeBuffer().refCnt(),
                "unexpected reference count of the pooled request body buffer");
    }

    /**
     * A plugin that records the body of every attempt and lets the test decide the response.
     */
    private static final class RecordingPlugin extends AbstractHttpClientPlugin<String> {

        private final List<String> capturedBodies = Collections.synchronizedList(new ArrayList<>());

        private final AtomicInteger attempts = new AtomicInteger();

        private final IntFunction<Mono<String>> response;

        private final IntConsumer attemptListener;

        RecordingPlugin(final IntFunction<Mono<String>> response) {
            this(response, attempt -> { });
        }

        RecordingPlugin(final IntFunction<Mono<String>> response, final IntConsumer attemptListener) {
            super(Constants.BYTES_PER_MB);
            this.response = response;
            this.attemptListener = attemptListener;
        }

        @Override
        protected Integer extractStatusCode(final String response) {
            return 200;
        }

        protected Mono<String> doRequest(final ServerWebExchange exchange, final String httpMethod,
                                         final URI uri, final Flux<DataBuffer> body) {
            return Mono.defer(() -> {
                final int attempt = attempts.incrementAndGet();
                return DataBufferUtils.join(body.doOnSubscribe(subscription -> attemptListener.accept(attempt)))
                        .map(buffer -> {
                            final byte[] bytes = new byte[buffer.readableByteCount()];
                            buffer.read(bytes);
                            DataBufferUtils.release(buffer);
                            return new String(bytes, StandardCharsets.UTF_8);
                        })
                        .defaultIfEmpty("")
                        .flatMap(bodyString -> {
                            capturedBodies.add(bodyString);
                            return response.apply(attempt);
                        });
            });
        }

        List<String> getCapturedBodies() {
            return capturedBodies;
        }

        @Override
        public int getOrder() {
            return 0;
        }

        @Override
        public boolean skip(final ServerWebExchange exchange) {
            return false;
        }

        @Override
        public String named() {
            return "RecordingPlugin";
        }
    }
}
