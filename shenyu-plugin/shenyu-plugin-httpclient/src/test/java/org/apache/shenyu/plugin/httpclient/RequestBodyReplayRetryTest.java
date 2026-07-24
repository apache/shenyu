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
import java.util.Collections;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpRequestDecorator;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Tests that request body is correctly replayed during retry.
 *
 * <p>The original body Flux from the Netty channel is single-use; without caching,
 * retry attempts would send an empty body. These tests verify the caching mechanism
 * in {@link AbstractHttpClientPlugin} ensures body replay on retry.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class RequestBodyReplayRetryTest {

    private ShenyuPluginChain chain;

    @BeforeEach
    public void setUp() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setApplicationContext(context);
        when(context.getBean(ShenyuResult.class)).thenReturn(mock(ShenyuResult.class));
        chain = mock(ShenyuPluginChain.class);
        when(chain.execute(any())).thenReturn(Mono.empty());
    }

    /**
     * Verifies that request body is correctly replayed when retry is triggered.
     *
     * <p>With a single-use body (simulating Netty FluxReceive), the first attempt
     * consumes the body. Without caching, the retry would send an empty body.
     * The fix caches the body so both attempts receive the full content.
     */
    @Test
    void testBodyReplayedOnRetry() {
        RecordingPlugin plugin = new RecordingPlugin(1);
        ServerWebExchange exchange = createExchangeWithSingleUseBody("{\"name\":\"hello\"}");

        StepVerifier.create(plugin.execute(exchange, chain))
                .expectComplete()
                .verify(Duration.ofSeconds(10));

        assertEquals(2, plugin.getCapturedBodies().size(), "Should have 2 attempts (1 fail + 1 success)");
        assertEquals("{\"name\":\"hello\"}", plugin.getCapturedBodies().get(0), "First attempt should receive full body");
        assertEquals("{\"name\":\"hello\"}", plugin.getCapturedBodies().get(1), "Retry attempt should receive replayed body");
        assertNotNull(exchange.getAttribute(Constants.CACHED_REQUEST_BODY),
                "Body should be cached when retry is enabled");
    }

    /**
     * Verifies that request body is correctly replayed across multiple retries.
     *
     * <p>Fails the first 2 attempts, succeeds on the 3rd. All 3 attempts should
     * receive the same full body content.
     */
    @Test
    void testBodyReplayedAcrossMultipleRetries() {
        RecordingPlugin plugin = new RecordingPlugin(2);
        ServerWebExchange exchange = createExchangeWithSingleUseBody("{\"name\":\"hello\"}");

        StepVerifier.create(plugin.execute(exchange, chain))
                .expectComplete()
                .verify(Duration.ofSeconds(10));

        assertEquals(3, plugin.getCapturedBodies().size(), "Should have 3 attempts (2 fail + 1 success)");
        for (int i = 0; i < 3; i++) {
            assertEquals("{\"name\":\"hello\"}", plugin.getCapturedBodies().get(i),
                    "Attempt " + (i + 1) + " should receive full body");
        }
    }

    /**
     * Verifies that getCachedRequestBody returns a replayable Flux.
     *
     * <p>Subscribing to the returned Flux multiple times should yield the same
     * body content each time, even when the source body is single-use.
     */
    @Test
    void testGetCachedRequestBodyIsReplayable() {
        RecordingPlugin plugin = new RecordingPlugin(0);
        ServerWebExchange exchange = createExchangeWithSingleUseBody("test-body");

        Flux<DataBuffer> cached = plugin.getCachedRequestBody(exchange);

        String firstRead = readBody(cached);
        String secondRead = readBody(cached);

        assertEquals("test-body", firstRead, "First read should get full body");
        assertEquals("test-body", secondRead, "Second read (retry) should get same body via replay");
    }

    /**
     * Verifies that getCachedRequestBody is idempotent — calling it twice
     * returns the same cached Flux instance from exchange attributes.
     */
    @Test
    void testGetCachedRequestBodyIsIdempotent() {
        RecordingPlugin plugin = new RecordingPlugin(0);
        ServerWebExchange exchange = createExchangeWithSingleUseBody("test-body");

        Flux<DataBuffer> firstCall = plugin.getCachedRequestBody(exchange);
        Flux<DataBuffer> secondCall = plugin.getCachedRequestBody(exchange);

        assertSame(firstCall, secondCall,
                "Subsequent calls should return the same cached Flux instance");
    }

    private ServerWebExchange createExchangeWithSingleUseBody(final String body) {
        final AtomicBoolean consumed = new AtomicBoolean(false);
        final MockServerHttpRequest mockRequest = MockServerHttpRequest
                .post("/test")
                .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .body(body);
        ServerWebExchange exchange = MockServerWebExchange.from(mockRequest);
        ServerHttpRequest singleUseRequest = new ServerHttpRequestDecorator(exchange.getRequest()) {
            @Override
            public Flux<DataBuffer> getBody() {
                return Flux.defer(() -> {
                    if (consumed.compareAndSet(false, true)) {
                        return mockRequest.getBody();
                    }
                    return Flux.empty();
                });
            }
        };
        exchange = exchange.mutate().request(singleUseRequest).build();
        exchange.getAttributes().put(Constants.CONTEXT, mock(ShenyuContext.class));
        exchange.getAttributes().put(Constants.HTTP_URI, URI.create("http://localhost/test"));
        exchange.getAttributes().put(Constants.HTTP_TIME_OUT, 30000L);
        exchange.getAttributes().put(Constants.HTTP_RETRY, 3);
        return exchange;
    }

    private String readBody(final Flux<DataBuffer> body) {
        return DataBufferUtils.join(body)
                .map(buffer -> {
                    byte[] bytes = new byte[buffer.readableByteCount()];
                    buffer.read(bytes);
                    DataBufferUtils.release(buffer);
                    return new String(bytes, StandardCharsets.UTF_8);
                })
                .defaultIfEmpty("")
                .block(Duration.ofSeconds(5));
    }

    /**
     * A test plugin that records the body received on each doRequest call
     * and fails the first N attempts with TimeoutException (retryable by DefaultRetryStrategy).
     */
    static class RecordingPlugin extends AbstractHttpClientPlugin<String> {

        private final List<String> capturedBodies = Collections.synchronizedList(new ArrayList<>());

        private final AtomicInteger attempts = new AtomicInteger();

        private final int failFirstN;

        RecordingPlugin(final int failFirstN) {
            this.failFirstN = failFirstN;
        }

        List<String> getCapturedBodies() {
            return capturedBodies;
        }

        @Override
        protected Mono<String> doRequest(final ServerWebExchange exchange, final String httpMethod,
                                         final URI uri, final Flux<DataBuffer> body) {
            return DataBufferUtils.join(body)
                    .map(buffer -> {
                        byte[] bytes = new byte[buffer.readableByteCount()];
                        buffer.read(bytes);
                        DataBufferUtils.release(buffer);
                        return new String(bytes, StandardCharsets.UTF_8);
                    })
                    .defaultIfEmpty("")
                    .flatMap(bodyStr -> {
                        capturedBodies.add(bodyStr);
                        if (attempts.incrementAndGet() <= failFirstN) {
                            return Mono.error(new java.util.concurrent.TimeoutException(
                                    "Simulated timeout, attempt " + attempts.get()));
                        }
                        return Mono.just("success");
                    });
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
