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
import org.apache.shenyu.common.enums.RetryEnum;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.api.utils.RequestUrlUtils;
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
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpRequestDecorator;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Tests that the request body is correctly replayed during retry.
 *
 * <p>The original body Flux from the Netty channel is single-use; without caching,
 * retry attempts would send an empty body.
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

    @Test
    void testBodyReplayedOnFailoverRetry() {
        RecordingPlugin plugin = new RecordingPlugin(1);
        ServerWebExchange exchange = createExchangeWithSingleUseBody("{\"name\":\"hello\"}");
        // switch to failover strategy: DefaultRetryStrategy.resend picks a new upstream
        exchange.getAttributes().put(Constants.RETRY_STRATEGY, RetryEnum.FAILOVER.getName());
        exchange.getAttributes().put(Constants.DIVIDE_SELECTOR_ID, "selector-1");
        exchange.getAttributes().put(Constants.LOAD_BALANCE, "roundRobin");

        Upstream standby = Upstream.builder().url("localhost:8081").build();
        UpstreamCacheManager cacheManager = mock(UpstreamCacheManager.class);
        when(cacheManager.findUpstreamListBySelectorId(anyString())).thenReturn(Collections.singletonList(standby));

        try (MockedStatic<UpstreamCacheManager> cacheMock = org.mockito.Mockito.mockStatic(UpstreamCacheManager.class);
             MockedStatic<LoadbalancerUtils> lbMock = org.mockito.Mockito.mockStatic(LoadbalancerUtils.class);
             MockedStatic<RequestUrlUtils> urlMock = org.mockito.Mockito.mockStatic(RequestUrlUtils.class)) {
            cacheMock.when(UpstreamCacheManager::getInstance).thenReturn(cacheManager);
            lbMock.when(() -> LoadbalancerUtils.getForExchange(any(), anyString(), any())).thenReturn(standby);
            urlMock.when(() -> RequestUrlUtils.buildRequestUri(any(), anyString())).thenReturn(URI.create("http://localhost:8081/test"));

            StepVerifier.create(plugin.execute(exchange, chain))
                    .expectComplete()
                    .verify(Duration.ofSeconds(10));
        }

        assertEquals(2, plugin.getCapturedBodies().size(), "Should have 2 attempts (1 fail + 1 failover success)");
        assertEquals("{\"name\":\"hello\"}", plugin.getCapturedBodies().get(0), "First attempt should receive full body");
        assertEquals("{\"name\":\"hello\"}", plugin.getCapturedBodies().get(1), "Failover attempt should receive replayed body");
        assertNotNull(exchange.getAttribute(Constants.CACHED_REQUEST_BODY),
                "Body should be cached so failover resend can replay it");
    }

    @Test
    void testGetRequestRetriesWithoutBody() {
        RecordingPlugin plugin = new RecordingPlugin(1);
        ServerWebExchange exchange = createGetExchangeWithRetry();

        StepVerifier.create(plugin.execute(exchange, chain))
                .expectComplete()
                .verify(Duration.ofSeconds(10));

        assertEquals(2, plugin.getCapturedBodies().size(), "GET should retry (2 attempts: 1 fail + 1 success)");
        assertNull(exchange.getAttribute(Constants.CACHED_REQUEST_BODY),
                "GET must not cache a body it never reads");
    }

    @Test
    void testOversizeBodyThrowsDataBufferLimitException() {
        // body (9 bytes) exceeds maxInMemorySize (4 bytes) during aggregation
        RecordingPlugin plugin = new RecordingPlugin(0, 4);
        ServerWebExchange exchange = createExchangeWithSingleUseBody("test-body");

        StepVerifier.create(plugin.execute(exchange, chain))
                .expectErrorSatisfies(err -> {
                    // Body aggregation overflow must be mapped to 413, not a raw/generic 500
                    assertTrue(err instanceof ResponseStatusException,
                            "Oversize body should surface as ResponseStatusException");
                    ResponseStatusException rse = (ResponseStatusException) err;
                    assertEquals(HttpStatus.PAYLOAD_TOO_LARGE, rse.getStatusCode(),
                            "Oversize body should map to 413 Payload Too Large");
                    assertTrue(rse.getCause() instanceof org.springframework.core.io.buffer.DataBufferLimitException,
                            "Underlying cause should be DataBufferLimitException");
                })
                .verify(Duration.ofSeconds(10));
        assertEquals(0, plugin.getCapturedBodies().size(), "Oversize body must never reach doRequest");
    }

    @Test
    void testOversizeBodyNotRetriedUnderFixedStrategy() {
        RecordingPlugin plugin = new RecordingPlugin(0, 4);
        ServerWebExchange exchange = createExchangeWithSingleUseBody("test-body");
        exchange.getAttributes().put(Constants.HTTP_RETRY_BACK_OFF_SPEC, "fixed");

        StepVerifier.create(plugin.execute(exchange, chain))
                .expectErrorSatisfies(err -> {
                    assertTrue(err instanceof ResponseStatusException);
                    assertEquals(HttpStatus.PAYLOAD_TOO_LARGE, ((ResponseStatusException) err).getStatusCode());
                })
                .verify(Duration.ofSeconds(10));

        assertEquals(0, plugin.getCapturedBodies().size(),
                "Fixed strategy must not retry oversize body (it can never be cached for replay)");
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

    private ServerWebExchange createGetExchangeWithRetry() {
        final MockServerHttpRequest mockRequest = MockServerHttpRequest.get("/test").build();
        ServerWebExchange exchange = MockServerWebExchange.from(mockRequest);
        exchange.getAttributes().put(Constants.CONTEXT, mock(ShenyuContext.class));
        exchange.getAttributes().put(Constants.HTTP_URI, URI.create("http://localhost/test"));
        exchange.getAttributes().put(Constants.HTTP_TIME_OUT, 30000L);
        exchange.getAttributes().put(Constants.HTTP_RETRY, 3);
        return exchange;
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
            this(failFirstN, Constants.BYTES_PER_MB);
        }

        RecordingPlugin(final int failFirstN, final long maxInMemorySize) {
            super(maxInMemorySize);
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
