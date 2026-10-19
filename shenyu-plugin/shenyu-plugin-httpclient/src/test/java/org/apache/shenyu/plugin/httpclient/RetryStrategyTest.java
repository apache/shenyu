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

import io.netty.channel.ConnectTimeoutException;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.RetryEnum;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.base.circuitbreaker.UpstreamCircuitBreaker;
import org.apache.shenyu.plugin.httpclient.exception.ShenyuUpstreamStatusException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpHeaders;
import org.junit.jupiter.params.provider.CsvSource;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedStatic;
import org.springframework.http.HttpStatus;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.reactive.function.client.WebClientResponseException;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.net.ConnectException;
import java.net.URI;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/**
 * retry strategy test.
 *
 * @Date 2025/3/16 22:46
 */
public class RetryStrategyTest {

    @Test
    void testDefaultRetryBackoffExecute() {
        // Create a simulated AbstractHttpClientPlugin
        AbstractHttpClientPlugin<String> httpClientPlugin = mock(AbstractHttpClientPlugin.class);
        ExponentialRetryBackoffStrategy<String> strategy = new ExponentialRetryBackoffStrategy<>(httpClientPlugin);

        // Create a simulated ServerWebExchange
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/").build());
        Duration duration = Duration.ofSeconds(30);
        int retryTimes = 3;

        // Create a mock response Mono that throws a network exception
        AtomicInteger attempts = new AtomicInteger();
        ConnectTimeoutException failure = new ConnectTimeoutException("connection timed out");
        Mono<String> response = Mono.defer(() -> {
            attempts.incrementAndGet();
            return Mono.error(failure);
        });

        // Execute retry policy
        StepVerifier.withVirtualTime(() -> strategy.execute(response, exchange, duration, retryTimes))
                .thenAwait(Duration.ofSeconds(30))
                .expectErrorMatches(error -> error == failure)
                .verify();
        assertEquals(retryTimes + 1, attempts.get());
    }

    @ParameterizedTest
    @ValueSource(strings = {"GET", "POST", "PUT"})
    void testExponentialStrategyDoesNotRetryUnsafeRequestsOrPermanentFailures(final String method) {
        ExponentialRetryBackoffStrategy<String> strategy = new ExponentialRetryBackoffStrategy<>(mock(AbstractHttpClientPlugin.class));
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.method(HttpMethod.valueOf(method), "/").build());
        Throwable failure = "GET".equals(method)
                ? WebClientResponseException.create(400, "Bad Request", HttpHeaders.EMPTY, new byte[0], null)
                : new TimeoutException("request timed out");
        AtomicInteger attempts = new AtomicInteger();
        Mono<String> response = Mono.defer(() -> {
            attempts.incrementAndGet();
            return Mono.error(failure);
        });
        StepVerifier.create(strategy.execute(response, exchange, Duration.ofSeconds(5), 3))
                .expectErrorMatches(error -> error == failure).verify();
        assertEquals(1, attempts.get());
    }

    @Test
    void testDefaultRetryStrategyExecute() {
        //Create a simulated AbstractHttpClientPlugin
        AbstractHttpClientPlugin<String> httpClientPlugin = mock(AbstractHttpClientPlugin.class);
        DefaultRetryStrategy<String> strategy = new DefaultRetryStrategy<>(httpClientPlugin);

        // Create a simulated ServerWebExchange
        ServerWebExchange exchange = mock(ServerWebExchange.class);
        Duration duration = Duration.ofSeconds(5);
        int retryTimes = 3;

        // Create a mock response Mono that throws an exception
        Mono<String> response = Mono.error(new RuntimeException("Test error"));

        // Execute retry policy
        Mono<String> result = strategy.execute(response, exchange, duration, retryTimes);

        // Use StepVerifier to verify results
        StepVerifier.create(result)
                .expectError(RuntimeException.class)
                .verify();
    }

    @ParameterizedTest
    @CsvSource({
        "http://localhost/test, http://, localhost",
        "http://localhost/test, http://, localhost:80",
        "https://localhost/test, https://, localhost:443",
        "http://localhost:8080/other?ignored=true, http://, localhost:8080"
    })
    void testFailoverExcludesSameUpstream(final String currentUri, final String protocol, final String url) {
        AbstractHttpClientPlugin<String> plugin = mock(AbstractHttpClientPlugin.class);
        ServerWebExchange exchange = createFailoverExchange(currentUri);
        Upstream upstream = Upstream.builder().protocol(protocol).url(url).build();
        UpstreamCacheManager cacheManager = mock(UpstreamCacheManager.class);
        when(cacheManager.findLegacyUpstreamListBySelectorId("selector-7475")).thenReturn(Collections.singletonList(upstream));

        try (MockedStatic<UpstreamCacheManager> cacheMock = mockStatic(UpstreamCacheManager.class)) {
            cacheMock.when(UpstreamCacheManager::getInstance).thenReturn(cacheManager);
            StepVerifier.create(new DefaultRetryStrategy<>(plugin).execute(Mono.error(new TimeoutException("upstream failed")),
                    exchange, Duration.ofSeconds(5), 2))
                    .expectErrorSatisfies(this::assertFailoverExhausted)
                    .verify();
        }
        verifyNoInteractions(plugin);
    }

    @ParameterizedTest
    @CsvSource({
        "http://, standby:8080",
        "http://, localhost:8081",
        "https://, localhost:8080"
    })
    void testFailoverSelectsDifferentUpstream(final String protocol, final String url) {
        AbstractHttpClientPlugin<String> plugin = mock(AbstractHttpClientPlugin.class);
        ServerWebExchange exchange = createFailoverExchange("http://localhost:8080/test?mode=retry");
        Upstream failed = Upstream.builder().url("localhost:8080").build();
        Upstream standby = Upstream.builder().protocol(protocol).url(url).build();
        UpstreamCacheManager cacheManager = mock(UpstreamCacheManager.class);
        when(cacheManager.findLegacyUpstreamListBySelectorId("selector-7475")).thenReturn(Arrays.asList(failed, standby));
        when(plugin.getCachedRequestBody(exchange)).thenReturn(Flux.empty());
        when(plugin.doRequest(eq(exchange), eq("GET"), any(URI.class), any())).thenReturn(Mono.just("success"));

        try (MockedStatic<UpstreamCacheManager> cacheMock = mockStatic(UpstreamCacheManager.class)) {
            cacheMock.when(UpstreamCacheManager::getInstance).thenReturn(cacheManager);
            StepVerifier.create(new DefaultRetryStrategy<>(plugin).execute(Mono.error(new TimeoutException("upstream failed")),
                    exchange, Duration.ofSeconds(5), 2))
                    .expectNext("success")
                    .verifyComplete();
        }
        ArgumentCaptor<URI> uriCaptor = ArgumentCaptor.forClass(URI.class);
        verify(plugin).doRequest(eq(exchange), eq("GET"), uriCaptor.capture(), any());
        assertEquals(URI.create(standby.buildDomain() + "/test?mode=retry"), uriCaptor.getValue());
    }

    @Test
    void testFailoverWithMissingUpstreamList() {
        AbstractHttpClientPlugin<String> httpClientPlugin = mock(AbstractHttpClientPlugin.class);
        DefaultRetryStrategy<String> strategy = new DefaultRetryStrategy<>(httpClientPlugin);
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/test").build());
        exchange.getAttributes().put(Constants.RETRY_STRATEGY, RetryEnum.FAILOVER.getName());
        exchange.getAttributes().put(Constants.HTTP_URI, URI.create("http://localhost:8080/test"));
        exchange.getAttributes().put(Constants.DIVIDE_SELECTOR_ID, "retry-missing-upstream");

        StepVerifier.create(strategy.execute(Mono.error(new ConnectTimeoutException("connection timed out")), exchange, Duration.ofSeconds(5), 3))
                .expectErrorSatisfies(error -> {
                    assertTrue(error instanceof ResponseStatusException);
                    ResponseStatusException exception = (ResponseStatusException) error;
                    assertEquals(HttpStatus.SERVICE_UNAVAILABLE, exception.getStatusCode());
                    assertEquals("CANNOT_FIND_HEALTHY_UPSTREAM_URL_AFTER_FAILOVER", exception.getReason());
                })
                .verify();

        verifyNoInteractions(httpClientPlugin);
    }

    @Test
    void testFailoverSkipsBlockedUpstream() {
        AbstractHttpClientPlugin<String> plugin = mock(AbstractHttpClientPlugin.class);
        ServerWebExchange exchange = createFailoverExchange("http://localhost:8080/test?mode=retry");
        Upstream failed = Upstream.builder().url("localhost:8080").build();
        Upstream standby = Upstream.builder().protocol("http://").url("localhost:8081").build();
        UpstreamCacheManager cacheManager = mock(UpstreamCacheManager.class);
        when(cacheManager.findLegacyUpstreamListBySelectorId("selector-7475")).thenReturn(Arrays.asList(failed, standby));
        final String standbyKey = UpstreamCircuitBreaker.buildKey("selector-7475", standby);
        UpstreamCircuitBreaker breaker = UpstreamCircuitBreaker.getInstance();
        for (int i = 0; i < 3; i++) {
            breaker.recordFailure(standbyKey);
        }
        try (MockedStatic<UpstreamCacheManager> cacheMock = mockStatic(UpstreamCacheManager.class)) {
            cacheMock.when(UpstreamCacheManager::getInstance).thenReturn(cacheManager);
            StepVerifier.create(new DefaultRetryStrategy<>(plugin).execute(Mono.error(new TimeoutException("upstream failed")),
                            exchange, Duration.ofSeconds(5), 2))
                    .expectErrorSatisfies(this::assertFailoverExhausted)
                    .verify();
        } finally {
            breaker.reset(standbyKey);
        }
        verifyNoInteractions(plugin);
    }

    @Test
    void testFailoverResendRecordsFailureOutcome() {
        AbstractHttpClientPlugin<String> plugin = mock(AbstractHttpClientPlugin.class);
        ServerWebExchange exchange = createFailoverExchange("http://localhost:8080/test?mode=retry");
        Upstream failed = Upstream.builder().url("localhost:8080").build();
        Upstream standby = Upstream.builder().protocol("http://").url("localhost:8081").build();
        UpstreamCacheManager cacheManager = mock(UpstreamCacheManager.class);
        when(cacheManager.findLegacyUpstreamListBySelectorId("selector-7475")).thenReturn(Arrays.asList(failed, standby));
        when(plugin.getCachedRequestBody(exchange)).thenReturn(Flux.empty());
        when(plugin.doRequest(eq(exchange), eq("GET"), any(URI.class), any())).thenReturn(Mono.error(new TimeoutException("resend failed")));
        final String standbyKey = UpstreamCircuitBreaker.buildKey("selector-7475", standby);
        UpstreamCircuitBreaker breaker = UpstreamCircuitBreaker.getInstance();
        try (MockedStatic<UpstreamCacheManager> cacheMock = mockStatic(UpstreamCacheManager.class)) {
            cacheMock.when(UpstreamCacheManager::getInstance).thenReturn(cacheManager);
            StepVerifier.create(new DefaultRetryStrategy<>(plugin).execute(Mono.error(new TimeoutException("upstream failed")),
                            exchange, Duration.ofSeconds(5), 2))
                    .expectErrorSatisfies(this::assertFailoverExhausted)
                    .verify();
        }
        // the failed resend recorded one failure, one more below the threshold keeps the breaker closed
        breaker.recordFailure(standbyKey);
        assertFalse(breaker.isBlocking(standbyKey));
        breaker.recordFailure(standbyKey);
        assertTrue(breaker.isBlocking(standbyKey));
        breaker.reset(standbyKey);
    }

    @Test
    void testFailoverResendRecordsSuccessOutcome() {
        AbstractHttpClientPlugin<String> plugin = mock(AbstractHttpClientPlugin.class);
        ServerWebExchange exchange = createFailoverExchange("http://localhost:8080/test?mode=retry");
        Upstream failed = Upstream.builder().url("localhost:8080").build();
        Upstream standby = Upstream.builder().protocol("http://").url("localhost:8081").build();
        UpstreamCacheManager cacheManager = mock(UpstreamCacheManager.class);
        when(cacheManager.findLegacyUpstreamListBySelectorId("selector-7475")).thenReturn(Arrays.asList(failed, standby));
        when(plugin.getCachedRequestBody(exchange)).thenReturn(Flux.empty());
        when(plugin.doRequest(eq(exchange), eq("GET"), any(URI.class), any())).thenReturn(Mono.just("success"));
        final String standbyKey = UpstreamCircuitBreaker.buildKey("selector-7475", standby);
        UpstreamCircuitBreaker breaker = UpstreamCircuitBreaker.getInstance();
        breaker.recordFailure(standbyKey);
        breaker.recordFailure(standbyKey);
        try (MockedStatic<UpstreamCacheManager> cacheMock = mockStatic(UpstreamCacheManager.class)) {
            cacheMock.when(UpstreamCacheManager::getInstance).thenReturn(cacheManager);
            StepVerifier.create(new DefaultRetryStrategy<>(plugin).execute(Mono.error(new TimeoutException("upstream failed")),
                            exchange, Duration.ofSeconds(5), 2))
                    .expectNext("success")
                    .verifyComplete();
        }
        // the successful resend reset the failure counter, so two more failures stay below the threshold
        breaker.recordFailure(standbyKey);
        breaker.recordFailure(standbyKey);
        assertFalse(breaker.isBlocking(standbyKey));
        breaker.reset(standbyKey);
    }

    @Test
    void testFixedRetryStrategyRetriesTransientGetFailures() {
        AbstractHttpClientPlugin<String> httpClientPlugin = mock(AbstractHttpClientPlugin.class);
        FixedRetryStrategy<String> strategy = new FixedRetryStrategy<>(httpClientPlugin);
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/").build());
        AtomicInteger attempts = new AtomicInteger();
        Mono<String> response = Mono.defer(() -> {
            attempts.incrementAndGet();
            return Mono.error(new ConnectTimeoutException("connection timed out"));
        });

        StepVerifier.withVirtualTime(() -> strategy.execute(response, exchange, Duration.ofSeconds(10), 2))
                .thenAwait(Duration.ofSeconds(4))
                .expectError(ConnectTimeoutException.class)
                .verify();

        assertEquals(3, attempts.get());
    }

    @Test
    void testFixedRetryStrategyDoesNotRetryPermanentFailures() {
        AbstractHttpClientPlugin<String> httpClientPlugin = mock(AbstractHttpClientPlugin.class);
        FixedRetryStrategy<String> strategy = new FixedRetryStrategy<>(httpClientPlugin);
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/").build());
        AtomicInteger attempts = new AtomicInteger();
        Mono<String> response = Mono.defer(() -> {
            attempts.incrementAndGet();
            return Mono.error(new IllegalArgumentException("permanent failure"));
        });

        StepVerifier.create(strategy.execute(response, exchange, Duration.ofSeconds(5), 3))
                .expectError(IllegalArgumentException.class)
                .verify();

        assertEquals(1, attempts.get());
    }

    @Test
    void testFixedRetryStrategyDoesNotRetryPostRequests() {
        AbstractHttpClientPlugin<String> httpClientPlugin = mock(AbstractHttpClientPlugin.class);
        FixedRetryStrategy<String> strategy = new FixedRetryStrategy<>(httpClientPlugin);
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/").build());
        AtomicInteger attempts = new AtomicInteger();
        Mono<String> response = Mono.defer(() -> {
            attempts.incrementAndGet();
            return Mono.error(new TimeoutException("request timed out"));
        });

        StepVerifier.create(strategy.execute(response, exchange, Duration.ofSeconds(5), 3))
                .expectError(TimeoutException.class)
                .verify();

        assertEquals(1, attempts.get());
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void testFixedFailoverWaitsBeforeSendingToStandby(final boolean serverError) {
        ServerWebExchange exchange = createFixedFailoverExchange(HttpMethod.GET, 1);
        URI primaryUri = exchange.getAttribute(Constants.HTTP_URI);
        URI standbyUri = URI.create("http://localhost:8081/test?mode=retry");
        Throwable failure = serverError ? new ShenyuUpstreamStatusException(503) : new ConnectException("connection refused");
        List<URI> attempts = new ArrayList<>();
        AbstractHttpClientPlugin<String> plugin = recordingPlugin(attempts,
                uri -> primaryUri.equals(uri) ? Mono.error(failure) : Mono.just("success"));
        Upstream standby = Upstream.builder().url("localhost:8081").build();
        UpstreamCacheManager cacheManager = mock(UpstreamCacheManager.class);
        when(cacheManager.findLegacyUpstreamListBySelectorId("fixed-failover")).thenReturn(Collections.singletonList(standby));

        try (MockedStatic<UpstreamCacheManager> cacheMock = mockStatic(UpstreamCacheManager.class)) {
            cacheMock.when(UpstreamCacheManager::getInstance).thenReturn(cacheManager);
            StepVerifier.withVirtualTime(() -> plugin.execute(exchange, ignored -> Mono.empty()))
                    .thenAwait(Duration.ofMillis(1999))
                    .then(() -> {
                        assertEquals(Collections.singletonList(primaryUri), attempts);
                        verifyNoInteractions(cacheManager);
                    })
                    .thenAwait(Duration.ofMillis(1))
                    .verifyComplete();
        }
        assertEquals(Arrays.asList(primaryUri, standbyUri), attempts);
    }

    @ParameterizedTest
    @ValueSource(strings = {"POST", "GET"})
    void testFixedFailoverPreservesRetryEligibility(final String method) {
        ServerWebExchange exchange = createFixedFailoverExchange(HttpMethod.valueOf(method), 1);
        Throwable failure = "POST".equals(method) ? new ConnectException("connection refused") : new IllegalArgumentException("permanent failure");
        List<URI> attempts = new ArrayList<>();
        AbstractHttpClientPlugin<String> plugin = recordingPlugin(attempts, uri -> Mono.error(failure));

        StepVerifier.create(plugin.execute(exchange, ignored -> Mono.empty()))
                .expectErrorMatches(error -> error == failure)
                .verify();
        assertEquals(Collections.singletonList(exchange.getAttribute(Constants.HTTP_URI)), attempts);
    }

    @Test
    void testFixedFailoverRetriesAcrossDistinctUpstreams() {
        ServerWebExchange exchange = createFixedFailoverExchange(HttpMethod.GET, 2);
        URI primaryUri = exchange.getAttribute(Constants.HTTP_URI);
        URI standbyUri = URI.create("http://localhost:8081/test?mode=retry");
        URI healthyUri = URI.create("http://localhost:8082/test?mode=retry");
        ShenyuUpstreamStatusException failure = new ShenyuUpstreamStatusException(503);
        List<URI> attempts = new ArrayList<>();
        AbstractHttpClientPlugin<String> plugin = recordingPlugin(attempts,
                uri -> healthyUri.equals(uri) ? Mono.just("success") : Mono.error(failure));
        Upstream primary = Upstream.builder().url("localhost:8080").build();
        Upstream standby = Upstream.builder().url("localhost:8081").build();
        Upstream healthy = Upstream.builder().url("localhost:8082").build();
        UpstreamCacheManager cacheManager = mock(UpstreamCacheManager.class);
        when(cacheManager.findLegacyUpstreamListBySelectorId("fixed-failover"))
                .thenReturn(Arrays.asList(primary, standby), Arrays.asList(primary, standby, healthy));

        try (MockedStatic<UpstreamCacheManager> cacheMock = mockStatic(UpstreamCacheManager.class)) {
            cacheMock.when(UpstreamCacheManager::getInstance).thenReturn(cacheManager);
            StepVerifier.withVirtualTime(() -> plugin.execute(exchange, ignored -> Mono.empty()))
                    .thenAwait(Duration.ofSeconds(4))
                    .verifyComplete();
        } finally {
            UpstreamCircuitBreaker.getInstance().reset(UpstreamCircuitBreaker.buildKey("fixed-failover", standby));
        }
        assertEquals(Arrays.asList(primaryUri, standbyUri, healthyUri), attempts);
    }

    private ServerWebExchange createFixedFailoverExchange(final HttpMethod method, final int retryTimes) {
        ServerWebExchange exchange = createFailoverExchange("http://localhost:8080/test?mode=retry")
                .mutate().request(request -> request.method(method)).build();
        exchange.getAttributes().put(Constants.CONTEXT, new ShenyuContext());
        exchange.getAttributes().put(Constants.HTTP_RETRY_BACK_OFF_SPEC, "fixed");
        exchange.getAttributes().put(Constants.HTTP_RETRY, retryTimes);
        exchange.getAttributes().put(Constants.HTTP_TIME_OUT, 10000L);
        exchange.getAttributes().put(Constants.DIVIDE_SELECTOR_ID, "fixed-failover");
        return exchange;
    }

    private AbstractHttpClientPlugin<String> recordingPlugin(final List<URI> attempts, final Function<URI, Mono<String>> response) {
        return new AbstractHttpClientPlugin<String>(1024) {
            @Override
            protected Mono<String> doRequest(final ServerWebExchange exchange, final String httpMethod, final URI uri, final Flux<DataBuffer> body) {
                return Mono.defer(() -> {
                    attempts.add(uri);
                    return response.apply(uri);
                });
            }

            @Override
            public int getOrder() {
                return 0;
            }
        };
    }

    private ServerWebExchange createFailoverExchange(final String currentUri) {
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/test?mode=retry"));
        exchange.getAttributes().put(Constants.HTTP_URI, URI.create(currentUri));
        exchange.getAttributes().put(Constants.REWRITE_URI, "/test");
        exchange.getAttributes().put(Constants.RETRY_STRATEGY, RetryEnum.FAILOVER.getName());
        exchange.getAttributes().put(Constants.DIVIDE_SELECTOR_ID, "selector-7475");
        exchange.getAttributes().put(Constants.LOAD_BALANCE, "roundRobin");
        return exchange;
    }

    private void assertFailoverExhausted(final Throwable error) {
        ResponseStatusException statusException = assertInstanceOf(ResponseStatusException.class, error);
        assertEquals(HttpStatus.SERVICE_UNAVAILABLE, statusException.getStatusCode());
        assertEquals("CANNOT_FIND_HEALTHY_UPSTREAM_URL_AFTER_FAILOVER", statusException.getReason());
    }
}
