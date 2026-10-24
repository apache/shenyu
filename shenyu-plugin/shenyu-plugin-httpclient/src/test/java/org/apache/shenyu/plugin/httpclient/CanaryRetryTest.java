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

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.base.circuitbreaker.UpstreamCircuitBreaker;
import org.apache.shenyu.plugin.httpclient.exception.ShenyuUpstreamStatusException;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.MockedStatic;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.net.ConnectException;
import java.net.URI;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeoutException;
import java.util.function.Function;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

class CanaryRetryTest {

    private static final String SELECTOR = "canary-retry";

    private final List<URI> attempts = new ArrayList<>();

    private final List<Upstream> nodes = new ArrayList<>();

    private final UpstreamCacheManager cache = mock(UpstreamCacheManager.class);

    @AfterEach
    void resetBreakers() {
        nodes.forEach(node -> UpstreamCircuitBreaker.getInstance().reset(UpstreamCircuitBreaker.buildKey(SELECTOR, node)));
    }

    @ParameterizedTest
    @MethodSource("partitionRetries")
    void retriesRefreshHealthyNodesWithinTheOriginalPartition(final String backoff, final String partition, final boolean serverError) {
        ServerWebExchange exchange = exchange(backoff, partition, 2);
        final Upstream primary = upstream("primary", partition);
        final Upstream second = upstream("second", partition);
        final Upstream added = upstream("added", partition);
        Upstream other = upstream("other", "canary".equals(partition) ? "stable" : "canary");
        other.setGray(true);
        Upstream wrongZone = upstream("wrong-zone", partition);
        wrongZone.setLabels(Map.of("release", partition, "zone", "west"));
        Upstream unlabeled = upstream("unlabeled", partition);
        unlabeled.setLabels(Map.of());
        when(cache.findAllUpstreamListBySelectorId(SELECTOR))
                .thenReturn(List.of(primary, second, other, wrongZone, unlabeled), List.of(added, other, wrongZone, unlabeled));
        // A legacy gray-only lookup would send the retry to the opposite partition.
        when(cache.findUpstreamListBySelectorId(SELECTOR)).thenReturn(List.of(other));
        Throwable failure = serverError ? new ShenyuUpstreamStatusException(503) : new ConnectException("connection refused");
        AbstractHttpClientPlugin<String> plugin = plugin(uri -> uri.getHost().equals("added") ? Mono.just("success") : Mono.error(failure));

        try (MockedStatic<UpstreamCacheManager> ignored = mockCache()) {
            StepVerifier.withVirtualTime(() -> plugin.execute(exchange, current -> Mono.empty()))
                    .thenAwait(Duration.ofSeconds(10)).verifyComplete();
        }
        assertEquals(List.of(uri("primary"), uri("second"), uri("added")), attempts);
        verify(cache, times(2)).findAllUpstreamListBySelectorId(SELECTOR);
        verify(cache, never()).findUpstreamListBySelectorId(anyString());
        assertEquals(partition, exchange.getAttribute(Constants.SHENYU_CANARY_PARTITION));
    }

    @ParameterizedTest
    @ValueSource(strings = {"default", "fixed", "exponential"})
    void exhaustedPartitionStopsWithoutCrossingToStable(final String backoff) {
        ServerWebExchange exchange = exchange(backoff, "canary", 5);
        Upstream sameEndpoint = upstream("PRIMARY:80", "canary");
        Upstream blocked = upstream("blocked", "canary");
        UpstreamCircuitBreaker breaker = UpstreamCircuitBreaker.getInstance();
        for (int i = 0; i < UpstreamCircuitBreaker.DEFAULT_FAILURE_THRESHOLD; i++) {
            breaker.recordFailure(UpstreamCircuitBreaker.buildKey(SELECTOR, blocked));
        }
        when(cache.findAllUpstreamListBySelectorId(SELECTOR)).thenReturn(List.of(sameEndpoint, blocked, upstream("stable", "stable")));
        AbstractHttpClientPlugin<String> plugin = plugin(uri -> Mono.error(new ConnectException("connection refused")));
        try (MockedStatic<UpstreamCacheManager> ignored = mockCache()) {
            StepVerifier.withVirtualTime(() -> plugin.execute(exchange, current -> Mono.empty()))
                    .thenAwait(Duration.ofSeconds(2)).expectErrorSatisfies(this::assertExhausted).verify();
        }
        assertEquals(List.of(uri("primary")), attempts);
        verify(cache).findAllUpstreamListBySelectorId(SELECTOR);
    }

    @ParameterizedTest
    @CsvSource({"default,true", "fixed,true", "exponential,true", "default,false", "fixed,false", "exponential,false"})
    void missingPartitionLabelsOrRemovedSelectorFailClosed(final String backoff, final boolean missingLabels) {
        ServerWebExchange exchange = exchange(backoff, "canary", 2);
        if (missingLabels) {
            exchange.getAttributes().remove(Constants.SHENYU_CANARY_LABELS);
            when(cache.findAllUpstreamListBySelectorId(SELECTOR)).thenReturn(List.of(upstream("second", "canary")));
        } else {
            when(cache.findAllUpstreamListBySelectorId(SELECTOR)).thenReturn(null);
        }
        AbstractHttpClientPlugin<String> plugin = plugin(uri -> Mono.error(new ConnectException("connection refused")));
        try (MockedStatic<UpstreamCacheManager> ignored = mockCache()) {
            StepVerifier.withVirtualTime(() -> plugin.execute(exchange, current -> Mono.empty()))
                    .thenAwait(Duration.ofSeconds(2)).expectErrorSatisfies(this::assertExhausted).verify();
        }
        assertEquals(List.of(uri("primary")), attempts);
        verify(cache).findAllUpstreamListBySelectorId(SELECTOR);
    }

    @ParameterizedTest
    @ValueSource(strings = {"default", "fixed", "exponential"})
    void currentStrategyKeepsTheOriginalEndpoint(final String backoff) {
        ServerWebExchange exchange = exchange(backoff, "canary", 1);
        exchange.getAttributes().put(Constants.RETRY_STRATEGY, "current");
        AbstractHttpClientPlugin<String> plugin = plugin(uri -> attempts.size() == 1
                ? Mono.error(new TimeoutException("transient failure")) : Mono.just("success"));
        try (MockedStatic<UpstreamCacheManager> ignored = mockCache()) {
            StepVerifier.withVirtualTime(() -> plugin.execute(exchange, current -> Mono.empty()))
                    .thenAwait(Duration.ofSeconds(2)).verifyComplete();
        }
        assertEquals(List.of(uri("primary"), uri("primary")), attempts);
        verifyNoInteractions(cache);
    }

    @ParameterizedTest
    @ValueSource(strings = {"default", "fixed", "exponential"})
    void legacyFailoverUsesTheGrayFilteredPool(final String backoff) {
        ServerWebExchange exchange = exchange(backoff, "canary", 1);
        exchange.getAttributes().remove(Constants.SHENYU_CANARY_PARTITION);
        exchange.getAttributes().remove(Constants.SHENYU_CANARY_LABELS);
        Upstream gray = upstream("gray", "canary");
        gray.setGray(true);
        when(cache.findUpstreamListBySelectorId(SELECTOR)).thenReturn(List.of(gray));
        when(cache.findAllUpstreamListBySelectorId(SELECTOR)).thenReturn(List.of(upstream("ordinary", "stable"), gray));
        AbstractHttpClientPlugin<String> plugin = plugin(uri -> uri.getHost().equals("primary")
                ? Mono.error(new ConnectException("connection refused")) : Mono.just("success"));
        try (MockedStatic<UpstreamCacheManager> ignored = mockCache()) {
            StepVerifier.withVirtualTime(() -> plugin.execute(exchange, current -> Mono.empty()))
                    .thenAwait(Duration.ofSeconds(2)).verifyComplete();
        }
        assertEquals(List.of(uri("primary"), uri("gray")), attempts);
        verify(cache).findUpstreamListBySelectorId(SELECTOR);
        verify(cache, never()).findAllUpstreamListBySelectorId(anyString());
    }

    @ParameterizedTest
    @CsvSource({"default,true", "fixed,true", "exponential,true", "default,false", "fixed,false", "exponential,false"})
    void probeOrZeroRetriesNeverFailOver(final String backoff, final boolean probe) {
        ServerWebExchange exchange = exchange(backoff, "canary", probe ? 3 : 0);
        exchange.getAttributes().put(UpstreamCircuitBreaker.HALF_OPEN_PROBE, probe);
        ConnectException failure = new ConnectException("connection refused");
        AbstractHttpClientPlugin<String> plugin = plugin(uri -> Mono.error(failure));
        try (MockedStatic<UpstreamCacheManager> ignored = mockCache()) {
            StepVerifier.create(plugin.execute(exchange, current -> Mono.empty())).expectErrorMatches(error -> error == failure).verify();
        }
        assertEquals(List.of(uri("primary")), attempts);
        verifyNoInteractions(cache);
    }

    @ParameterizedTest
    @CsvSource({"fixed,POST", "exponential,POST", "fixed,GET", "exponential,GET"})
    void backoffPreservesMethodAndFailureEligibility(final String backoff, final String method) {
        ServerWebExchange exchange = exchange(backoff, "canary", 2).mutate().request(request -> request.method(HttpMethod.valueOf(method))).build();
        Throwable failure = "POST".equals(method) ? new ConnectException("connection refused") : new IllegalArgumentException("permanent failure");
        AbstractHttpClientPlugin<String> plugin = plugin(uri -> Mono.error(failure));
        try (MockedStatic<UpstreamCacheManager> ignored = mockCache()) {
            StepVerifier.create(plugin.execute(exchange, current -> Mono.empty())).expectErrorMatches(error -> error == failure).verify();
        }
        assertEquals(List.of(uri("primary")), attempts);
        verifyNoInteractions(cache);
    }

    @ParameterizedTest
    @ValueSource(strings = {"fixed", "exponential"})
    void cancellationDuringBackoffDoesNotSelectAnotherNode(final String backoff) {
        ServerWebExchange exchange = exchange(backoff, "canary", 2);
        AbstractHttpClientPlugin<String> plugin = plugin(uri -> Mono.error(new ConnectException("connection refused")));
        try (MockedStatic<UpstreamCacheManager> ignored = mockCache()) {
            StepVerifier.withVirtualTime(() -> plugin.execute(exchange, current -> Mono.empty()))
                    .thenAwait(Duration.ofMillis(200)).thenCancel().verify();
        }
        assertEquals(List.of(uri("primary")), attempts);
        verifyNoInteractions(cache);
    }

    @ParameterizedTest
    @ValueSource(strings = {"default", "fixed", "exponential"})
    void perAttemptTimeoutAndBackoffAllowEveryConfiguredRetry(final String backoff) {
        ServerWebExchange exchange = exchange(backoff, "canary", 2);
        exchange.getAttributes().put(Constants.HTTP_TIME_OUT, 100L);
        when(cache.findAllUpstreamListBySelectorId(SELECTOR))
                .thenReturn(List.of(upstream("second", "canary")), List.of(upstream("added", "canary")));
        AbstractHttpClientPlugin<String> plugin = plugin(uri -> uri.getHost().equals("added") ? Mono.just("success") : Mono.never());
        try (MockedStatic<UpstreamCacheManager> ignored = mockCache()) {
            StepVerifier.withVirtualTime(() -> plugin.execute(exchange, current -> Mono.empty()))
                    .thenAwait(Duration.ofSeconds(10)).verifyComplete();
        }
        assertEquals(List.of(uri("primary"), uri("second"), uri("added")), attempts);
    }

    @ParameterizedTest
    @ValueSource(strings = {"default", "fixed", "exponential"})
    void retryStateIsFreshForEachSubscription(final String backoff) {
        ServerWebExchange exchange = exchange(backoff, "canary", 1);
        when(cache.findAllUpstreamListBySelectorId(SELECTOR)).thenReturn(List.of(upstream("second", "canary")));
        AbstractHttpClientPlugin<String> plugin = plugin(uri -> uri.getHost().equals("primary")
                ? Mono.error(new ConnectException("connection refused")) : Mono.just("success"));
        try (MockedStatic<UpstreamCacheManager> ignored = mockCache()) {
            StepVerifier.withVirtualTime(() -> {
                Mono<Void> response = plugin.execute(exchange, current -> Mono.empty());
                return response.then(response);
            }).thenAwait(Duration.ofSeconds(10)).verifyComplete();
        }
        assertEquals(List.of(uri("primary"), uri("second"), uri("primary"), uri("second")), attempts);
    }

    private static Stream<Arguments> partitionRetries() {
        return Stream.of("default", "fixed", "exponential").flatMap(backoff -> Stream.of("stable", "canary")
                .flatMap(partition -> Stream.of(false, true).map(serverError -> Arguments.of(backoff, partition, serverError))));
    }

    private ServerWebExchange exchange(final String backoff, final String partition, final int retries) {
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/test?mode=retry"));
        exchange.getAttributes().put(Constants.CONTEXT, new ShenyuContext());
        exchange.getAttributes().put(Constants.HTTP_URI, uri("primary"));
        exchange.getAttributes().put(Constants.REWRITE_URI, "/test");
        exchange.getAttributes().put(Constants.DIVIDE_SELECTOR_ID, SELECTOR);
        exchange.getAttributes().put(Constants.LOAD_BALANCE, "roundRobin");
        exchange.getAttributes().put(Constants.RETRY_STRATEGY, "failover");
        exchange.getAttributes().put(Constants.HTTP_RETRY_BACK_OFF_SPEC, backoff);
        exchange.getAttributes().put(Constants.HTTP_RETRY, retries);
        exchange.getAttributes().put(Constants.HTTP_TIME_OUT, 10000L);
        exchange.getAttributes().put(Constants.SHENYU_CANARY_PARTITION, partition);
        exchange.getAttributes().put(Constants.SHENYU_CANARY_LABELS, Map.of("release", partition, "zone", "east"));
        return exchange;
    }

    private Upstream upstream(final String host, final String partition) {
        Upstream upstream = Upstream.builder().protocol("http://").url(host).weight(100).build();
        upstream.setLabels(Map.of("release", partition, "zone", "east"));
        nodes.add(upstream);
        return upstream;
    }

    private URI uri(final String host) {
        return URI.create("http://" + host + "/test?mode=retry");
    }

    private MockedStatic<UpstreamCacheManager> mockCache() {
        MockedStatic<UpstreamCacheManager> mocked = mockStatic(UpstreamCacheManager.class);
        mocked.when(UpstreamCacheManager::getInstance).thenReturn(cache);
        return mocked;
    }

    private AbstractHttpClientPlugin<String> plugin(final Function<URI, Mono<String>> response) {
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

    private void assertExhausted(final Throwable failure) {
        ResponseStatusException exception = assertInstanceOf(ResponseStatusException.class, failure);
        assertEquals(HttpStatus.SERVICE_UNAVAILABLE, exception.getStatusCode());
        assertEquals("CANNOT_FIND_HEALTHY_UPSTREAM_URL_AFTER_FAILOVER", exception.getReason());
    }
}
