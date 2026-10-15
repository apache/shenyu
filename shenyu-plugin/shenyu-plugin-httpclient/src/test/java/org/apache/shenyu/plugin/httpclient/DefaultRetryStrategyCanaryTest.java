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
import org.apache.shenyu.plugin.api.context.CanaryContext;
import org.apache.shenyu.plugin.base.circuitbreaker.UpstreamCircuitBreaker;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.springframework.http.HttpStatus;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.net.InetSocketAddress;
import java.net.URI;
import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.TimeoutException;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/**
 * Failover stays in the saved actual partition and uses current healthy nodes.
 */
class DefaultRetryStrategyCanaryTest {

    @Test
    void testCanaryFailureRetriesOnlyCanaryAndStopsWhenExhausted() {
        ServerWebExchange exchange = exchange("canary", "c1:8080");
        AbstractHttpClientPlugin<String> client = mock(AbstractHttpClientPlugin.class);
        when(client.doRequest(eq(exchange), anyString(), eq(URI.create("http://c2:8080/test")), any()))
                .thenReturn(Mono.error(new IllegalStateException("c2 failed")));
        UpstreamCacheManager manager = mock(UpstreamCacheManager.class);
        when(manager.findUpstreamListBySelectorId("selector")).thenReturn(List.of(
                upstream("c1:8080", "canary"), upstream("c2:8080", "canary"), upstream("s1:8080", "stable")));
        try (MockedStatic<UpstreamCacheManager> cache = cache(manager)) {
            verifyUnavailable(new DefaultRetryStrategy<>(client).execute(Mono.error(new IllegalStateException("c1 failed")), exchange, Duration.ofSeconds(1), 3));
        }
        verify(manager, times(2)).findUpstreamListBySelectorId("selector");
        verify(client, times(1)).doRequest(eq(exchange), anyString(), any(), any());
        verify(client, never()).doRequest(eq(exchange), anyString(), eq(URI.create("http://s1:8080/test")), any());
        assertEquals("canary", exchange.getAttribute(Constants.SHENYU_CANARY_PARTITION));
    }

    @Test
    void testInitialStableFallbackRemainsStable() {
        ServerWebExchange exchange = exchange("stable", "s1:8080");
        CanaryContext observation = new CanaryContext("selector", "rule", "canary", "stable", CanaryContext.CANARY_POOL_EMPTY, null, 80_000);
        exchange.getAttributes().put(Constants.SHENYU_CANARY_CONTEXT, observation);
        AbstractHttpClientPlugin<String> client = mock(AbstractHttpClientPlugin.class);
        when(client.doRequest(eq(exchange), anyString(), eq(URI.create("http://s2:8080/test")), any())).thenReturn(Mono.just("stable response"));
        UpstreamCacheManager manager = mock(UpstreamCacheManager.class);
        when(manager.findUpstreamListBySelectorId("selector")).thenReturn(List.of(
                upstream("s1:8080", "stable"), upstream("s2:8080", "stable"), upstream("c1:8080", "canary")));
        try (MockedStatic<UpstreamCacheManager> cache = cache(manager)) {
            StepVerifier.create(new DefaultRetryStrategy<>(client).execute(Mono.error(new IllegalStateException("s1 failed")), exchange, Duration.ofSeconds(1), 2))
                    .expectNext("stable response").verifyComplete();
        }
        verify(client).doRequest(eq(exchange), anyString(), eq(URI.create("http://s2:8080/test")), any());
        assertEquals("stable", exchange.getAttribute(Constants.SHENYU_CANARY_PARTITION));
        assertSame(observation, exchange.getAttribute(Constants.SHENYU_CANARY_CONTEXT));
    }

    @Test
    void testRetryReadsLatestLabelsAndHealthyNodes() {
        ServerWebExchange exchange = exchange("canary", "c1:8080");
        AbstractHttpClientPlugin<String> client = mock(AbstractHttpClientPlugin.class);
        when(client.doRequest(eq(exchange), anyString(), eq(URI.create("http://c2:8080/test")), any()))
                .thenReturn(Mono.error(new IllegalStateException("c2 failed")));
        when(client.doRequest(eq(exchange), anyString(), eq(URI.create("http://c3:8080/test")), any())).thenReturn(Mono.just("new canary"));
        UpstreamCacheManager manager = mock(UpstreamCacheManager.class);
        when(manager.findUpstreamListBySelectorId("selector"))
                .thenReturn(List.of(upstream("c1:8080", "canary"), upstream("c2:8080", "canary")))
                .thenReturn(List.of(upstream("c2:8080", "stable"), upstream("c3:8080", "canary"), upstream("s1:8080", "stable")));
        try (MockedStatic<UpstreamCacheManager> cache = cache(manager)) {
            StepVerifier.create(new DefaultRetryStrategy<>(client).execute(Mono.error(new IllegalStateException("c1 failed")), exchange, Duration.ofSeconds(1), 3))
                    .expectNext("new canary").verifyComplete();
        }
        verify(manager, times(2)).findUpstreamListBySelectorId("selector");
        verify(client, times(2)).doRequest(eq(exchange), anyString(), any(), any());
    }

    @Test
    void testRemovedSelectorAndMissingLabelSnapshotFailClosed() {
        ServerWebExchange exchange = exchange("canary", "c1:8080");
        AbstractHttpClientPlugin<String> client = mock(AbstractHttpClientPlugin.class);
        UpstreamCacheManager manager = mock(UpstreamCacheManager.class);
        try (MockedStatic<UpstreamCacheManager> cache = cache(manager)) {
            verifyUnavailable(new DefaultRetryStrategy<>(client).execute(Mono.error(new IllegalStateException("failed")), exchange, Duration.ofSeconds(1), 1));
            when(manager.findUpstreamListBySelectorId("selector")).thenReturn(List.of(upstream("c2:8080", "canary")));
            exchange.getAttributes().remove(Constants.SHENYU_CANARY_LABELS);
            verifyUnavailable(new DefaultRetryStrategy<>(client).execute(Mono.error(new IllegalStateException("failed")), exchange, Duration.ofSeconds(1), 1));
        }
        verifyNoInteractions(client);
    }

    @Test
    void testSpecifyDomainKeepsLegacyFailoverWithoutPartition() {
        ServerWebExchange exchange = exchange(null, "override:9090");
        exchange = exchange.mutate().request(request -> request.header(Constants.SPECIFY_DOMAIN, "override:9090")).build();
        AbstractHttpClientPlugin<String> client = mock(AbstractHttpClientPlugin.class);
        when(client.doRequest(eq(exchange), anyString(), eq(URI.create("http://legacy:8080/test")), any())).thenReturn(Mono.just("legacy response"));
        UpstreamCacheManager manager = mock(UpstreamCacheManager.class);
        when(manager.findLegacyUpstreamListBySelectorId("selector")).thenReturn(List.of(upstream("legacy:8080", "stable")));
        try (MockedStatic<UpstreamCacheManager> cache = cache(manager)) {
            StepVerifier.create(new DefaultRetryStrategy<>(client).execute(Mono.error(new IllegalStateException("override failed")), exchange, Duration.ofSeconds(1), 1))
                    .expectNext("legacy response").verifyComplete();
        }
    }

    @Test
    void testLegacyFailoverDoesNotUseFullCanaryView() {
        ServerWebExchange exchange = exchange(null, "gray:8080");
        AbstractHttpClientPlugin<String> client = mock(AbstractHttpClientPlugin.class);
        UpstreamCacheManager manager = mock(UpstreamCacheManager.class);
        when(manager.findUpstreamListBySelectorId("selector")).thenReturn(List.of(upstream("stable:8080", "stable")));
        when(manager.findLegacyUpstreamListBySelectorId("selector")).thenReturn(List.of());
        try (MockedStatic<UpstreamCacheManager> cache = cache(manager)) {
            verifyUnavailable(new DefaultRetryStrategy<>(client).execute(Mono.error(new IllegalStateException("gray failed")), exchange, Duration.ofSeconds(1), 3));
        }
        verifyNoInteractions(client);
        verify(manager, times(1)).findLegacyUpstreamListBySelectorId("selector");
        verify(manager, never()).findUpstreamListBySelectorId("selector");
    }

    @Test
    void testCurrentRetryPreservesPartitionWithoutReselecting() {
        ServerWebExchange exchange = exchange("canary", "c1:8080");
        exchange.getAttributes().put(Constants.RETRY_STRATEGY, "current");
        AbstractHttpClientPlugin<String> client = mock(AbstractHttpClientPlugin.class);
        UpstreamCacheManager manager = mock(UpstreamCacheManager.class);
        try (MockedStatic<UpstreamCacheManager> cache = cache(manager)) {
            StepVerifier.create(new DefaultRetryStrategy<>(client).execute(Mono.error(new TimeoutException("timeout")), exchange, Duration.ofSeconds(1), 1))
                    .expectErrorMatches(error -> error instanceof ResponseStatusException
                            && ((ResponseStatusException) error).getStatusCode() == HttpStatus.REQUEST_TIMEOUT).verify();
        }
        verifyNoInteractions(client, manager);
        assertEquals("canary", exchange.getAttribute(Constants.SHENYU_CANARY_PARTITION));
    }

    @Test
    void testRetryPreservesDefaultPortExclusionAndCircuitBreakerWithinPartition() {
        ServerWebExchange exchange = exchange("canary", "C1");
        Upstream blocked = upstream("c2:8080", "canary");
        UpstreamCircuitBreaker breaker = UpstreamCircuitBreaker.getInstance();
        String key = UpstreamCircuitBreaker.buildKey("selector", blocked);
        for (int i = 0; i < UpstreamCircuitBreaker.DEFAULT_FAILURE_THRESHOLD; i++) {
            breaker.recordFailure(key);
        }
        AbstractHttpClientPlugin<String> client = mock(AbstractHttpClientPlugin.class);
        UpstreamCacheManager manager = mock(UpstreamCacheManager.class);
        when(manager.findUpstreamListBySelectorId("selector")).thenReturn(List.of(
                upstream("c1:80", "canary"), blocked, upstream("s1:8080", "stable")));
        try (MockedStatic<UpstreamCacheManager> cache = cache(manager)) {
            verifyUnavailable(new DefaultRetryStrategy<>(client).execute(Mono.error(new IllegalStateException("failed")), exchange, Duration.ofSeconds(1), 2));
            verifyNoInteractions(client);
        } finally {
            breaker.reset(key);
        }
    }

    private void verifyUnavailable(final Mono<String> result) {
        StepVerifier.create(result).expectErrorMatches(error -> error instanceof ResponseStatusException
                && ((ResponseStatusException) error).getStatusCode() == HttpStatus.SERVICE_UNAVAILABLE).verify();
    }

    private ServerWebExchange exchange(final String partition, final String initialHost) {
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/test")
                .remoteAddress(new InetSocketAddress("127.0.0.1", 12345)));
        exchange.getAttributes().put(Constants.DIVIDE_SELECTOR_ID, "selector");
        exchange.getAttributes().put(Constants.LOAD_BALANCE, "random");
        exchange.getAttributes().put(Constants.RETRY_STRATEGY, "failover");
        exchange.getAttributes().put(Constants.REWRITE_URI, "/test");
        exchange.getAttributes().put(Constants.HTTP_URI, URI.create("http://" + initialHost + "/test"));
        if (Objects.nonNull(partition)) {
            exchange.getAttributes().put(Constants.SHENYU_CANARY_PARTITION, partition);
            exchange.getAttributes().put(Constants.SHENYU_CANARY_LABELS, Map.of("release", partition));
        }
        return exchange;
    }

    private MockedStatic<UpstreamCacheManager> cache(final UpstreamCacheManager manager) {
        MockedStatic<UpstreamCacheManager> cache = mockStatic(UpstreamCacheManager.class);
        cache.when(UpstreamCacheManager::getInstance).thenReturn(manager);
        return cache;
    }

    private Upstream upstream(final String url, final String partition) {
        Upstream upstream = Upstream.builder().url(url).protocol("http://").weight(100).build();
        upstream.setLabels(Map.of("release", partition));
        return upstream;
    }
}
