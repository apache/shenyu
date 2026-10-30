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
import org.apache.shenyu.plugin.base.utils.UpstreamAttemptOwner;
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
import reactor.test.scheduler.VirtualTimeScheduler;

import java.net.ConnectException;
import java.net.URI;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Function;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;
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
        when(cacheManager.findUpstreamListBySelectorId("selector-7475")).thenReturn(Collections.singletonList(upstream));

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
        when(cacheManager.findUpstreamListBySelectorId("selector-7475")).thenReturn(Arrays.asList(failed, standby));
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

    /**
     * DividePlugin installs the attempt owner before the retry chain runs and releases
     * whoever holds it when the whole logical request (response body included) ends.
     */
    private UpstreamAttemptOwner armAttemptOwner(final ServerWebExchange exchange) {
        final UpstreamAttemptOwner owner = new UpstreamAttemptOwner(upstream -> upstream.getInflight().decrementAndGet());
        exchange.getAttributes().put(UpstreamAttemptOwner.ATTRIBUTE, owner);
        return owner;
    }

    private void terminalRelease(final UpstreamAttemptOwner owner) {
        owner.releaseCurrent();
    }

    @Test
    void testFailoverReleasesP2cInflightOnMultiCandidateRetry() {
        AbstractHttpClientPlugin<String> plugin = mock(AbstractHttpClientPlugin.class);
        ServerWebExchange exchange = createFailoverExchange("http://localhost:8080/test?mode=retry");
        exchange.getAttributes().put(Constants.LOAD_BALANCE, "p2c");
        exchange.getAttributes().put(Constants.DIVIDE_SELECTOR_ID, "selector-7975-multi");
        Upstream failed = Upstream.builder().url("localhost:8080").build();
        Upstream standbyA = Upstream.builder().protocol("http://").url("localhost:8081").build();
        Upstream standbyB = Upstream.builder().protocol("http://").url("localhost:8082").build();
        UpstreamCacheManager cacheManager = mock(UpstreamCacheManager.class);
        when(cacheManager.findUpstreamListBySelectorId("selector-7975-multi")).thenReturn(Arrays.asList(failed, standbyA, standbyB));
        when(plugin.getCachedRequestBody(exchange)).thenReturn(Flux.empty());
        when(plugin.doRequest(eq(exchange), eq("GET"), any(URI.class), any())).thenReturn(Mono.just("success"));
        final long baselineA = standbyA.getInflight().get();
        final long baselineB = standbyB.getInflight().get();
        final UpstreamAttemptOwner owner = armAttemptOwner(exchange);

        try (MockedStatic<UpstreamCacheManager> cacheMock = mockStatic(UpstreamCacheManager.class)) {
            cacheMock.when(UpstreamCacheManager::getInstance).thenReturn(cacheManager);
            StepVerifier.create(new DefaultRetryStrategy<>(plugin).execute(Mono.error(new TimeoutException("upstream failed")),
                            exchange, Duration.ofSeconds(5), 2))
                    .expectNext("success")
                    .verifyComplete();
        }
        // Two candidates remain after exclusion, so failover takes P2C's doSelect path and increments the
        // picked standby. The succeeded attempt's ownership moved to the terminal release (#8158), which
        // runs when the response body is done - not when the response headers arrive.
        assertEquals(baselineA + baselineB + 1, standbyA.getInflight().get() + standbyB.getInflight().get(),
                "the succeeded attempt stays reserved while the response body is open");
        terminalRelease(owner);
        assertEquals(baselineA, standbyA.getInflight().get());
        assertEquals(baselineB, standbyB.getInflight().get());
    }

    @Test
    void testFailoverReleasesP2cInflightOnSingletonRetry() {
        AbstractHttpClientPlugin<String> plugin = mock(AbstractHttpClientPlugin.class);
        ServerWebExchange exchange = createFailoverExchange("http://localhost:8080/test?mode=retry");
        exchange.getAttributes().put(Constants.LOAD_BALANCE, "p2c");
        exchange.getAttributes().put(Constants.DIVIDE_SELECTOR_ID, "selector-7975-single");
        Upstream failed = Upstream.builder().url("localhost:8080").build();
        Upstream onlyStandby = Upstream.builder().protocol("http://").url("localhost:8083").build();
        UpstreamCacheManager cacheManager = mock(UpstreamCacheManager.class);
        when(cacheManager.findUpstreamListBySelectorId("selector-7975-single")).thenReturn(Arrays.asList(failed, onlyStandby));
        when(plugin.getCachedRequestBody(exchange)).thenReturn(Flux.empty());
        when(plugin.doRequest(eq(exchange), eq("GET"), any(URI.class), any())).thenReturn(Mono.just("success"));
        final long baseline = onlyStandby.getInflight().get();
        final UpstreamAttemptOwner owner = armAttemptOwner(exchange);

        try (MockedStatic<UpstreamCacheManager> cacheMock = mockStatic(UpstreamCacheManager.class)) {
            cacheMock.when(UpstreamCacheManager::getInstance).thenReturn(cacheManager);
            StepVerifier.create(new DefaultRetryStrategy<>(plugin).execute(Mono.error(new TimeoutException("upstream failed")),
                            exchange, Duration.ofSeconds(5), 2))
                    .expectNext("success")
                    .verifyComplete();
        }
        // Only one candidate remains after exclusion, so P2C selection is short-circuited; the singleton
        // increment paired by #7974 stays reserved until the terminal release (#8158), then returns to
        // its baseline instead of leaking on every singleton failover. See #7975.
        assertEquals(baseline + 1, onlyStandby.getInflight().get(),
                "the succeeded attempt stays reserved while the response body is open");
        terminalRelease(owner);
        assertEquals(baseline, onlyStandby.getInflight().get());
    }

    @Test
    void testFailoverReservesAndReleasesLeastActiveAccounting() {
        AbstractHttpClientPlugin<String> plugin = mock(AbstractHttpClientPlugin.class);
        ServerWebExchange exchange = createFailoverExchange("http://localhost:8080/test?mode=retry");
        exchange.getAttributes().put(Constants.LOAD_BALANCE, "leastActive");
        Upstream failed = Upstream.builder().url("localhost:8080").build();
        Upstream standby = Upstream.builder().protocol("http://").url("localhost:8081").build();
        UpstreamCacheManager cacheManager = mock(UpstreamCacheManager.class);
        when(cacheManager.findUpstreamListBySelectorId("selector-7475")).thenReturn(Arrays.asList(failed, standby));
        when(plugin.getCachedRequestBody(exchange)).thenReturn(Flux.empty());
        AtomicLong inflightDuringResend = new AtomicLong();
        when(plugin.doRequest(eq(exchange), eq("GET"), any(URI.class), any())).thenReturn(Mono.defer(() -> {
            inflightDuringResend.set(standby.getInflight().get());
            return Mono.just("success");
        }));

        final UpstreamAttemptOwner owner = armAttemptOwner(exchange);

        try (MockedStatic<UpstreamCacheManager> cacheMock = mockStatic(UpstreamCacheManager.class)) {
            cacheMock.when(UpstreamCacheManager::getInstance).thenReturn(cacheManager);
            StepVerifier.create(new DefaultRetryStrategy<>(plugin).execute(Mono.error(new TimeoutException("upstream failed")),
                    exchange, Duration.ofSeconds(5), 2))
                    .expectNext("success")
                    .verifyComplete();
        }
        // the attempt is counted while it runs; a succeeded attempt stays reserved until the
        // terminal release runs when the response body is done (#8158)
        assertEquals(2, inflightDuringResend.get());
        assertEquals(2, standby.getInflight().get(), "the succeeded attempt must stay reserved");
        terminalRelease(owner);
        assertEquals(1, standby.getInflight().get());
    }

    @Test
    void testFailoverReleasesTheFailedAttemptImmediately() {
        AbstractHttpClientPlugin<String> plugin = mock(AbstractHttpClientPlugin.class);
        ServerWebExchange exchange = createFailoverExchange("http://localhost:8080/test?mode=retry");
        exchange.getAttributes().put(Constants.LOAD_BALANCE, "leastActive");
        exchange.getAttributes().put(Constants.DIVIDE_SELECTOR_ID, "selector-8158-fail");
        Upstream failed = Upstream.builder().url("localhost:8080").build();
        Upstream standby = Upstream.builder().protocol("http://").url("localhost:8081").build();
        UpstreamCacheManager cacheManager = mock(UpstreamCacheManager.class);
        when(cacheManager.findUpstreamListBySelectorId("selector-8158-fail")).thenReturn(Arrays.asList(failed, standby));
        when(plugin.getCachedRequestBody(exchange)).thenReturn(Flux.empty());
        when(plugin.doRequest(eq(exchange), eq("GET"), any(URI.class), any())).thenReturn(Mono.error(new TimeoutException("retry failed too")));
        final UpstreamAttemptOwner owner = armAttemptOwner(exchange);

        try (MockedStatic<UpstreamCacheManager> cacheMock = mockStatic(UpstreamCacheManager.class)) {
            cacheMock.when(UpstreamCacheManager::getInstance).thenReturn(cacheManager);
            StepVerifier.create(new DefaultRetryStrategy<>(plugin).execute(Mono.error(new TimeoutException("upstream failed")),
                    exchange, Duration.ofSeconds(5), 2))
                    .expectError(org.springframework.web.server.ResponseStatusException.class)
                    .verify();
        }
        // the failed attempt released itself: a failing upstream must not keep looking busy (#8158)
        assertEquals(1, standby.getInflight().get(), "a failed retry attempt releases its own reservation");
        assertNull(owner.current(), "no ownership survives a failed attempt");
    }

    @Test
    void testFailoverKeepsTheSucceededAttemptOwnedUntilTheTerminalRelease() {
        AbstractHttpClientPlugin<String> plugin = mock(AbstractHttpClientPlugin.class);
        ServerWebExchange exchange = createFailoverExchange("http://localhost:8080/test?mode=retry");
        exchange.getAttributes().put(Constants.LOAD_BALANCE, "leastActive");
        exchange.getAttributes().put(Constants.DIVIDE_SELECTOR_ID, "selector-8158-own");
        Upstream failed = Upstream.builder().url("localhost:8080").build();
        Upstream standby = Upstream.builder().protocol("http://").url("localhost:8081").build();
        UpstreamCacheManager cacheManager = mock(UpstreamCacheManager.class);
        when(cacheManager.findUpstreamListBySelectorId("selector-8158-own")).thenReturn(Arrays.asList(failed, standby));
        when(plugin.getCachedRequestBody(exchange)).thenReturn(Flux.empty());
        when(plugin.doRequest(eq(exchange), eq("GET"), any(URI.class), any())).thenReturn(Mono.just("success"));
        final UpstreamAttemptOwner owner = armAttemptOwner(exchange);

        try (MockedStatic<UpstreamCacheManager> cacheMock = mockStatic(UpstreamCacheManager.class)) {
            cacheMock.when(UpstreamCacheManager::getInstance).thenReturn(cacheManager);
            StepVerifier.create(new DefaultRetryStrategy<>(plugin).execute(Mono.error(new TimeoutException("upstream failed")),
                    exchange, Duration.ofSeconds(5), 2))
                    .expectNext("success")
                    .verifyComplete();
        }
        // the ownership traveled to the retry target: the terminal release (response body done)
        // must release the standby, not the failed initial upstream (#8158)
        assertSame(standby, owner.current(), "the serving upstream owns the attempt");
        assertEquals(2, standby.getInflight().get());
        terminalRelease(owner);
        assertEquals(1, standby.getInflight().get());
    }

    @Test
    void testTwoCandidateFailoverReleasesEachAttemptAndHoldsTheFinalOne() {
        AbstractHttpClientPlugin<String> plugin = mock(AbstractHttpClientPlugin.class);
        ServerWebExchange exchange = createFailoverExchange("http://localhost:8080/test?mode=retry");
        exchange.getAttributes().put(Constants.LOAD_BALANCE, "leastActive");
        exchange.getAttributes().put(Constants.DIVIDE_SELECTOR_ID, "selector-8158-chain");
        Upstream initial = Upstream.builder().protocol("http://").url("localhost:8080").build();
        Upstream retryA = Upstream.builder().protocol("http://").url("localhost:8081").build();
        Upstream retryB = Upstream.builder().protocol("http://").url("localhost:8082").build();
        initial.getInflight().incrementAndGet();
        UpstreamCacheManager cacheManager = mock(UpstreamCacheManager.class);
        when(cacheManager.findUpstreamListBySelectorId("selector-8158-chain"))
                .thenReturn(Collections.singletonList(retryA), Collections.singletonList(retryB));
        when(plugin.getCachedRequestBody(exchange)).thenReturn(Flux.empty());
        when(plugin.doRequest(eq(exchange), eq("GET"), any(URI.class), any())).thenAnswer(invocation -> {
            final URI uri = invocation.getArgument(2);
            return "8081".equals(String.valueOf(uri.getPort()))
                    ? Mono.error(new TimeoutException("first retry failed"))
                    : Mono.just("success");
        });
        final UpstreamAttemptOwner owner = armAttemptOwner(exchange);
        owner.move(initial);

        try (MockedStatic<UpstreamCacheManager> cacheMock = mockStatic(UpstreamCacheManager.class)) {
            cacheMock.when(UpstreamCacheManager::getInstance).thenReturn(cacheManager);
            StepVerifier.create(new DefaultRetryStrategy<>(plugin).execute(Mono.error(new TimeoutException("initial failed")),
                    exchange, Duration.ofSeconds(5), 2))
                    .expectNext("success")
                    .verifyComplete();
        }
        assertEquals(1, initial.getInflight().get(), "the failed initial attempt returns to its baseline");
        assertEquals(1, retryA.getInflight().get(), "the failed retry attempt returns to its baseline");
        assertEquals(2, retryB.getInflight().get(), "the final serving attempt stays reserved");
        assertEquals(retryB, owner.current());

        terminalRelease(owner);
        assertEquals(1, retryB.getInflight().get());
    }

    @Test
    void testBackoffReleasesTheFailedAttemptBeforeResend() {
        AbstractHttpClientPlugin<String> plugin = mock(AbstractHttpClientPlugin.class);
        ServerWebExchange exchange = createFailoverExchange("http://localhost:8080/test?mode=retry");
        exchange.getAttributes().put(Constants.LOAD_BALANCE, "leastActive");
        exchange.getAttributes().put(Constants.DIVIDE_SELECTOR_ID, "selector-8158-backoff");
        Upstream initial = Upstream.builder().protocol("http://").url("localhost:8080").build();
        Upstream retry = Upstream.builder().protocol("http://").url("localhost:8081").build();
        initial.getInflight().incrementAndGet();
        UpstreamCacheManager cacheManager = mock(UpstreamCacheManager.class);
        when(cacheManager.findUpstreamListBySelectorId("selector-8158-backoff")).thenReturn(Collections.singletonList(retry));
        when(plugin.getCachedRequestBody(exchange)).thenReturn(Flux.empty());
        when(plugin.doRequest(eq(exchange), eq("GET"), any(URI.class), any())).thenReturn(Mono.just("success"));
        final UpstreamAttemptOwner owner = armAttemptOwner(exchange);
        owner.move(initial);

        try (MockedStatic<UpstreamCacheManager> cacheMock = mockStatic(UpstreamCacheManager.class)) {
            cacheMock.when(UpstreamCacheManager::getInstance).thenReturn(cacheManager);
            StepVerifier.withVirtualTime(() -> new DefaultRetryStrategy<>(plugin).executeFailover(
                            Mono.error(new TimeoutException("initial failed")), exchange, Duration.ofSeconds(5), 1,
                            Duration.ofSeconds(2), throwable -> true))
                    .thenAwait(Duration.ofSeconds(1))
                    .then(() -> assertEquals(1, initial.getInflight().get(),
                            "a failed attempt is released before the backoff window"))
                    .thenAwait(Duration.ofSeconds(1))
                    .expectNext("success")
                    .verifyComplete();
        }
        assertEquals(retry, owner.current());
        terminalRelease(owner);
        assertEquals(1, retry.getInflight().get());
    }

    @Test
    void testSynchronousResendFailureReleasesTheSelectedAttempt() {
        AbstractHttpClientPlugin<String> plugin = mock(AbstractHttpClientPlugin.class);
        ServerWebExchange exchange = createFailoverExchange("http://localhost:8080/test?mode=retry");
        exchange.getAttributes().put(Constants.LOAD_BALANCE, "leastActive");
        exchange.getAttributes().put(Constants.DIVIDE_SELECTOR_ID, "selector-8158-sync");
        Upstream standby = Upstream.builder().protocol("http://").url("localhost:8081").build();
        UpstreamCacheManager cacheManager = mock(UpstreamCacheManager.class);
        when(cacheManager.findUpstreamListBySelectorId("selector-8158-sync")).thenReturn(Collections.singletonList(standby));
        when(plugin.getCachedRequestBody(exchange)).thenReturn(Flux.empty());
        when(plugin.doRequest(eq(exchange), eq("GET"), any(URI.class), any()))
                .thenThrow(new IllegalStateException("request construction failed"));
        final UpstreamAttemptOwner owner = armAttemptOwner(exchange);

        try (MockedStatic<UpstreamCacheManager> cacheMock = mockStatic(UpstreamCacheManager.class)) {
            cacheMock.when(UpstreamCacheManager::getInstance).thenReturn(cacheManager);
            StepVerifier.create(new DefaultRetryStrategy<>(plugin).execute(Mono.error(new TimeoutException("initial failed")),
                    exchange, Duration.ofSeconds(5), 1))
                    .expectError()
                    .verify();
        }
        assertEquals(1, standby.getInflight().get(), "a synchronous failure releases the selected attempt");
        assertNull(owner.current());
    }

    @Test
    void testLegacyRetryAccountingIsPreservedWithoutAnOwner() {
        AbstractHttpClientPlugin<String> plugin = mock(AbstractHttpClientPlugin.class);
        ServerWebExchange exchange = createFailoverExchange("http://localhost:8080/test?mode=retry");
        exchange.getAttributes().put(Constants.LOAD_BALANCE, "leastActive");
        exchange.getAttributes().put(Constants.DIVIDE_SELECTOR_ID, "selector-8158-legacy");
        Upstream standby = Upstream.builder().protocol("http://").url("localhost:8081").build();
        UpstreamCacheManager cacheManager = mock(UpstreamCacheManager.class);
        when(cacheManager.findUpstreamListBySelectorId("selector-8158-legacy")).thenReturn(Collections.singletonList(standby));
        when(plugin.getCachedRequestBody(exchange)).thenReturn(Flux.empty());
        when(plugin.doRequest(eq(exchange), eq("GET"), any(URI.class), any())).thenReturn(Mono.just("success"));

        try (MockedStatic<UpstreamCacheManager> cacheMock = mockStatic(UpstreamCacheManager.class)) {
            cacheMock.when(UpstreamCacheManager::getInstance).thenReturn(cacheManager);
            StepVerifier.create(new DefaultRetryStrategy<>(plugin).execute(Mono.error(new TimeoutException("initial failed")),
                    exchange, Duration.ofSeconds(5), 1))
                    .expectNext("success")
                    .verifyComplete();
        }
        // no Divide owner: keep the pre-#8158 release on retry completion
        assertEquals(1, standby.getInflight().get());
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
        when(cacheManager.findUpstreamListBySelectorId("selector-7475")).thenReturn(Arrays.asList(failed, standby));
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
        when(cacheManager.findUpstreamListBySelectorId("selector-7475")).thenReturn(Arrays.asList(failed, standby));
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
        when(cacheManager.findUpstreamListBySelectorId("selector-7475")).thenReturn(Arrays.asList(failed, standby));
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
    @CsvSource({"fixed, false", "fixed, true", "exponential, false", "exponential, true"})
    void testBackoffFailoverWaitsBeforeSendingToStandby(final String type, final boolean serverError) {
        ServerWebExchange exchange = createBackoffFailoverExchange(type, HttpMethod.GET, 1);
        URI primaryUri = exchange.getAttribute(Constants.HTTP_URI);
        URI standbyUri = URI.create("http://localhost:8081/test?mode=retry");
        Throwable failure = serverError ? new ShenyuUpstreamStatusException(503) : new ConnectException("connection refused");
        List<URI> attempts = new ArrayList<>();
        AbstractHttpClientPlugin<String> plugin = recordingPlugin(attempts,
                uri -> primaryUri.equals(uri) ? Mono.error(failure) : Mono.just("success"));
        Upstream standby = Upstream.builder().url("localhost:8081").build();
        UpstreamCacheManager cacheManager = mock(UpstreamCacheManager.class);
        when(cacheManager.findUpstreamListBySelectorId(type + "-failover")).thenReturn(Collections.singletonList(standby));

        try (MockedStatic<UpstreamCacheManager> cacheMock = mockStatic(UpstreamCacheManager.class)) {
            cacheMock.when(UpstreamCacheManager::getInstance).thenReturn(cacheManager);
            StepVerifier.withVirtualTime(() -> plugin.execute(exchange, ignored -> Mono.empty()))
                    .thenAwait(Duration.ofMillis("fixed".equals(type) ? 1999 : 499))
                    .then(() -> {
                        assertEquals(Collections.singletonList(primaryUri), attempts);
                        verifyNoInteractions(cacheManager);
                    })
                    .thenAwait(Duration.ofMillis("fixed".equals(type) ? 1 : 251))
                    .verifyComplete();
        }
        assertEquals(Arrays.asList(primaryUri, standbyUri), attempts);
    }

    @ParameterizedTest
    @CsvSource({"fixed, POST, false", "fixed, PUT, true", "fixed, GET, false",
        "exponential, POST, false", "exponential, POST, true", "exponential, PUT, false", "exponential, PUT, true", "exponential, GET, false"})
    void testBackoffFailoverPreservesRetryEligibility(final String type, final String method, final boolean serverError) {
        ServerWebExchange exchange = createBackoffFailoverExchange(type, HttpMethod.valueOf(method), 1);
        Throwable failure = "GET".equals(method) ? new IllegalArgumentException("permanent failure")
                : serverError ? new ShenyuUpstreamStatusException(503) : new ConnectException("connection refused");
        List<URI> attempts = new ArrayList<>();
        AbstractHttpClientPlugin<String> plugin = recordingPlugin(attempts, uri -> Mono.error(failure));
        UpstreamCacheManager cacheManager = mock(UpstreamCacheManager.class);

        try (MockedStatic<UpstreamCacheManager> cacheMock = mockStatic(UpstreamCacheManager.class)) {
            cacheMock.when(UpstreamCacheManager::getInstance).thenReturn(cacheManager);
            StepVerifier.create(plugin.execute(exchange, ignored -> Mono.empty()))
                    .expectErrorMatches(error -> error == failure)
                    .verify();
        }
        assertEquals(Collections.singletonList(exchange.getAttribute(Constants.HTTP_URI)), attempts);
        verifyNoInteractions(cacheManager);
    }

    @ParameterizedTest
    @ValueSource(strings = {"fixed", "exponential"})
    void testBackoffFailoverRetriesAcrossDistinctUpstreams(final String type) {
        ServerWebExchange exchange = createBackoffFailoverExchange(type, HttpMethod.GET, 2);
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
        when(cacheManager.findUpstreamListBySelectorId(type + "-failover"))
                .thenReturn(Arrays.asList(primary, standby), Arrays.asList(primary, standby, healthy));

        try (MockedStatic<UpstreamCacheManager> cacheMock = mockStatic(UpstreamCacheManager.class)) {
            cacheMock.when(UpstreamCacheManager::getInstance).thenReturn(cacheManager);
            StepVerifier.withVirtualTime(() -> plugin.execute(exchange, ignored -> Mono.empty()))
                    .thenAwait(Duration.ofSeconds(4))
                    .verifyComplete();
        } finally {
            UpstreamCircuitBreaker.getInstance().reset(UpstreamCircuitBreaker.buildKey(type + "-failover", standby));
        }
        assertEquals(Arrays.asList(primaryUri, standbyUri, healthyUri), attempts);
    }

    @Test
    void testExponentialFailoverPreservesBackoffAndLastFailureWhenRetriesAreExhausted() {
        ServerWebExchange exchange = createBackoffFailoverExchange("exponential", HttpMethod.GET, 6);
        exchange.getAttributes().put(Constants.DIVIDE_SELECTOR_ID, "exponential-exhausted");
        URI primaryUri = exchange.getAttribute(Constants.HTTP_URI);
        List<URI> attempts = new ArrayList<>();
        List<Long> attemptTimes = new ArrayList<>();
        ShenyuUpstreamStatusException primaryFailure = new ShenyuUpstreamStatusException(503);
        ConnectException failure = new ConnectException("last upstream failed");
        AbstractHttpClientPlugin<String> plugin = recordingPlugin(attempts, uri -> {
            attemptTimes.add(VirtualTimeScheduler.get().now(TimeUnit.MILLISECONDS));
            return Mono.error(primaryUri.equals(uri) ? primaryFailure : failure);
        });
        List<Upstream> upstreams = new ArrayList<>();
        for (int i = 0; i < 7; i++) {
            upstreams.add(Upstream.builder().url("localhost:" + (8080 + i)).build());
        }
        UpstreamCacheManager cacheManager = mock(UpstreamCacheManager.class);
        when(cacheManager.findUpstreamListBySelectorId("exponential-exhausted")).thenReturn(upstreams);

        try (MockedStatic<UpstreamCacheManager> cacheMock = mockStatic(UpstreamCacheManager.class)) {
            cacheMock.when(UpstreamCacheManager::getInstance).thenReturn(cacheManager);
            StepVerifier.withVirtualTime(() -> plugin.execute(exchange, ignored -> Mono.empty()))
                    .thenAwait(Duration.ofMinutes(1))
                    .expectErrorMatches(error -> error == failure)
                    .verify(Duration.ofSeconds(5));
        } finally {
            upstreams.forEach(upstream -> UpstreamCircuitBreaker.getInstance()
                    .reset(UpstreamCircuitBreaker.buildKey("exponential-exhausted", upstream)));
        }
        assertEquals(primaryUri, attempts.get(0));
        assertEquals(7, attempts.size());
        assertEquals(7, new HashSet<>(attempts).size());
        long[] minimumDelays = {500, 500, 1000, 2000, 2500, 2500};
        long[] maximumDelays = {750, 1500, 3000, 5000, 5000, 5000};
        for (int i = 0; i < minimumDelays.length; i++) {
            long delay = attemptTimes.get(i + 1) - attemptTimes.get(i);
            assertTrue(delay >= minimumDelays[i] && delay <= maximumDelays[i], "Unexpected retry delay: " + delay);
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"missing", "empty", "current"})
    void testExponentialFailoverWithNoStandby(final String topology) {
        ServerWebExchange exchange = createBackoffFailoverExchange("exponential", HttpMethod.GET, 3);
        List<URI> attempts = new ArrayList<>();
        AbstractHttpClientPlugin<String> plugin = recordingPlugin(attempts, uri -> Mono.error(new ConnectException("connection refused")));
        UpstreamCacheManager cacheManager = mock(UpstreamCacheManager.class);
        List<Upstream> upstreams = "missing".equals(topology) ? null : "empty".equals(topology)
                ? Collections.emptyList() : Collections.singletonList(Upstream.builder().url("localhost:8080").build());
        when(cacheManager.findUpstreamListBySelectorId("exponential-failover")).thenReturn(upstreams);

        try (MockedStatic<UpstreamCacheManager> cacheMock = mockStatic(UpstreamCacheManager.class)) {
            cacheMock.when(UpstreamCacheManager::getInstance).thenReturn(cacheManager);
            StepVerifier.withVirtualTime(() -> plugin.execute(exchange, ignored -> Mono.empty()))
                    .thenAwait(Duration.ofMinutes(1))
                    .expectErrorSatisfies(this::assertFailoverExhausted)
                    .verify();
        }
        assertEquals(Collections.singletonList(exchange.getAttribute(Constants.HTTP_URI)), attempts);
        verify(cacheManager).findUpstreamListBySelectorId("exponential-failover");
    }

    @Test
    void testExponentialFailoverCancellationDuringBackoffDoesNotSelectStandby() {
        ServerWebExchange exchange = createBackoffFailoverExchange("exponential", HttpMethod.GET, 1);
        List<URI> attempts = new ArrayList<>();
        AbstractHttpClientPlugin<String> plugin = recordingPlugin(attempts, uri -> Mono.error(new ConnectException("connection refused")));
        UpstreamCacheManager cacheManager = mock(UpstreamCacheManager.class);

        try (MockedStatic<UpstreamCacheManager> cacheMock = mockStatic(UpstreamCacheManager.class)) {
            cacheMock.when(UpstreamCacheManager::getInstance).thenReturn(cacheManager);
            StepVerifier.withVirtualTime(() -> plugin.execute(exchange, ignored -> Mono.empty()))
                    .thenAwait(Duration.ofMillis(499))
                    .thenCancel()
                    .verify();
        }
        assertEquals(Collections.singletonList(exchange.getAttribute(Constants.HTTP_URI)), attempts);
        verifyNoInteractions(cacheManager);
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void testExponentialFailoverCancelsStandbyAndReleasesAccounting(final boolean timeout) {
        ServerWebExchange exchange = createBackoffFailoverExchange("exponential", HttpMethod.GET, 1);
        exchange.getAttributes().put(Constants.DIVIDE_SELECTOR_ID, "exponential-cancel");
        exchange.getAttributes().put(Constants.LOAD_BALANCE, "leastActive");
        exchange.getAttributes().put(Constants.HTTP_TIME_OUT, timeout ? 100L : 10000L);
        URI primaryUri = exchange.getAttribute(Constants.HTTP_URI);
        Upstream standby = Upstream.builder().url("localhost:8081").build();
        long baseline = standby.getInflight().get();
        AtomicBoolean cancelled = new AtomicBoolean();
        List<URI> attempts = new ArrayList<>();
        AbstractHttpClientPlugin<String> plugin = recordingPlugin(attempts, uri -> primaryUri.equals(uri)
                ? Mono.error(new ConnectException("connection refused")) : Mono.<String>never().doOnCancel(() -> cancelled.set(true)));
        UpstreamCacheManager cacheManager = mock(UpstreamCacheManager.class);
        when(cacheManager.findUpstreamListBySelectorId("exponential-cancel")).thenReturn(Collections.singletonList(standby));

        try (MockedStatic<UpstreamCacheManager> cacheMock = mockStatic(UpstreamCacheManager.class)) {
            cacheMock.when(UpstreamCacheManager::getInstance).thenReturn(cacheManager);
            if (timeout) {
                StepVerifier.withVirtualTime(() -> plugin.execute(exchange, ignored -> Mono.empty()))
                        .thenAwait(Duration.ofSeconds(1))
                        .expectErrorSatisfies(error -> assertEquals(HttpStatus.GATEWAY_TIMEOUT,
                                assertInstanceOf(ResponseStatusException.class, error).getStatusCode()))
                        .verify();
            } else {
                StepVerifier.withVirtualTime(() -> plugin.execute(exchange, ignored -> Mono.empty()))
                        .thenAwait(Duration.ofSeconds(1))
                        .then(() -> assertEquals(baseline + 1, standby.getInflight().get()))
                        .thenCancel()
                        .verify();
            }
        } finally {
            UpstreamCircuitBreaker.getInstance().reset(UpstreamCircuitBreaker.buildKey("exponential-cancel", standby));
        }
        assertEquals(2, attempts.size());
        assertTrue(cancelled.get());
        assertEquals(baseline, standby.getInflight().get());
    }

    @Test
    void testExponentialFailoverResetsStateForEachSubscription() {
        ServerWebExchange exchange = createBackoffFailoverExchange("exponential", HttpMethod.GET, 1);
        AbstractHttpClientPlugin<String> plugin = mock(AbstractHttpClientPlugin.class);
        when(plugin.getCachedRequestBody(exchange)).thenReturn(Flux.empty());
        when(plugin.doRequest(eq(exchange), eq("GET"), any(URI.class), any())).thenReturn(Mono.just("success"));
        AtomicInteger primaryAttempts = new AtomicInteger();
        Mono<String> primary = Mono.defer(() -> {
            primaryAttempts.incrementAndGet();
            return Mono.error(new ConnectException("connection refused"));
        });
        UpstreamCacheManager cacheManager = mock(UpstreamCacheManager.class);
        when(cacheManager.findUpstreamListBySelectorId("exponential-failover"))
                .thenReturn(Collections.singletonList(Upstream.builder().url("localhost:8081").build()));

        try (MockedStatic<UpstreamCacheManager> cacheMock = mockStatic(UpstreamCacheManager.class)) {
            cacheMock.when(UpstreamCacheManager::getInstance).thenReturn(cacheManager);
            StepVerifier.withVirtualTime(() -> {
                Mono<String> response = new ExponentialRetryBackoffStrategy<>(plugin).execute(primary, exchange, Duration.ofSeconds(10), 1);
                return response.concatWith(response);
            })
                    .thenAwait(Duration.ofSeconds(2))
                    .expectNext("success", "success")
                    .verifyComplete();
        }
        assertEquals(2, primaryAttempts.get());
        verify(plugin, times(2)).doRequest(eq(exchange), eq("GET"), eq(URI.create("http://localhost:8081/test?mode=retry")), any());
    }

    private ServerWebExchange createBackoffFailoverExchange(final String type, final HttpMethod method, final int retryTimes) {
        ServerWebExchange exchange = createFailoverExchange("http://localhost:8080/test?mode=retry")
                .mutate().request(request -> request.method(method)).build();
        exchange.getAttributes().put(Constants.CONTEXT, new ShenyuContext());
        exchange.getAttributes().put(Constants.HTTP_RETRY_BACK_OFF_SPEC, type);
        exchange.getAttributes().put(Constants.HTTP_RETRY, retryTimes);
        exchange.getAttributes().put(Constants.HTTP_TIME_OUT, 10000L);
        exchange.getAttributes().put(Constants.DIVIDE_SELECTOR_ID, type + "-failover");
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
