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
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.base.circuitbreaker.UpstreamCircuitBreaker;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.net.URI;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Verifies that each HTTP attempt records its own circuit-breaker outcome (#8156):
 * the initial attempt's failure is recorded immediately against the initial upstream,
 * and a successful failover can never credit the failed upstream's breaker.
 */
class InitialAttemptBreakerAttributionTest {

    private static final String SELECTOR_ID = "selector-8156";

    private final Map<String, Mono<String>> stubsByHost = new HashMap<>();

    private final AtomicInteger initialCalls = new AtomicInteger();

    private ServerWebExchange exchange;

    private ShenyuPluginChain chain;

    private Upstream initial;

    private Upstream standby;

    private String initialKey;

    private String standbyKey;

    private TestHttpClientPlugin plugin;

    @BeforeEach
    void setUp() {
        plugin = new TestHttpClientPlugin();
        chain = mock(ShenyuPluginChain.class);
        exchange = MockServerWebExchange.from(MockServerHttpRequest.get("http://localhost/test").build());
        exchange.getAttributes().put(Constants.CONTEXT, new ShenyuContext());
        exchange.getAttributes().put(Constants.HTTP_URI, URI.create("http://a:8080/test"));
        exchange.getAttributes().put(Constants.DIVIDE_SELECTOR_ID, SELECTOR_ID);
        exchange.getAttributes().put(Constants.RETRY_STRATEGY, org.apache.shenyu.common.enums.RetryEnum.FAILOVER.getName());
        exchange.getAttributes().put(Constants.HTTP_RETRY, 1);
        exchange.getAttributes().put(Constants.HTTP_TIME_OUT, 3000L);
        exchange.getAttributes().put(Constants.LOAD_BALANCE, "random");
        initial = Upstream.builder().protocol("http://").url("a:8080").build();
        standby = Upstream.builder().protocol("http://").url("b:8080").build();
        initialKey = UpstreamCircuitBreaker.buildKey(SELECTOR_ID, initial);
        standbyKey = UpstreamCircuitBreaker.buildKey(SELECTOR_ID, standby);
        exchange.getAttributes().put(UpstreamCircuitBreaker.INITIAL_BREAKER_KEY, initialKey);
        UpstreamCircuitBreaker.getInstance().reset(initialKey);
        UpstreamCircuitBreaker.getInstance().reset(standbyKey);
        UpstreamCacheManager.getInstance().submit(SELECTOR_ID, Arrays.asList(initial, standby));
    }

    @AfterEach
    void tearDown() {
        UpstreamCircuitBreaker.getInstance().reset(initialKey);
        UpstreamCircuitBreaker.getInstance().reset(standbyKey);
        UpstreamCacheManager.getInstance().removeByKey(SELECTOR_ID);
    }

    @Test
    void initialFailureIsRecordedImmediatelyAgainstTheInitialUpstream() {
        // no failover target: the cache only knows the initial upstream, so every
        // logical request fails with it and there is nothing to serve instead
        stubsByHost.put("a", Mono.error(new RuntimeException("down")));
        UpstreamCacheManager.getInstance().submit(SELECTOR_ID, java.util.Collections.singletonList(initial));
        whenChainErrors();

        final UpstreamCircuitBreaker breaker = UpstreamCircuitBreaker.getInstance();
        for (int i = 0; i < UpstreamCircuitBreaker.DEFAULT_FAILURE_THRESHOLD; i++) {
            StepVerifier.create(plugin.execute(exchange, chain))
                    .expectErrorSatisfies(e -> assertTrue(
                            e instanceof RuntimeException
                                    || e instanceof org.springframework.web.server.ResponseStatusException,
                            "unexpected error " + e))
                    .verify();
        }
        assertTrue(breaker.isBlocking(initialKey),
                "repeated initial failures must open the initial upstream's breaker");
        assertFalse(breaker.isBlocking(standbyKey));
    }

    @Test
    void successfulFailoverCannotCreditTheFailedUpstreamBreaker() {
        // the initial attempt fails (headers stage), the failover target succeeds
        stubsByHost.put("a", Mono.error(new RuntimeException("down")));
        stubsByHost.put("b", Mono.just("served-by-b"));
        whenChainCompletes();

        final UpstreamCircuitBreaker breaker = UpstreamCircuitBreaker.getInstance();
        for (int i = 0; i < UpstreamCircuitBreaker.DEFAULT_FAILURE_THRESHOLD + 1; i++) {
            StepVerifier.create(plugin.execute(exchange, chain))
                    .verifyComplete();
        }
        assertEquals(UpstreamCircuitBreaker.DEFAULT_FAILURE_THRESHOLD + 1, initialCalls.get(),
                "every logical request must have failed over from the initial upstream");
        assertTrue(breaker.isBlocking(initialKey),
                "the failed initial upstream's breaker must open even though the logical request succeeded");
        assertFalse(breaker.isBlocking(standbyKey), "the serving upstream must stay closed");
    }

    @Test
    void initialSuccessIsCreditedWhenNoFailoverHappens() {
        stubsByHost.put("a", Mono.just("ok"));
        whenChainCompletes();

        final UpstreamCircuitBreaker breaker = UpstreamCircuitBreaker.getInstance();
        breaker.recordFailure(initialKey);
        StepVerifier.create(plugin.execute(exchange, chain))
                .verifyComplete();
        assertFalse(breaker.isBlocking(initialKey),
                "a succeeded initial attempt clears its own consecutive failures");
    }

    private void whenChainErrors() {
        when(chain.execute(exchange)).thenReturn(Mono.error(new RuntimeException("propagated")));
    }

    private void whenChainCompletes() {
        when(chain.execute(exchange)).thenReturn(Mono.empty());
    }

    /**
     * Serves the per-host stubs; the host of the attempt URI selects the behavior.
     */
    private final class TestHttpClientPlugin extends AbstractHttpClientPlugin<String> {

        TestHttpClientPlugin() {
            super(256 * 1024);
        }

        @Override
        protected Mono<String> doRequest(final ServerWebExchange ex, final String httpMethod,
                                         final URI uri, final Flux<DataBuffer> body) {
            if ("a".equals(uri.getHost())) {
                initialCalls.incrementAndGet();
            }
            final Mono<String> stub = stubsByHost.get(uri.getHost());
            return Objects.nonNull(stub) ? stub : Mono.error(new RuntimeException("no stub for " + uri.getHost()));
        }

        @Override
        public int getOrder() {
            return 100;
        }
    }
}
