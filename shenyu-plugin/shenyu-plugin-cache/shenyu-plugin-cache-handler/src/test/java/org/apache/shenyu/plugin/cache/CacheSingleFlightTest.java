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

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.CacheRuleHandle;
import org.apache.shenyu.common.dto.convert.selector.CacheUpstream;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.cache.cache.ApplicationConfigCache;
import org.apache.shenyu.plugin.cache.cache.ResponseSingleFlight;
import org.apache.shenyu.plugin.cache.handler.CachePluginDataHandler;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.context.ApplicationContext;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatusCode;
import org.springframework.http.MediaType;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.Disposable;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Verifies that concurrent misses on one key share a single upstream fill.
 */
class CacheSingleFlightTest {

    private static final String SELECTOR = "cache-single-flight-selector";

    private static final String URI = "http://gateway.example/cache-single-flight";

    private final CachePlugin plugin = new CachePlugin();

    private final SelectorData selector = new SelectorData();

    private final RuleData rule = new RuleData();

    private ICache cache;

    @BeforeEach
    void setUp() {
        selector.setId(SELECTOR);
        selector.setHandle("{\"cacheType\":\"memory\",\"url\":\"memory://single-flight\"}");
        rule.setSelectorId(SELECTOR);
        rule.setId("cache-single-flight-rule");
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
        Singleton.INST.single(ResponseSingleFlight.class, ResponseSingleFlight.disabled());
        SpringBeanUtils.getInstance().setApplicationContext(null);
    }

    @Test
    void testConcurrentMissesShareOneFill() throws Exception {
        Singleton.INST.single(ResponseSingleFlight.class, new ResponseSingleFlight(64, 64, 5000L, 30000L, 1048576L));
        Sinks.One<Void> gate = Sinks.one();
        CountDownLatch leaderStarted = new CountDownLatch(1);
        AtomicInteger calls = new AtomicInteger();
        ShenyuPluginChain chain = exchange -> {
            calls.incrementAndGet();
            exchange.getResponse().getHeaders().setContentType(MediaType.APPLICATION_JSON);
            leaderStarted.countDown();
            return gate.asMono().then(write(exchange, "shared"));
        };
        MockServerWebExchange leader = request();
        Disposable leaderSubscription = execute(leader, chain).subscribe();
        try {
            assertTrue(leaderStarted.await(5, TimeUnit.SECONDS));
            MockServerWebExchange follower = request();

            StepVerifier.create(execute(follower, chain))
                    .then(() -> gate.tryEmitEmpty())
                    .verifyComplete();

            assertEquals(1, calls.get());
            assertEquals("shared", follower.getResponse().getBodyAsString().block());
            assertEquals("shared", leader.getResponse().getBodyAsString().block());
        } finally {
            leaderSubscription.dispose();
        }
    }

    @Test
    void testDisabledCoalescingForwardsEveryRequest() throws Exception {
        Singleton.INST.single(ResponseSingleFlight.class, ResponseSingleFlight.disabled());
        Sinks.One<Void> gate = Sinks.one();
        CountDownLatch leaderStarted = new CountDownLatch(1);
        AtomicInteger calls = new AtomicInteger();
        ShenyuPluginChain chain = exchange -> {
            calls.incrementAndGet();
            exchange.getResponse().getHeaders().setContentType(MediaType.APPLICATION_JSON);
            leaderStarted.countDown();
            return gate.asMono().then(write(exchange, "body-" + calls.get()));
        };
        MockServerWebExchange leader = request();
        Disposable leaderSubscription = execute(leader, chain).subscribe();
        try {
            assertTrue(leaderStarted.await(5, TimeUnit.SECONDS));
            MockServerWebExchange follower = request();

            StepVerifier.create(execute(follower, chain))
                    .then(() -> gate.tryEmitEmpty())
                    .verifyComplete();

            assertEquals(2, calls.get());
            assertEquals("body-2", follower.getResponse().getBodyAsString().block());
        } finally {
            leaderSubscription.dispose();
        }
    }

    @Test
    void testFollowerFillsWhenLeaderResponseCannotBeShared() throws Exception {
        Singleton.INST.single(ResponseSingleFlight.class, new ResponseSingleFlight(64, 64, 5000L, 30000L, 1048576L));
        Sinks.One<Void> gate = Sinks.one();
        CountDownLatch leaderStarted = new CountDownLatch(1);
        AtomicInteger calls = new AtomicInteger();
        ShenyuPluginChain chain = exchange -> {
            int call = calls.incrementAndGet();
            exchange.getResponse().getHeaders().setContentType(MediaType.APPLICATION_JSON);
            if (call == 1) {
                exchange.getResponse().getHeaders().add(HttpHeaders.SET_COOKIE, "session=private");
                leaderStarted.countDown();
                return gate.asMono().then(write(exchange, "private"));
            }
            return write(exchange, "shared");
        };
        MockServerWebExchange leader = request();
        Disposable leaderSubscription = execute(leader, chain).subscribe();
        try {
            assertTrue(leaderStarted.await(5, TimeUnit.SECONDS));
            MockServerWebExchange follower = request();

            StepVerifier.create(execute(follower, chain))
                    .then(() -> gate.tryEmitEmpty())
                    .verifyComplete();

            assertEquals(2, calls.get());
            assertEquals("shared", follower.getResponse().getBodyAsString().block());
        } finally {
            leaderSubscription.dispose();
        }
    }

    @Test
    void testFollowerFillsAfterWaitingTooLong() throws Exception {
        Singleton.INST.single(ResponseSingleFlight.class, new ResponseSingleFlight(64, 64, 50L, 30000L, 1048576L));
        Sinks.One<Void> gate = Sinks.one();
        CountDownLatch leaderStarted = new CountDownLatch(1);
        AtomicInteger calls = new AtomicInteger();
        ShenyuPluginChain chain = exchange -> {
            int call = calls.incrementAndGet();
            exchange.getResponse().getHeaders().setContentType(MediaType.APPLICATION_JSON);
            if (call == 1) {
                leaderStarted.countDown();
                return gate.asMono().then(write(exchange, "slow"));
            }
            return write(exchange, "fast");
        };
        MockServerWebExchange leader = request();
        Disposable leaderSubscription = execute(leader, chain).subscribe();
        try {
            assertTrue(leaderStarted.await(5, TimeUnit.SECONDS));
            MockServerWebExchange follower = request();

            StepVerifier.create(execute(follower, chain)).verifyComplete();

            assertEquals(2, calls.get());
            assertEquals("fast", follower.getResponse().getBodyAsString().block());
        } finally {
            leaderSubscription.dispose();
        }
    }

    @Test
    void testFollowerFillsWhenResponseExceedsShareLimit() throws Exception {
        Singleton.INST.single(ResponseSingleFlight.class, new ResponseSingleFlight(64, 64, 5000L, 30000L, 4L));
        Sinks.One<Void> gate = Sinks.one();
        CountDownLatch leaderStarted = new CountDownLatch(1);
        AtomicInteger calls = new AtomicInteger();
        ShenyuPluginChain chain = exchange -> {
            int call = calls.incrementAndGet();
            exchange.getResponse().getHeaders().setContentType(MediaType.APPLICATION_JSON);
            if (call == 1) {
                leaderStarted.countDown();
                return gate.asMono().then(write(exchange, "larger-than-limit"));
            }
            return write(exchange, "fast");
        };
        MockServerWebExchange leader = request();
        Disposable leaderSubscription = execute(leader, chain).subscribe();
        try {
            assertTrue(leaderStarted.await(5, TimeUnit.SECONDS));
            MockServerWebExchange follower = request();

            StepVerifier.create(execute(follower, chain))
                    .then(() -> gate.tryEmitEmpty())
                    .verifyComplete();

            assertEquals(2, calls.get());
            assertEquals("fast", follower.getResponse().getBodyAsString().block());
        } finally {
            leaderSubscription.dispose();
        }
    }

    @Test
    void testFollowerFillsWhenNoWaiterSlotIsAvailable() throws Exception {
        Singleton.INST.single(ResponseSingleFlight.class, new ResponseSingleFlight(64, 0, 5000L, 30000L, 1048576L));
        Sinks.One<Void> gate = Sinks.one();
        CountDownLatch leaderStarted = new CountDownLatch(1);
        AtomicInteger calls = new AtomicInteger();
        ShenyuPluginChain chain = exchange -> {
            int call = calls.incrementAndGet();
            exchange.getResponse().getHeaders().setContentType(MediaType.APPLICATION_JSON);
            if (call == 1) {
                leaderStarted.countDown();
                return gate.asMono().then(write(exchange, "slow"));
            }
            return write(exchange, "fast");
        };
        MockServerWebExchange leader = request();
        Disposable leaderSubscription = execute(leader, chain).subscribe();
        try {
            assertTrue(leaderStarted.await(5, TimeUnit.SECONDS));
            MockServerWebExchange follower = request();

            StepVerifier.create(execute(follower, chain)).verifyComplete();

            assertEquals(2, calls.get());
            assertEquals("fast", follower.getResponse().getBodyAsString().block());
        } finally {
            leaderSubscription.dispose();
        }
    }

    @Test
    void testDistinctKeysFillIndependently() throws Exception {
        Singleton.INST.single(ResponseSingleFlight.class, new ResponseSingleFlight(64, 64, 5000L, 30000L, 1048576L));
        Sinks.One<Void> gate = Sinks.one();
        CountDownLatch leaderStarted = new CountDownLatch(1);
        AtomicInteger calls = new AtomicInteger();
        ShenyuPluginChain chain = exchange -> {
            calls.incrementAndGet();
            exchange.getResponse().getHeaders().setContentType(MediaType.APPLICATION_JSON);
            leaderStarted.countDown();
            return gate.asMono().then(write(exchange, "body"));
        };
        MockServerWebExchange leader = request();
        Disposable leaderSubscription = execute(leader, chain).subscribe();
        try {
            assertTrue(leaderStarted.await(5, TimeUnit.SECONDS));
            MockServerWebExchange other = MockServerWebExchange.from(
                    MockServerHttpRequest.get(URI + "/other").build());

            StepVerifier.create(execute(other, chain))
                    .then(() -> gate.tryEmitEmpty())
                    .verifyComplete();

            assertEquals(2, calls.get());
            assertEquals("body", other.getResponse().getBodyAsString().block());
        } finally {
            leaderSubscription.dispose();
        }
    }

    @Test
    void testCancelledFollowerReleasesItsSlot() throws Exception {
        Singleton.INST.single(ResponseSingleFlight.class, new ResponseSingleFlight(64, 64, 5000L, 30000L, 1048576L));
        Sinks.One<Void> gate = Sinks.one();
        CountDownLatch leaderStarted = new CountDownLatch(1);
        ShenyuPluginChain chain = exchange -> {
            exchange.getResponse().getHeaders().setContentType(MediaType.APPLICATION_JSON);
            leaderStarted.countDown();
            return gate.asMono().then(write(exchange, "shared"));
        };
        MockServerWebExchange leader = request();
        Disposable leaderSubscription = execute(leader, chain).subscribe();
        try {
            assertTrue(leaderStarted.await(5, TimeUnit.SECONDS));
            MockServerWebExchange cancelled = request();
            StepVerifier.create(execute(cancelled, chain)).thenCancel().verify();

            MockServerWebExchange follower = request();
            StepVerifier.create(execute(follower, chain))
                    .then(() -> gate.tryEmitEmpty())
                    .verifyComplete();

            assertEquals("shared", follower.getResponse().getBodyAsString().block());
        } finally {
            leaderSubscription.dispose();
        }
    }

    @Test
    void testFillsWhenNoKeySlotIsAvailable() throws Exception {
        Singleton.INST.single(ResponseSingleFlight.class, new ResponseSingleFlight(1, 64, 50L, 30000L, 1048576L));
        Sinks.One<Void> gate = Sinks.one();
        CountDownLatch leaderStarted = new CountDownLatch(1);
        AtomicInteger calls = new AtomicInteger();
        ShenyuPluginChain chain = exchange -> {
            int call = calls.incrementAndGet();
            exchange.getResponse().getHeaders().setContentType(MediaType.APPLICATION_JSON);
            if (call == 1) {
                leaderStarted.countDown();
                return gate.asMono().then(write(exchange, "slow"));
            }
            return write(exchange, "fast-" + call);
        };
        MockServerWebExchange leader = request();
        Disposable leaderSubscription = execute(leader, chain).subscribe();
        try {
            assertTrue(leaderStarted.await(5, TimeUnit.SECONDS));
            MockServerWebExchange otherKey = MockServerWebExchange.from(
                    MockServerHttpRequest.get(URI + "/capacity").build());

            StepVerifier.create(execute(otherKey, chain)).verifyComplete();

            assertEquals(2, calls.get());
            assertEquals("fast-2", otherKey.getResponse().getBodyAsString().block());
        } finally {
            leaderSubscription.dispose();
        }
    }

    @Test
    void testFollowerFillsWhenLeaderChainFails() throws Exception {
        Singleton.INST.single(ResponseSingleFlight.class, new ResponseSingleFlight(64, 64, 5000L, 30000L, 1048576L));
        Sinks.One<Void> gate = Sinks.one();
        CountDownLatch leaderStarted = new CountDownLatch(1);
        AtomicInteger calls = new AtomicInteger();
        IllegalStateException failure = new IllegalStateException("upstream failed");
        ShenyuPluginChain chain = exchange -> {
            int call = calls.incrementAndGet();
            exchange.getResponse().getHeaders().setContentType(MediaType.APPLICATION_JSON);
            if (call == 1) {
                leaderStarted.countDown();
                return gate.asMono().then(Mono.error(failure));
            }
            return write(exchange, "recovered");
        };
        MockServerWebExchange leader = request();
        Disposable leaderSubscription = execute(leader, chain).subscribe();
        try {
            assertTrue(leaderStarted.await(5, TimeUnit.SECONDS));
            MockServerWebExchange follower = request();

            StepVerifier.create(execute(follower, chain))
                    .then(() -> gate.tryEmitEmpty())
                    .verifyComplete();

            assertEquals(2, calls.get());
            assertEquals("recovered", follower.getResponse().getBodyAsString().block());
        } finally {
            leaderSubscription.dispose();
        }
    }

    @ParameterizedTest
    @ValueSource(ints = {201, 404})
    void testFollowerReplaysLeaderStatus(final int status) throws Exception {
        Singleton.INST.single(ResponseSingleFlight.class, new ResponseSingleFlight(64, 64, 5000L, 30000L, 1048576L));
        Sinks.One<Void> gate = Sinks.one();
        CountDownLatch leaderStarted = new CountDownLatch(1);
        AtomicInteger calls = new AtomicInteger();
        ShenyuPluginChain chain = exchange -> {
            calls.incrementAndGet();
            exchange.getResponse().setStatusCode(HttpStatusCode.valueOf(status));
            exchange.getResponse().getHeaders().setContentType(MediaType.APPLICATION_JSON);
            exchange.getResponse().getHeaders().set("X-Upstream", "leader");
            leaderStarted.countDown();
            return gate.asMono().then(write(exchange, "body"));
        };
        MockServerWebExchange leader = request();
        Disposable leaderSubscription = execute(leader, chain).subscribe();
        try {
            assertTrue(leaderStarted.await(5, TimeUnit.SECONDS));
            MockServerWebExchange follower = request();

            StepVerifier.create(execute(follower, chain))
                    .then(() -> gate.tryEmitEmpty())
                    .verifyComplete();

            assertEquals(1, calls.get());
            assertEquals(status, follower.getResponse().getStatusCode().value());
            assertEquals("leader", follower.getResponse().getHeaders().getFirst("X-Upstream"));
            assertEquals("body", follower.getResponse().getBodyAsString().block());
        } finally {
            leaderSubscription.dispose();
        }
    }

    private Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        return plugin.doExecute(exchange, chain, selector, rule);
    }

    private static Mono<Void> write(final ServerWebExchange exchange, final String body) {
        return exchange.getResponse().writeWith(Mono.just(exchange.getResponse().bufferFactory().wrap(body.getBytes(StandardCharsets.UTF_8))));
    }

    private static MockServerWebExchange request() {
        return MockServerWebExchange.from(MockServerHttpRequest.get(URI).build());
    }
}
