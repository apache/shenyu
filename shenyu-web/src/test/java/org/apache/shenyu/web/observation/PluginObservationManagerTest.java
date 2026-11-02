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

package org.apache.shenyu.web.observation;

import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.metrics.PluginObservationEvent;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.web.handler.ShenyuWebHandler;
import org.junit.jupiter.api.Test;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;
import reactor.core.scheduler.Schedulers;
import reactor.test.StepVerifier;

import java.time.Duration;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BiFunction;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class PluginObservationManagerTest {

    @Test
    void disabledPathAndRootAssemblyHaveNoObservationOrRequestEffects() {
        AtomicInteger entered = new AtomicInteger();
        ShenyuWebHandler handler = handler(PluginObservationManager.disabled(), plugin((exchange, chain) -> {
            entered.incrementAndGet();
            return chain.execute(exchange);
        }));
        Mono<Void> root = handler.handle(exchange());
        assertEquals(0, entered.get());
        assertFalse(handler.observationStats().enabled());
        root.block();
        root.block();
        assertEquals(2, entered.get());
    }

    @Test
    void publisherLifetimeIncludesDelayedSuccessorAndKeepsConstructionHooks() throws Exception {
        List<PluginObservationEvent> events = new CopyOnWriteArrayList<>();
        CountDownLatch exported = new CountDownLatch(6);
        List<String> hooks = new CopyOnWriteArrayList<>();
        Sinks.Empty<Void> gate = Sinks.empty();
        ShenyuPlugin outer = new FixturePlugin((exchange, chain) -> {
            hooks.add("execute");
            return chain.execute(exchange);
        }) {
            @Override
            public void before(final ServerWebExchange exchange) {
                hooks.add("before");
            }

            @Override
            public void after(final ServerWebExchange exchange) {
                hooks.add("after");
            }
        };
        try (var observer = observer(event -> {
            events.add(event);
            exported.countDown();
        })) {
            Mono<Void> root = handler(observer, outer, plugin((exchange, chain) -> gate.asMono())).handle(exchange());
            assertTrue(hooks.isEmpty());
            StepVerifier.create(root).then(() -> {
                assertEquals(List.of("before", "execute", "after"), hooks);
                assertTrue(events.stream().noneMatch(event -> event.kind() == PluginObservationEvent.Kind.TERMINAL));
                gate.tryEmitEmpty();
            }).expectComplete().verify(Duration.ofSeconds(5));
            assertTrue(exported.await(5, TimeUnit.SECONDS));
            assertEquals(2, events.stream().filter(event -> event.kind() == PluginObservationEvent.Kind.TERMINAL).count());
            assertTrue(events.stream().allMatch(event -> Objects.isNull(event.matching())));
        }
    }

    @Test
    void constructionFailureIsDifferentFromReturnedPublisherFailure() throws Exception {
        for (boolean synchronous : new boolean[]{true, false}) {
            List<PluginObservationEvent> events = new CopyOnWriteArrayList<>();
            CountDownLatch exported = new CountDownLatch(synchronous ? 1 : 3);
            IllegalStateException original = new IllegalStateException("SECRET must not reach telemetry");
            AtomicInteger after = new AtomicInteger();
            ShenyuPlugin fixture = new FixturePlugin((exchange, chain) -> {
                if (synchronous) {
                    throw original;
                }
                return Mono.error(original);
            }) {
                @Override
                public void after(final ServerWebExchange exchange) {
                    after.incrementAndGet();
                }
            };
            try (var observer = observer(event -> {
                events.add(event);
                exported.countDown();
            })) {
                StepVerifier.create(handler(observer, fixture).handle(exchange())).expectErrorMatches(error -> error == original).verify();
                assertTrue(exported.await(5, TimeUnit.SECONDS));
                assertEquals(1, after.get());
                assertEquals(synchronous, events.stream().anyMatch(event -> event.kind() == PluginObservationEvent.Kind.CONSTRUCTION_ERROR));
                assertEquals(!synchronous, events.stream().anyMatch(event -> event.kind() == PluginObservationEvent.Kind.TERMINAL));
                assertFalse(events.toString().contains("SECRET"));
            }
        }
    }

    @Test
    void cancellationAndRepeatedRootsHaveIndependentIdentities() throws Exception {
        List<PluginObservationEvent> events = new CopyOnWriteArrayList<>();
        CountDownLatch exported = new CountDownLatch(6);
        AtomicInteger cancelled = new AtomicInteger();
        try (var observer = observer(event -> {
            events.add(event);
            exported.countDown();
        })) {
            Mono<Void> root = handler(observer, plugin((exchange, chain) -> Mono.<Void>never()
                    .doOnCancel(cancelled::incrementAndGet))).handle(exchange());
            StepVerifier.create(root).thenCancel().verify();
            StepVerifier.create(root).thenCancel().verify();
            assertTrue(exported.await(5, TimeUnit.SECONDS));
            assertEquals(2, cancelled.get());
            var terminals = events.stream().filter(event -> event.kind() == PluginObservationEvent.Kind.TERMINAL).toList();
            assertEquals(2, terminals.size());
            assertTrue(terminals.stream().allMatch(event -> event.outcome() == PluginObservationEvent.Outcome.CANCELLED));
            assertEquals(2, terminals.stream().map(PluginObservationEvent::requestId).distinct().count());
        }
    }

    @Test
    void skippedAndShortCircuitPluginsAreNotInferredToHaveMatched() throws Exception {
        List<PluginObservationEvent> events = new CopyOnWriteArrayList<>();
        CountDownLatch exported = new CountDownLatch(4);
        ShenyuPlugin skipped = new FixturePlugin((exchange, chain) -> Mono.error(new IllegalStateException())) {
            @Override
            public boolean skip(final ServerWebExchange exchange) {
                return true;
            }
        };
        try (var observer = observer(event -> {
            events.add(event);
            exported.countDown();
        })) {
            handler(observer, skipped, plugin((exchange, chain) -> Mono.empty())).handle(exchange()).block();
            assertTrue(exported.await(5, TimeUnit.SECONDS));
            assertTrue(events.stream().anyMatch(event -> event.kind() == PluginObservationEvent.Kind.SKIPPED));
            assertTrue(events.stream().noneMatch(event -> event.kind() == PluginObservationEvent.Kind.MATCHING));
        }
    }

    @Test
    void slowAndThrowingExportersCannotBlockRequestsAndDropsAreObservable() throws Exception {
        CountDownLatch entered = new CountDownLatch(1);
        CountDownLatch release = new CountDownLatch(1);
        try (var observer = new PluginObservationManager(2, 1, 128, 1048576, event -> {
            assertFalse(Schedulers.isInNonBlockingThread());
            entered.countDown();
            try {
                release.await(5, TimeUnit.SECONDS);
            } catch (InterruptedException interrupted) {
                Thread.currentThread().interrupt();
            }
            throw new IllegalArgumentException("SECRET exporter");
        })) {
            ShenyuWebHandler handler = handler(observer, plugin((exchange, chain) -> Mono.empty()));
            handler.handle(exchange()).block();
            assertTrue(entered.await(5, TimeUnit.SECONDS));
            Flux.range(0, 32).concatMap(index -> handler.handle(exchange())).subscribeOn(Schedulers.parallel())
                    .then().block(Duration.ofSeconds(5));
            assertTrue(observer.stats().queueDrops() > 0);
            release.countDown();
        } finally {
            release.countDown();
        }
    }

    @Test
    void finiteRootBudgetEmitsTruncationWithoutAffectingForwarding() throws Exception {
        List<PluginObservationEvent> events = new CopyOnWriteArrayList<>();
        CountDownLatch exported = new CountDownLatch(2);
        try (var observer = new PluginObservationManager(16, 1, 2, 16384, event -> {
            events.add(event);
            exported.countDown();
        })) {
            handler(observer, plugin((exchange, chain) -> chain.execute(exchange)), plugin((exchange, chain) -> Mono.empty()))
                    .handle(exchange()).block();
            assertTrue(exported.await(5, TimeUnit.SECONDS));
            assertEquals(2, events.size());
            assertEquals(PluginObservationEvent.Kind.TRUNCATED, events.get(1).kind());
            assertTrue(observer.stats().budgetDrops() > 0);
        }
    }

    @Test
    void optionalForkSharesRootIdentityAndPreservesIndependentCursors() throws Exception {
        List<PluginObservationEvent> events = new CopyOnWriteArrayList<>();
        CountDownLatch exported = new CountDownLatch(10);
        ShenyuPlugin dispatch = new FixturePlugin((exchange, chain) -> {
            exchange.getAttributes().put("forked", true);
            return chain.fork().execute(exchange).then(chain.execute(exchange))
                    .doFinally(signal -> exchange.getAttributes().remove("forked"));
        }) {
            @Override
            public boolean skip(final ServerWebExchange exchange) {
                return Boolean.TRUE.equals(exchange.getAttribute("forked"));
            }
        };
        try (var observer = observer(event -> {
            events.add(event);
            exported.countDown();
        })) {
            handler(observer, dispatch, plugin((exchange, chain) -> Mono.empty())).handle(exchange()).block();
            assertTrue(exported.await(5, TimeUnit.SECONDS));
            assertEquals(1, events.stream().map(PluginObservationEvent::requestId).distinct().count());
            assertEquals(3, events.stream().filter(event -> event.kind() == PluginObservationEvent.Kind.TERMINAL).count());
            assertEquals(1, events.stream().filter(event -> event.kind() == PluginObservationEvent.Kind.SKIPPED).count());
        }
    }

    @Test
    void samplingLeavesUnsampledRootsUnobserved() throws Exception {
        List<PluginObservationEvent> events = new CopyOnWriteArrayList<>();
        CountDownLatch exported = new CountDownLatch(6);
        try (var observer = new PluginObservationManager(64, 2, 128, 1048576, event -> {
            events.add(event);
            exported.countDown();
        })) {
            ShenyuWebHandler handler = handler(observer, plugin((exchange, chain) -> Mono.empty()));
            for (int index = 0; index < 4; index++) {
                handler.handle(exchange()).block();
            }
            assertTrue(exported.await(5, TimeUnit.SECONDS));
            assertEquals(List.of(1L, 3L), events.stream().map(PluginObservationEvent::requestId).distinct().toList());
        }
    }

    private PluginObservationManager observer(final org.apache.shenyu.common.metrics.PluginObservationSink sink) {
        return new PluginObservationManager(128, 1, 128, 1048576, sink);
    }

    private ShenyuWebHandler handler(final PluginObservationManager observer, final ShenyuPlugin... plugins) {
        ShenyuConfig config = new ShenyuConfig();
        config.getScheduler().setEnabled(false);
        return new ShenyuWebHandler(List.of(plugins), null, config, observer);
    }

    private MockServerWebExchange exchange() {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/fixture").build());
        exchange.getAttributes().put(Constants.SHENYU_AGENT_TRACE_ID, "existing-trace");
        return exchange;
    }

    private ShenyuPlugin plugin(final BiFunction<ServerWebExchange, ShenyuPluginChain, Mono<Void>> behavior) {
        return new FixturePlugin(behavior);
    }

    private static class FixturePlugin implements ShenyuPlugin {

        private final BiFunction<ServerWebExchange, ShenyuPluginChain, Mono<Void>> behavior;

        protected FixturePlugin(final BiFunction<ServerWebExchange, ShenyuPluginChain, Mono<Void>> behavior) {
            this.behavior = behavior;
        }

        @Override
        public Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
            return behavior.apply(exchange, chain);
        }

        @Override
        public int getOrder() {
            return 0;
        }

        @Override
        public String named() {
            return "fixture";
        }
    }
}
