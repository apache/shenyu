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

package org.apache.shenyu.web.handler;

import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.web.loader.ShenyuLoaderService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.test.util.ReflectionTestUtils;
import reactor.core.Disposable;
import reactor.core.Disposables;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;
import reactor.core.scheduler.Scheduler;
import reactor.test.StepVerifier;
import reactor.test.scheduler.VirtualTimeScheduler;

import java.time.Duration;
import java.util.Collections;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Tests subscription and terminal-signal lifecycle with controlled publishers and scheduling.
 */
class ShenyuWebHandlerLifecycleTest {

    private final ShenyuPlugin plugin = mock(ShenyuPlugin.class);

    private final MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/lifecycle"));

    private final VirtualTimeScheduler scheduler = VirtualTimeScheduler.create();

    @AfterEach
    void tearDown() {
        scheduler.dispose();
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void testCompletion(final boolean scheduled) {
        verifyTerminal(scheduled, null, false);
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void testAsynchronousFailure(final boolean scheduled) {
        verifyTerminal(scheduled, new IllegalStateException("upstream failed"), false);
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void testCancellation(final boolean scheduled) {
        verifyTerminal(scheduled, null, true);
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void testSynchronousPluginFailure(final boolean scheduled) {
        ShenyuWebHandler handler = handler(scheduled);
        IllegalStateException failure = new IllegalStateException("plugin failed");
        when(plugin.execute(any(), any())).thenThrow(failure);

        StepVerifier.create(handler.handle(exchange))
                .then(() -> scheduler.advanceTimeBy(Duration.ZERO))
                .expectErrorSatisfies(error -> assertSame(failure, error))
                .verify(Duration.ofSeconds(5));

        verify(handler).before(exchange);
        verify(handler).after(exchange);
        verify(plugin).after(exchange);
        assertNull(exchange.getAttribute(Constants.CHAIN_START_TIME));
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void testTimingRemainsActiveUntilDelayedCompletion(final boolean scheduled) {
        ShenyuWebHandler handler = handler(scheduled);
        when(plugin.execute(any(), any())).thenReturn(Mono.delay(Duration.ofSeconds(5), scheduler).then());
        final Mono<Void> result = handler.handle(exchange);
        verify(handler, never()).before(exchange);
        verify(handler, never()).after(exchange);
        assertNull(exchange.getAttribute(Constants.CHAIN_START_TIME));

        StepVerifier.create(result)
                .then(() -> scheduler.advanceTimeBy(Duration.ofSeconds(4)))
                .then(() -> {
                    assertNotNull(exchange.getAttribute(Constants.CHAIN_START_TIME));
                    verify(handler, never()).after(exchange);
                })
                .then(() -> scheduler.advanceTimeBy(Duration.ofSeconds(1)))
                .expectComplete()
                .verify(Duration.ofSeconds(5));

        verify(handler).before(exchange);
        verify(handler).after(exchange);
        assertNull(exchange.getAttribute(Constants.CHAIN_START_TIME));
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void testEachSubscriptionExecutesAFreshChain(final boolean scheduled) {
        ShenyuWebHandler handler = handler(scheduled);
        when(plugin.execute(any(), any())).thenReturn(Mono.empty());
        final Mono<Void> result = handler.handle(exchange);
        for (int subscription = 0; subscription < 2; subscription++) {
            StepVerifier.create(result)
                    .then(() -> scheduler.advanceTimeBy(Duration.ZERO))
                    .expectComplete().verify(Duration.ofSeconds(5));
            assertNull(exchange.getAttribute(Constants.CHAIN_START_TIME));
        }
        verify(plugin, times(2)).execute(any(), any());
        verify(handler, times(2)).before(exchange);
        verify(handler, times(2)).after(exchange);
    }

    @Test
    void testCancellationBeforeScheduledExecution() {
        ShenyuWebHandler handler = handler(true);
        Scheduler queuedScheduler = mock(Scheduler.class);
        Scheduler.Worker worker = mock(Scheduler.Worker.class);
        when(queuedScheduler.createWorker()).thenReturn(worker);
        when(worker.schedule(any(Runnable.class))).thenReturn(Disposables.single());
        ReflectionTestUtils.setField(handler, "scheduler", queuedScheduler);
        Disposable subscription = handler.handle(exchange).subscribe();
        assertNotNull(exchange.getAttribute(Constants.CHAIN_START_TIME));
        verify(plugin, never()).execute(any(), any());

        subscription.dispose();
        scheduler.advanceTimeBy(Duration.ZERO);

        verify(plugin, never()).execute(any(), any());
        verify(handler).before(exchange);
        verify(handler).after(exchange);
        verify(worker).dispose();
        assertNull(exchange.getAttribute(Constants.CHAIN_START_TIME));
    }

    private void verifyTerminal(final boolean scheduled, final Throwable failure, final boolean cancel) {
        ShenyuWebHandler handler = handler(scheduled);
        Sinks.Empty<Void> pending = Sinks.empty();
        when(plugin.execute(any(), any())).thenReturn(pending.asMono());
        final Mono<Void> result = handler.handle(exchange);
        verify(handler, never()).before(exchange);
        verify(handler, never()).after(exchange);
        assertNull(exchange.getAttribute(Constants.CHAIN_START_TIME));
        AtomicReference<Throwable> observedFailure = new AtomicReference<>();
        Disposable subscription = result.subscribe(ignored -> { }, observedFailure::set);
        scheduler.advanceTimeBy(Duration.ZERO);
        verify(handler).before(exchange);
        verify(handler, never()).after(exchange);
        assertNotNull(exchange.getAttribute(Constants.CHAIN_START_TIME));
        // Plugin hooks still bracket publisher construction, including WASM callbacks.
        verify(plugin).before(exchange);
        verify(plugin).after(exchange);
        if (cancel) {
            subscription.dispose();
        } else if (Objects.isNull(failure)) {
            pending.tryEmitEmpty();
        } else {
            pending.tryEmitError(failure);
        }
        assertSame(failure, observedFailure.get());
        verify(handler).after(exchange);
        assertNull(exchange.getAttribute(Constants.CHAIN_START_TIME));
        subscription.dispose();
        verify(handler).after(exchange);
    }

    private ShenyuWebHandler handler(final boolean scheduled) {
        ShenyuConfig config = new ShenyuConfig();
        config.getScheduler().setEnabled(scheduled);
        config.getScheduler().setType("elastic");
        ShenyuWebHandler handler = spy(new ShenyuWebHandler(Collections.singletonList(plugin), mock(ShenyuLoaderService.class), config));
        ReflectionTestUtils.setField(handler, "scheduler", scheduler);
        return handler;
    }
}
