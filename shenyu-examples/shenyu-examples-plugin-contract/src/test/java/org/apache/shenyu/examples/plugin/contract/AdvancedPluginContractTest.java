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

package org.apache.shenyu.examples.plugin.contract;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.web.contract.ContractProbe;
import org.apache.shenyu.web.contract.LegacyPluginRuntime;
import org.apache.shenyu.web.contract.PluginContractAssertions;
import org.apache.shenyu.web.contract.PluginContractProfile;
import org.apache.shenyu.web.contract.RecordingPlugin;
import org.junit.jupiter.api.Test;
import org.reactivestreams.Publisher;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.http.server.reactive.ServerHttpResponseDecorator;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.Disposable;
import reactor.core.publisher.Mono;
import reactor.core.publisher.SignalType;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.BiFunction;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Consumer examples for resource ownership, response wrapping and legacy lifecycle contracts.
 */
public class AdvancedPluginContractTest {

    private static final String CONFIG_ATTRIBUTE = "advanced.config";

    private static final String FORK_ATTRIBUTE = "advanced.fork";

    @Test
    public void cancellationRunsOwnedCleanup() {
        final AtomicBoolean cleaned = new AtomicBoolean();
        final ContractProbe probe = probe("owned-cancellation", PluginContractProfile.SHORT_CIRCUIT,
                (exchange, chain) -> Mono.<Void>never().doFinally(signal -> cleaned.set(signal == SignalType.CANCEL)));

        PluginContractAssertions.assertCancellationCleanup(new LegacyPluginRuntime(probe),
                LegacyPluginRuntime.newExchange(), probe, cleaned::get);

        assertTrue(cleaned.get());
        PluginContractAssertions.assertContinuation(probe);
    }

    @Test
    public void cancellationRejectsDetachedSubscriptionThatLosesCleanup() {
        final AtomicBoolean cleaned = new AtomicBoolean();
        final AtomicReference<Disposable> detached = new AtomicReference<>();
        final ContractProbe probe = probe("detached-cancellation", PluginContractProfile.SHORT_CIRCUIT,
                (exchange, chain) -> Mono.defer(() -> {
                    detached.set(Mono.<Void>never().doFinally(signal -> cleaned.set(true)).subscribe());
                    return Mono.never();
                }));
        try {
            final AssertionError failure = assertThrows(AssertionError.class,
                    () -> PluginContractAssertions.assertCancellationCleanup(new LegacyPluginRuntime(probe),
                            LegacyPluginRuntime.newExchange(), probe, cleaned::get));

            assertDiagnostic(failure, probe);
            assertEquals(1, probe.getCancellationCount(), "root cancellation alone does not prove resource cleanup");
            assertFalse(cleaned.get(), "the detached work is still owned by a separate subscription");
        } finally {
            final Disposable subscription = detached.get();
            if (Objects.nonNull(subscription)) {
                subscription.dispose();
            }
        }
        assertTrue(cleaned.get(), "the test explicitly releases detached work");
    }

    @Test
    public void nestedWrappersPreserveSignalsAndDecoratedResponse() {
        final List<String> signals = new ArrayList<>();
        final ResponseWrapper outer = new ResponseWrapper("outer", signals, false);
        final ResponseWrapper inner = new ResponseWrapper("inner", signals, false);
        final ContractProbe outerProbe = ContractProbe.around(outer, "outer", 10, PluginContractProfile.DOWNSTREAM_WRAPPER);
        final ContractProbe innerProbe = ContractProbe.around(inner, "inner", 20, PluginContractProfile.DOWNSTREAM_WRAPPER);
        final MockServerWebExchange exchange = (MockServerWebExchange) LegacyPluginRuntime.newExchange();

        PluginContractAssertions.assertCompletes(new LegacyPluginRuntime(outerProbe, innerProbe, bodyTerminal(signals)), exchange);

        assertDecoratedResponse(exchange, outerProbe, outer);
        assertDecoratedResponse(exchange, innerProbe, inner);
        assertEquals(List.of("outer-subscribe", "inner-subscribe", "terminal-write",
                "inner-complete", "outer-complete"), signals);
        PluginContractAssertions.assertContinuation(outerProbe);
        PluginContractAssertions.assertContinuation(innerProbe);
    }

    @Test
    public void wrapperThatDropsDecoratedExchangeIsRejectedWithProfileDetail() {
        final ResponseWrapper wrapper = new ResponseWrapper("lost-decorator", new ArrayList<>(), true);
        final ContractProbe probe = ContractProbe.around(wrapper, "lost-decorator", 10, PluginContractProfile.DOWNSTREAM_WRAPPER);
        final MockServerWebExchange exchange = (MockServerWebExchange) LegacyPluginRuntime.newExchange();

        PluginContractAssertions.assertCompletes(new LegacyPluginRuntime(probe, bodyTerminal(new ArrayList<>())), exchange);
        PluginContractAssertions.assertContinuation(probe);

        final AssertionError failure = assertThrows(AssertionError.class, () -> assertDecoratedResponse(exchange, probe, wrapper));
        assertDiagnostic(failure, probe);
        assertEquals("contract-body", exchange.getResponse().getBodyAsString().block());
        assertEquals(0, wrapper.writes.get());
    }

    @Test
    public void duplicatePassThroughSubscriptionRepeatsSideEffectsAndIsRejected() {
        final AtomicInteger effects = new AtomicInteger();
        final ContractProbe probe = probe("duplicate-pass-through", PluginContractProfile.PASS_THROUGH,
                (exchange, chain) -> Mono.fromRunnable(effects::incrementAndGet)
                        .then(chain.execute(exchange)).repeat(1).then());
        final RecordingPlugin terminal = new RecordingPlugin("terminal", 100, RecordingPlugin.Role.SHORT_CIRCUIT);

        PluginContractAssertions.assertCompletes(new LegacyPluginRuntime(probe, terminal), LegacyPluginRuntime.newExchange());

        assertEquals(2, effects.get(), "repeating the returned publisher repeats the plugin's side effect");
        assertEquals(1, probe.getChainInvocations());
        assertEquals(2, probe.getDownstreamSubscriptions());
        assertEquals(1, terminal.getExecuteCount(), "the legacy downstream cursor is exhausted on resubscription");
        assertDiagnostic(assertThrows(AssertionError.class, () -> PluginContractAssertions.assertContinuation(probe)), probe);
    }

    @Test
    public void duplicatePassThroughInvocationIsRejected() {
        final ContractProbe probe = probe("duplicate-invocation", PluginContractProfile.PASS_THROUGH,
                (exchange, chain) -> chain.execute(exchange).then(chain.execute(exchange)));
        PluginContractAssertions.assertCompletes(new LegacyPluginRuntime(probe), LegacyPluginRuntime.newExchange());

        assertEquals(2, probe.getChainInvocations());
        assertDiagnostic(assertThrows(AssertionError.class, () -> PluginContractAssertions.assertContinuation(probe)), probe);
    }

    @Test
    public void legitimateWrapperMayResubscribeForEveryRootExecution() {
        final ContractProbe probe = probe("repeating-wrapper", PluginContractProfile.DOWNSTREAM_WRAPPER,
                (exchange, chain) -> chain.execute(exchange).repeat(1).then());
        final RecordingPlugin terminal = new RecordingPlugin("terminal", 100, RecordingPlugin.Role.SHORT_CIRCUIT);
        final LegacyPluginRuntime runtime = new LegacyPluginRuntime(probe, terminal);

        PluginContractAssertions.assertProbeRepeatedSubscriptions(runtime, LegacyPluginRuntime.newExchange(), probe);
        PluginContractAssertions.assertContinuation(probe);

        assertEquals(2, probe.getExecuteCount());
        assertEquals(2, probe.getChainInvocations());
        assertEquals(4, probe.getDownstreamSubscriptions());
        assertEquals(2, terminal.getExecuteCount(), "each fresh root chain reaches the terminal once");
    }

    @Test
    public void wrapperMustContinueForEveryRootExecution() {
        final AtomicInteger executions = new AtomicInteger();
        final ContractProbe probe = probe("inconsistent-wrapper", PluginContractProfile.DOWNSTREAM_WRAPPER,
                (exchange, chain) -> executions.incrementAndGet() == 1 ? chain.execute(exchange) : Mono.empty());
        final LegacyPluginRuntime runtime = new LegacyPluginRuntime(probe);

        PluginContractAssertions.assertCompletes(runtime, LegacyPluginRuntime.newExchange());
        PluginContractAssertions.assertContinuation(probe);
        PluginContractAssertions.assertCompletes(runtime, LegacyPluginRuntime.newExchange());

        assertEquals(2, probe.getExecuteCount());
        assertEquals(1, probe.getChainInvocations());
        assertEquals(1, probe.getDownstreamSubscriptions());
        assertDiagnostic(assertThrows(AssertionError.class, () -> PluginContractAssertions.assertContinuation(probe)), probe);
    }

    @Test
    public void continuationCountsScaleWithPassThroughAndTransformExecutions() {
        for (PluginContractProfile profile : List.of(PluginContractProfile.PASS_THROUGH, PluginContractProfile.TRANSFORM)) {
            final AtomicInteger effects = new AtomicInteger();
            final ContractProbe probe = probe("repeated-" + profile.displayName(), profile,
                    (exchange, chain) -> Mono.defer(() -> {
                        effects.incrementAndGet();
                        exchange.getAttributes().put(CONFIG_ATTRIBUTE, profile.displayName());
                        return chain.execute(exchange);
                    }));
            final LegacyPluginRuntime runtime = new LegacyPluginRuntime(probe);
            final ServerWebExchange exchange = LegacyPluginRuntime.newExchange();

            PluginContractAssertions.assertProbeRepeatedSubscriptions(runtime, exchange, probe);
            PluginContractAssertions.assertContinuation(probe);

            assertEquals(2, effects.get());
            assertEquals(2, probe.getChainInvocations());
            assertEquals(2, probe.getDownstreamSubscriptions());
            assertEquals(profile.displayName(), exchange.getAttribute(CONFIG_ATTRIBUTE));
        }
    }

    @Test
    public void downstreamFailurePropagatesThroughWrapperAndRunsCleanup() {
        final IllegalStateException expected = new IllegalStateException("controlled downstream failure");
        final AtomicReference<Throwable> observed = new AtomicReference<>();
        final AtomicReference<SignalType> cleanup = new AtomicReference<>();
        final ContractProbe wrapper = probe("failure-wrapper", PluginContractProfile.DOWNSTREAM_WRAPPER,
                (exchange, chain) -> chain.execute(exchange).doOnError(observed::set).doFinally(cleanup::set));
        final ContractProbe terminal = probe("failure-terminal", PluginContractProfile.SHORT_CIRCUIT,
                (exchange, chain) -> Mono.error(expected));

        StepVerifier.create(new LegacyPluginRuntime(wrapper, terminal).handle(LegacyPluginRuntime.newExchange()))
                .expectErrorSatisfies(error -> assertSame(expected, error))
                .verify(Duration.ofSeconds(5));

        assertSame(expected, observed.get());
        assertEquals(SignalType.ON_ERROR, cleanup.get());
        assertEquals(1, wrapper.getErrorCount());
        assertEquals(0, wrapper.getCompletionCount());
        assertEquals(1, terminal.getErrorCount());
        PluginContractAssertions.assertContinuation(wrapper);
        PluginContractAssertions.assertContinuation(terminal);
    }

    @Test
    public void probePreservesConstructionTimeLifecycleAndSynchronousExecuteEffect() {
        final List<String> events = new ArrayList<>();
        final AtomicBoolean effect = new AtomicBoolean();
        final ContractProbe probe = ContractProbe.around(new LifecyclePlugin(events, effect), "lifecycle", 10,
                PluginContractProfile.PASS_THROUGH);
        final Mono<Void> handle = new LegacyPluginRuntime(probe).handle(LegacyPluginRuntime.newExchange());

        assertTrue(events.isEmpty(), "constructing the root publisher must not enter the plugin");
        assertFalse(effect.get());
        StepVerifier.create(handle).expectComplete().verify(Duration.ofSeconds(5));

        assertEquals(List.of("before", "execute", "after", "onSubscribe"), events);
        assertTrue(effect.get(), "synchronous execute effects are permitted after root subscription");
        assertEquals(1, probe.getSubscribedCount());
        PluginContractAssertions.assertContinuation(probe);
    }

    @Test
    public void guardedForkRunsAnIndependentChainWithoutConsumingParentCursor() {
        final ContractProbe fork = probe("guarded-fork", PluginContractProfile.DOWNSTREAM_WRAPPER,
                (exchange, chain) -> {
                    if (Boolean.TRUE.equals(exchange.getAttribute(FORK_ATTRIBUTE))) {
                        return chain.execute(exchange);
                    }
                    return Mono.defer(() -> {
                        exchange.getAttributes().put(FORK_ATTRIBUTE, Boolean.TRUE);
                        return chain.fork().execute(exchange)
                                .then(Mono.defer(() -> {
                                    exchange.getAttributes().remove(FORK_ATTRIBUTE);
                                    return chain.execute(exchange);
                                }));
                    });
                });
        final RecordingPlugin terminal = new RecordingPlugin("fork-terminal", 100, RecordingPlugin.Role.SHORT_CIRCUIT);
        final ServerWebExchange exchange = LegacyPluginRuntime.newExchange();

        PluginContractAssertions.assertCompletes(new LegacyPluginRuntime(fork, terminal), exchange);

        assertEquals(2, fork.getExecuteCount(), "fork starts at the first plugin with its own cursor");
        assertEquals(2, terminal.getExecuteCount(), "the fork must not consume the parent continuation");
        assertFalse(exchange.getAttributes().containsKey(FORK_ATTRIBUTE));
        PluginContractAssertions.assertContinuation(fork);
    }

    @Test
    public void pluginDataHandlerUpdatesAndRemovalAffectSubsequentRequests() {
        final LocalConfigHandler handler = new LocalConfigHandler();
        final ContractProbe probe = probe(handler.pluginNamed(), PluginContractProfile.TRANSFORM,
                (exchange, chain) -> Mono.defer(() -> {
                    exchange.getAttributes().put(CONFIG_ATTRIBUTE, handler.value.get());
                    return chain.execute(exchange);
                }));
        final LegacyPluginRuntime runtime = new LegacyPluginRuntime(probe);
        final PluginData first = pluginData("first");
        final PluginData updated = pluginData("updated");

        handler.handlerPlugin(first);
        final ServerWebExchange firstExchange = LegacyPluginRuntime.newExchange();
        PluginContractAssertions.assertCompletes(runtime, firstExchange);
        assertEquals("first", firstExchange.getAttribute(CONFIG_ATTRIBUTE));

        handler.handlerPlugin(updated);
        final ServerWebExchange updatedExchange = LegacyPluginRuntime.newExchange();
        PluginContractAssertions.assertCompletes(runtime, updatedExchange);
        assertEquals("updated", updatedExchange.getAttribute(CONFIG_ATTRIBUTE));
        assertEquals("first", firstExchange.getAttribute(CONFIG_ATTRIBUTE), "an update applies to subsequent requests");

        handler.removePlugin(updated);
        final ServerWebExchange removedExchange = LegacyPluginRuntime.newExchange();
        PluginContractAssertions.assertCompletes(runtime, removedExchange);
        assertEquals("default", removedExchange.getAttribute(CONFIG_ATTRIBUTE));
        assertEquals(3, probe.getExecuteCount());
        PluginContractAssertions.assertContinuation(probe);
    }

    @Test
    public void repeatedSubscriptionRejectsMissingSecondContinuation() {
        final AtomicInteger executions = new AtomicInteger();
        final ContractProbe probe = probe("missing-second", PluginContractProfile.PASS_THROUGH,
                (exchange, chain) -> executions.incrementAndGet() == 1 ? chain.execute(exchange) : Mono.empty());
        assertDiagnostic(assertThrows(AssertionError.class,
                () -> PluginContractAssertions.assertProbeRepeatedSubscriptions(new LegacyPluginRuntime(probe),
                        LegacyPluginRuntime.newExchange(), probe)), probe);
    }

    @Test
    public void selectorContinuationRejectsAnUnsubscribedDownstreamPublisher() {
        final ContractProbe probe = probe("discarded-downstream", PluginContractProfile.SELECTOR_RULE,
                (exchange, chain) -> {
                    chain.execute(exchange);
                    return Mono.empty();
                });
        PluginContractAssertions.assertCompletes(new LegacyPluginRuntime(probe), LegacyPluginRuntime.newExchange());
        assertDiagnostic(assertThrows(AssertionError.class, () -> PluginContractAssertions.assertContinuation(probe)), probe);
    }

    @Test
    public void configurationRemovalRejectsStaleHandlerState() {
        final AtomicReference<String> state = new AtomicReference<>("default");
        final PluginDataHandler stale = new PluginDataHandler() {
            @Override
            public void handlerPlugin(final PluginData data) {
                state.set(data.getConfig());
            }

            @Override
            public String pluginNamed() {
                return "stale-config";
            }
        };
        final ContractProbe probe = probe(stale.pluginNamed(), PluginContractProfile.TRANSFORM,
                (exchange, chain) -> {
                    exchange.getAttributes().put(CONFIG_ATTRIBUTE, state.get());
                    return chain.execute(exchange);
                });
        stale.handlerPlugin(pluginData("configured"));
        stale.removePlugin(pluginData("configured"));
        final ServerWebExchange exchange = LegacyPluginRuntime.newExchange();
        PluginContractAssertions.assertCompletes(new LegacyPluginRuntime(probe), exchange);
        final AssertionError failure = assertThrows(AssertionError.class,
                () -> assertEquals("default", exchange.getAttribute(CONFIG_ATTRIBUTE),
                        "stale-config [transform] removal must reset plugin state"));
        assertDiagnostic(failure, probe);
    }

    private static ContractProbe probe(final String name, final PluginContractProfile profile,
                                       final BiFunction<ServerWebExchange, ShenyuPluginChain, Mono<Void>> action) {
        return ContractProbe.around(new ActionPlugin(action), name, 10, profile);
    }

    private static ShenyuPlugin bodyTerminal(final List<String> signals) {
        return new ActionPlugin((exchange, chain) -> Mono.defer(() -> {
            signals.add("terminal-write");
            final DataBuffer buffer = exchange.getResponse().bufferFactory().wrap("contract-body".getBytes(StandardCharsets.UTF_8));
            return exchange.getResponse().writeWith(Mono.just(buffer));
        }));
    }

    private static void assertDecoratedResponse(final MockServerWebExchange exchange, final ContractProbe probe,
                                               final ResponseWrapper wrapper) {
        final String detail = probe.named() + " [" + probe.getProfile().displayName() + "]";
        assertEquals(1, wrapper.writes.get(), detail + " must propagate its response decorator to the terminal");
        assertEquals("written", exchange.getResponse().getHeaders().getFirst("X-Contract-" + probe.named()),
                detail + " must retain response decoration headers");
        assertEquals("contract-body", exchange.getResponse().getBodyAsString().block(), detail + " must retain the response body");
    }

    private static void assertDiagnostic(final AssertionError failure, final ContractProbe probe) {
        assertTrue(failure.getMessage().contains(probe.named()), failure::getMessage);
        assertTrue(failure.getMessage().contains(probe.getProfile().displayName()), failure::getMessage);
    }

    private static PluginData pluginData(final String config) {
        final PluginData data = new PluginData();
        data.setName("local-config");
        data.setConfig(config);
        data.setEnabled(true);
        return data;
    }

    private static final class ActionPlugin implements ShenyuPlugin {

        private final BiFunction<ServerWebExchange, ShenyuPluginChain, Mono<Void>> action;

        private ActionPlugin(final BiFunction<ServerWebExchange, ShenyuPluginChain, Mono<Void>> action) {
            this.action = action;
        }

        @Override
        public Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
            return action.apply(exchange, chain);
        }

        @Override
        public int getOrder() {
            return 100;
        }
    }

    private static final class ResponseWrapper implements ShenyuPlugin {

        private final String name;

        private final List<String> signals;

        private final boolean dropDecoration;

        private final AtomicInteger writes = new AtomicInteger();

        private ResponseWrapper(final String name, final List<String> signals, final boolean dropDecoration) {
            this.name = name;
            this.signals = signals;
            this.dropDecoration = dropDecoration;
        }

        @Override
        public Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
            final ServerHttpResponseDecorator response = new ServerHttpResponseDecorator(exchange.getResponse()) {
                @Override
                public Mono<Void> writeWith(final Publisher<? extends DataBuffer> body) {
                    writes.incrementAndGet();
                    getHeaders().set("X-Contract-" + name, "written");
                    return super.writeWith(body);
                }
            };
            final ServerWebExchange decorated = exchange.mutate().response(response).build();
            return Mono.defer(() -> {
                signals.add(name + "-subscribe");
                return chain.execute(dropDecoration ? exchange : decorated);
            }).doOnSuccess(ignored -> signals.add(name + "-complete"));
        }

        @Override
        public int getOrder() {
            return 10;
        }
    }

    private static final class LifecyclePlugin implements ShenyuPlugin {

        private final List<String> events;

        private final AtomicBoolean effect;

        private LifecyclePlugin(final List<String> events, final AtomicBoolean effect) {
            this.events = events;
            this.effect = effect;
        }

        @Override
        public void before(final ServerWebExchange exchange) {
            events.add("before");
        }

        @Override
        public Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
            events.add("execute");
            effect.set(true);
            return chain.execute(exchange).doOnSubscribe(subscription -> events.add("onSubscribe"));
        }

        @Override
        public void after(final ServerWebExchange exchange) {
            events.add("after");
        }

        @Override
        public int getOrder() {
            return 10;
        }
    }

    private static final class LocalConfigHandler implements PluginDataHandler {

        private final AtomicReference<String> value = new AtomicReference<>("default");

        @Override
        public void handlerPlugin(final PluginData pluginData) {
            value.set(pluginData.getConfig());
        }

        @Override
        public void removePlugin(final PluginData pluginData) {
            value.set("default");
        }

        @Override
        public String pluginNamed() {
            return "local-config";
        }
    }
}
