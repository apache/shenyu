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

package org.apache.shenyu.web.contract;

import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.time.Duration;
import java.util.Objects;
import java.util.function.BooleanSupplier;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Assertions for the observable part of the legacy {@code ShenyuPlugin} contract.
 *
 * <p>The checks drive the real {@link LegacyPluginRuntime}; they do not create a replacement
 * chain. A failure identifies the plugin name and the boundary that was violated.</p>
 */
public final class PluginContractAssertions {

    private static final Duration VERIFICATION_TIMEOUT = Duration.ofSeconds(5);

    private PluginContractAssertions() {
    }

    /**
     * Asserts that the chain completes normally.
     *
     * @param runtime runtime fixture
     * @param exchange request exchange
     */
    public static void assertCompletes(final LegacyPluginRuntime runtime, final ServerWebExchange exchange) {
        StepVerifier.create(runtime.handle(exchange)).expectSubscription().expectComplete().verify(VERIFICATION_TIMEOUT);
    }

    /**
     * Asserts that the chain fails with the supplied message fragment.
     *
     * @param runtime runtime fixture
     * @param exchange request exchange
     * @param expectedMessage message fragment
     */
    public static void assertFails(final LegacyPluginRuntime runtime, final ServerWebExchange exchange,
                                   final String expectedMessage) {
        StepVerifier.create(runtime.handle(exchange))
                .expectSubscription()
                .expectErrorSatisfies(error -> {
                    final String message = error.getMessage();
                    assertTrue(Objects.nonNull(message) && message.contains(expectedMessage),
                            () -> "unexpected failure: " + error);
                })
                .verify(VERIFICATION_TIMEOUT);
    }

    /**
     * Asserts that cancelling the root subscription is observed by the plugin.
     *
     * @param runtime runtime fixture
     * @param exchange request exchange
     * @param plugin plugin that should observe cancellation
     */
    public static void assertCancellation(final LegacyPluginRuntime runtime, final ServerWebExchange exchange,
                                          final RecordingPlugin plugin) {
        StepVerifier.create(runtime.handle(exchange)).thenCancel().verify(VERIFICATION_TIMEOUT);
        assertEquals(1, plugin.getCancellationCount(),
                () -> plugin.named() + " must observe exactly one cancellation");
    }

    /**
     * Asserts that plugin effects and lifecycle hooks do not run while the handler publisher is
     * assembled, and run once after the root subscription.
     *
     * @param runtime runtime fixture
     * @param exchange request exchange
     * @param plugin plugin expected to delegate once
     */
    public static void assertSubscriptionBoundary(final LegacyPluginRuntime runtime,
                                                  final ServerWebExchange exchange,
                                                  final RecordingPlugin plugin) {
        assertEquals(0, plugin.getExecuteCount(), () -> plugin.named() + " ran before handle() was subscribed");
        final Mono<Void> handle = runtime.handle(exchange);
        assertEquals(0, plugin.getExecuteCount(), () -> plugin.named() + " ran while handle() was assembled");
        StepVerifier.create(handle).expectSubscription().expectComplete().verify(VERIFICATION_TIMEOUT);
        assertEquals(1, plugin.getExecuteCount(), () -> plugin.named() + " must run exactly once");
        assertEquals(1, plugin.getSubscribedCount(), () -> plugin.named() + " publisher must be subscribed once");
        assertEquals(1, plugin.getChainInvocations(), () -> plugin.named() + " must delegate exactly once");
        assertEquals(1, plugin.getBeforeCount(), () -> plugin.named() + " before hook must run once");
        assertEquals(1, plugin.getAfterCount(), () -> plugin.named() + " after hook must run once");
    }

    /**
     * Asserts that a skipped plugin does not enter the plugin or its lifecycle hooks while the
     * downstream plugin still runs.
     *
     * @param runtime runtime fixture
     * @param exchange request exchange
     * @param skippedPlugin skipped plugin
     * @param downstreamPlugin downstream plugin
     */
    public static void assertSkip(final LegacyPluginRuntime runtime, final ServerWebExchange exchange,
                                  final RecordingPlugin skippedPlugin, final RecordingPlugin downstreamPlugin) {
        assertCompletes(runtime, exchange);
        assertEquals(0, skippedPlugin.getExecuteCount(),
                () -> skippedPlugin.named() + " must not execute when skip() is true");
        assertEquals(0, skippedPlugin.getBeforeCount(),
                () -> skippedPlugin.named() + " before hook must not run when skip() is true");
        assertEquals(0, skippedPlugin.getChainInvocations(),
                () -> skippedPlugin.named() + " must not delegate when skip() is true");
        assertEquals(1, downstreamPlugin.getExecuteCount(),
                () -> downstreamPlugin.named() + " must still execute after a skipped plugin");
        assertEquals(1, downstreamPlugin.getBeforeCount(),
                () -> downstreamPlugin.named() + " before hook must run after a skipped plugin");
        assertEquals(1, downstreamPlugin.getAfterCount(),
                () -> downstreamPlugin.named() + " after hook must run after a skipped plugin");
    }

    /**
     * Asserts that every root subscription creates a fresh chain, as the legacy handler does.
     *
     * @param runtime runtime fixture
     * @param exchange request exchange
     * @param plugin plugin expected to execute per subscription
     */
    public static void assertRepeatedSubscriptions(final LegacyPluginRuntime runtime,
                                                   final ServerWebExchange exchange,
                                                   final RecordingPlugin plugin) {
        final Mono<Void> handle = runtime.handle(exchange);
        StepVerifier.create(handle).expectSubscription().expectComplete().verify(VERIFICATION_TIMEOUT);
        StepVerifier.create(handle).expectSubscription().expectComplete().verify(VERIFICATION_TIMEOUT);
        assertEquals(2, plugin.getExecuteCount(),
                () -> plugin.named() + " must execute once per root subscription");
        assertEquals(2, plugin.getSubscribedCount(),
                () -> plugin.named() + " publisher must be subscribed once per root subscription");
        assertEquals(2, plugin.getBeforeCount(),
                () -> plugin.named() + " before hook must run once per root subscription");
        assertEquals(2, plugin.getAfterCount(),
                () -> plugin.named() + " after hook must run once per root subscription");
    }

    /**
     * Asserts that an effect is not observed while the handler publisher is assembled and is
     * observed after the root subscription.
     *
     * @param runtime runtime fixture
     * @param exchange request exchange
     * @param effectObserved effect probe supplied by the plugin's test
     * @param pluginName plugin name for diagnostics
     */
    public static void assertRootSubscriptionBoundary(final LegacyPluginRuntime runtime,
                                                      final ServerWebExchange exchange,
                                                      final BooleanSupplier effectObserved,
                                                      final String pluginName) {
        assertFalse(effectObserved.getAsBoolean(), () -> pluginName + " produced an effect before handle()");
        final Mono<Void> handle = runtime.handle(exchange);
        assertFalse(effectObserved.getAsBoolean(), () -> pluginName + " produced an effect while handle() was assembled");
        StepVerifier.create(handle).expectSubscription().expectComplete().verify(VERIFICATION_TIMEOUT);
        assertTrue(effectObserved.getAsBoolean(), () -> pluginName + " did not produce an effect after completion");
    }

    /**
     * Asserts the downstream shape declared by a probe's profile.
     *
     * <p>Pass-through and transform plugins must invoke and subscribe the downstream exactly once.
     * A short-circuit plugin must not reach downstream. A wrapper may decorate or resubscribe, so
     * it is only required to reach downstream at least once. A selector/rule plugin must delegate
     * when it is configured to continue.</p>
     *
     * @param probe plugin probe
     */
    public static void assertContinuation(final ContractProbe probe) {
        assertContinuationCounts(probe, probe.getExecuteCount(), probe.getChainInvocations(),
                probe.getDownstreamSubscriptions());
    }

    private static void assertContinuationCounts(final ContractProbe probe, final int executions,
                                                  final int invocations, final int subscriptions) {
        final String detail = probe.named() + " [" + probe.getProfile().displayName() + "]";
        assertTrue(executions > 0, () -> detail + " must execute before continuation is checked");
        switch (probe.getProfile()) {
            case PASS_THROUGH:
            case TRANSFORM:
                assertEquals(executions, invocations,
                        () -> detail + " must invoke downstream exactly once per execution");
                assertEquals(executions, subscriptions,
                        () -> detail + " must subscribe downstream exactly once per execution");
                break;
            case SHORT_CIRCUIT:
                assertEquals(0, invocations,
                        () -> detail + " must not invoke downstream");
                assertEquals(0, subscriptions,
                        () -> detail + " must not subscribe downstream");
                break;
            case DOWNSTREAM_WRAPPER:
                assertTrue(invocations >= executions,
                        () -> detail + " wrapper must invoke downstream at least once");
                assertTrue(subscriptions >= executions,
                        () -> detail + " wrapper must subscribe downstream at least once");
                break;
            case SELECTOR_RULE:
                assertTrue(invocations >= executions,
                        () -> detail + " must delegate after a matching selector/rule");
                assertTrue(subscriptions >= executions,
                        () -> detail + " must subscribe downstream after a matching selector/rule");
                break;
            default:
                throw new IllegalStateException("unsupported contract profile: " + probe.getProfile());
        }
    }

    /**
     * Asserts that cancelling the root subscription is observed by a wrapped plugin.
     *
     * @param runtime runtime fixture
     * @param exchange request exchange
     * @param plugin plugin probe
     */
    public static void assertProbeCancellation(final LegacyPluginRuntime runtime, final ServerWebExchange exchange,
                                               final ContractProbe plugin) {
        StepVerifier.create(runtime.handle(exchange)).thenCancel().verify(VERIFICATION_TIMEOUT);
        assertEquals(1, plugin.getCancellationCount(),
                () -> plugin.named() + " [" + plugin.getProfile().displayName()
                        + "] must observe exactly one cancellation");
    }

    /**
     * Cancels a synchronously acquired resource and checks the plugin-owned cleanup observation.
     * For asynchronously acquired resources, drive acquisition and cancellation with a custom StepVerifier.
     *
     * @param runtime runtime fixture
     * @param exchange request exchange
     * @param plugin plugin probe
     * @param cleanupObserved true after the delegate releases its resource or cancels its upstream
     */
    public static void assertCancellationCleanup(final LegacyPluginRuntime runtime, final ServerWebExchange exchange,
                                                 final ContractProbe plugin, final BooleanSupplier cleanupObserved) {
        final String detail = plugin.named() + " [" + plugin.getProfile().displayName() + "]";
        assertFalse(cleanupObserved.getAsBoolean(), () -> detail + " cleanup must not precede resource acquisition");
        assertProbeCancellation(runtime, exchange, plugin);
        assertTrue(cleanupObserved.getAsBoolean(), () -> detail + " cancellation must release the plugin-owned resource");
    }

    /**
     * Asserts that every root subscription creates a fresh chain for a wrapped plugin.
     *
     * @param runtime runtime fixture
     * @param exchange request exchange
     * @param plugin plugin probe
     */
    public static void assertProbeRepeatedSubscriptions(final LegacyPluginRuntime runtime,
                                                        final ServerWebExchange exchange,
                                                        final ContractProbe plugin) {
        final Mono<Void> handle = runtime.handle(exchange);
        for (int subscription = 0; subscription < 2; subscription++) {
            final int executions = plugin.getExecuteCount();
            final int subscriptions = plugin.getSubscribedCount();
            final int invocations = plugin.getChainInvocations();
            final int downstreamSubscriptions = plugin.getDownstreamSubscriptions();
            StepVerifier.create(handle).expectSubscription().expectComplete().verify(VERIFICATION_TIMEOUT);
            assertEquals(1, plugin.getExecuteCount() - executions,
                    () -> plugin.named() + " must execute once per root subscription");
            assertEquals(1, plugin.getSubscribedCount() - subscriptions,
                    () -> plugin.named() + " publisher must be subscribed once per root subscription");
            assertContinuationCounts(plugin, 1, plugin.getChainInvocations() - invocations,
                    plugin.getDownstreamSubscriptions() - downstreamSubscriptions);
        }
    }

    /**
     * Asserts that a skipped wrapped plugin does not execute while the downstream fixture runs.
     *
     * @param runtime runtime fixture
     * @param exchange request exchange
     * @param skippedPlugin skipped plugin probe
     * @param downstreamPlugin downstream fixture
     */
    public static void assertProbeSkip(final LegacyPluginRuntime runtime, final ServerWebExchange exchange,
                                       final ContractProbe skippedPlugin, final RecordingPlugin downstreamPlugin) {
        assertCompletes(runtime, exchange);
        assertEquals(0, skippedPlugin.getExecuteCount(),
                () -> skippedPlugin.named() + " must not execute when skip() is true");
        assertEquals(0, skippedPlugin.getChainInvocations(),
                () -> skippedPlugin.named() + " must not delegate when skip() is true");
        assertEquals(1, downstreamPlugin.getExecuteCount(),
                () -> downstreamPlugin.named() + " must still execute after a skipped plugin");
    }
}
