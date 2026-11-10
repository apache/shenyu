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

import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Assertions for the observable part of the legacy {@code ShenyuPlugin} contract.
 *
 * <p>The checks drive the real {@link LegacyPluginRuntime}; they do not create a replacement
 * chain. A failure identifies the plugin name and the boundary that was violated.</p>
 */
public final class PluginContractAssertions {

    private PluginContractAssertions() {
    }

    /**
     * Asserts that the chain completes normally.
     *
     * @param runtime runtime fixture
     * @param exchange request exchange
     */
    public static void assertCompletes(final LegacyPluginRuntime runtime, final ServerWebExchange exchange) {
        StepVerifier.create(runtime.handle(exchange)).expectSubscription().verifyComplete();
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
                .verify();
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
        StepVerifier.create(runtime.handle(exchange)).thenCancel().verify();
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
        StepVerifier.create(handle).expectSubscription().verifyComplete();
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
        StepVerifier.create(handle).expectSubscription().verifyComplete();
        StepVerifier.create(handle).expectSubscription().verifyComplete();
        assertEquals(2, plugin.getExecuteCount(),
                () -> plugin.named() + " must execute once per root subscription");
        assertEquals(2, plugin.getSubscribedCount(),
                () -> plugin.named() + " publisher must be subscribed once per root subscription");
        assertEquals(2, plugin.getBeforeCount(),
                () -> plugin.named() + " before hook must run once per root subscription");
        assertEquals(2, plugin.getAfterCount(),
                () -> plugin.named() + " after hook must run once per root subscription");
    }
}
