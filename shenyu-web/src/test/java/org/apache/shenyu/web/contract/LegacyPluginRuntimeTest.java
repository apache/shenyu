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

import org.junit.jupiter.api.Test;
import org.springframework.web.server.ServerWebExchange;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Verifies the reusable legacy plugin contract fixture.
 */
public class LegacyPluginRuntimeTest {

    @Test
    public void passThroughCompletesAtTheRootSubscriptionBoundary() {
        final RecordingPlugin plugin = new RecordingPlugin("pass-through", 1, RecordingPlugin.Role.PASS_THROUGH);
        final LegacyPluginRuntime runtime = new LegacyPluginRuntime(plugin);

        PluginContractAssertions.assertSubscriptionBoundary(runtime, LegacyPluginRuntime.newExchange(), plugin);
    }

    @Test
    public void transformAppliesItsEffectOnSubscription() {
        final RecordingPlugin plugin = new RecordingPlugin("transform", 1, RecordingPlugin.Role.TRANSFORM);
        final LegacyPluginRuntime runtime = new LegacyPluginRuntime(plugin);
        final ServerWebExchange exchange = LegacyPluginRuntime.newExchange();

        PluginContractAssertions.assertSubscriptionBoundary(runtime, exchange, plugin);

        assertEquals("transform", exchange.getAttributes().get(RecordingPlugin.TRANSFORMED_ATTRIBUTE));
    }

    @Test
    public void shortCircuitCompletesWithoutInvokingDownstream() {
        final RecordingPlugin shortCircuit = new RecordingPlugin("short-circuit", 1, RecordingPlugin.Role.SHORT_CIRCUIT);
        final RecordingPlugin downstream = new RecordingPlugin("downstream", 2, RecordingPlugin.Role.PASS_THROUGH);
        final LegacyPluginRuntime runtime = new LegacyPluginRuntime(shortCircuit, downstream);

        PluginContractAssertions.assertCompletes(runtime, LegacyPluginRuntime.newExchange());

        assertEquals(1, shortCircuit.getExecuteCount());
        assertEquals(0, shortCircuit.getChainInvocations());
        assertEquals(0, downstream.getExecuteCount(), "a short-circuit must not reach the downstream plugin");
    }

    @Test
    public void skippedPluginDoesNotEnterThePluginOrLifecycleHooks() {
        final RecordingPlugin skipped = new RecordingPlugin("skipped", 1, RecordingPlugin.Role.PASS_THROUGH, true);
        final RecordingPlugin downstream = new RecordingPlugin("downstream", 2, RecordingPlugin.Role.PASS_THROUGH);
        final LegacyPluginRuntime runtime = new LegacyPluginRuntime(skipped, downstream);

        PluginContractAssertions.assertSkip(runtime, LegacyPluginRuntime.newExchange(), skipped, downstream);
    }

    @Test
    public void cancellationReachesThePlugin() {
        final RecordingPlugin plugin = new RecordingPlugin("cancellation", 1, RecordingPlugin.Role.CANCELLATION);
        final LegacyPluginRuntime runtime = new LegacyPluginRuntime(plugin);

        PluginContractAssertions.assertCancellation(runtime, LegacyPluginRuntime.newExchange(), plugin);
    }

    @Test
    public void failurePropagatesWithThePluginMessage() {
        final RecordingPlugin plugin = new RecordingPlugin("failure", 1, RecordingPlugin.Role.FAILURE);
        final LegacyPluginRuntime runtime = new LegacyPluginRuntime(plugin);

        PluginContractAssertions.assertFails(runtime, LegacyPluginRuntime.newExchange(), "plugin failure failed");
    }

    @Test
    public void repeatedSubscriptionsUseFreshChains() {
        final RecordingPlugin plugin = new RecordingPlugin("repeated", 1, RecordingPlugin.Role.PASS_THROUGH);
        final LegacyPluginRuntime runtime = new LegacyPluginRuntime(plugin);

        PluginContractAssertions.assertRepeatedSubscriptions(runtime, LegacyPluginRuntime.newExchange(), plugin);

        assertEquals(2, plugin.getBeforeCount());
        assertEquals(2, plugin.getAfterCount());
    }
}
