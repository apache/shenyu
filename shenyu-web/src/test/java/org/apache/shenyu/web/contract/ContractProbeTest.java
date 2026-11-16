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

import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.junit.jupiter.api.Test;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.concurrent.atomic.AtomicBoolean;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Tests the reusable probe used by out-of-tree plugin contract tests.
 */
public class ContractProbeTest {

    @Test
    public void passThroughProbeRecordsOneContinuation() {
        final RecordingPlugin delegate = new RecordingPlugin("delegate", 1, RecordingPlugin.Role.PASS_THROUGH);
        final ContractProbe probe = ContractProbe.around(delegate, "probe", 1,
                PluginContractProfile.PASS_THROUGH);
        final LegacyPluginRuntime runtime = new LegacyPluginRuntime(probe);
        final ServerWebExchange exchange = LegacyPluginRuntime.newExchange();

        PluginContractAssertions.assertRootSubscriptionBoundary(runtime, exchange,
                () -> probe.getExecuteCount() > 0, "probe");
        PluginContractAssertions.assertContinuation(probe);

        assertEquals(1, delegate.getChainInvocations());
    }

    @Test
    public void shortCircuitProbeDoesNotReachDownstream() {
        final ContractProbe probe = ContractProbe.around(
                new RecordingPlugin("short", 1, RecordingPlugin.Role.SHORT_CIRCUIT),
                "short", 1, PluginContractProfile.SHORT_CIRCUIT);
        final RecordingPlugin downstream = new RecordingPlugin("downstream", 2,
                RecordingPlugin.Role.PASS_THROUGH);
        final LegacyPluginRuntime runtime = new LegacyPluginRuntime(probe, downstream);

        PluginContractAssertions.assertCompletes(runtime, LegacyPluginRuntime.newExchange());
        PluginContractAssertions.assertContinuation(probe);

        assertEquals(0, downstream.getExecuteCount());
    }

    @Test
    public void wrapperProbeAllowsDownstreamDecoration() {
        final ContractProbe probe = ContractProbe.around(
                new RecordingPlugin("wrapper", 1, RecordingPlugin.Role.WRAPPER),
                "wrapper", 1, PluginContractProfile.DOWNSTREAM_WRAPPER);
        final LegacyPluginRuntime runtime = new LegacyPluginRuntime(probe);
        final ServerWebExchange exchange = LegacyPluginRuntime.newExchange();

        PluginContractAssertions.assertCompletes(runtime, exchange);
        PluginContractAssertions.assertContinuation(probe);

        assertTrue(exchange.getAttributes().containsKey(RecordingPlugin.WRAPPED_ATTRIBUTE));
    }

    @Test
    public void badPassThroughIsRejected() {
        final ShenyuPlugin bad = new ShenyuPlugin() {
            @Override
            public Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
                return Mono.empty();
            }

            @Override
            public int getOrder() {
                return 1;
            }

            @Override
            public String named() {
                return "bad-pass-through";
            }
        };
        final ContractProbe probe = ContractProbe.around(bad, "bad-pass-through", 1,
                PluginContractProfile.PASS_THROUGH);
        final LegacyPluginRuntime runtime = new LegacyPluginRuntime(probe);

        PluginContractAssertions.assertCompletes(runtime, LegacyPluginRuntime.newExchange());

        assertThrows(AssertionError.class, () -> PluginContractAssertions.assertContinuation(probe));
    }

    @Test
    public void eagerEffectIsRejectedBeforeRootSubscription() {
        final AtomicBoolean effect = new AtomicBoolean(true);
        final ShenyuPlugin eager = new ShenyuPlugin() {
            @Override
            public Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
                return Mono.empty();
            }

            @Override
            public int getOrder() {
                return 1;
            }

            @Override
            public String named() {
                return "eager-effect";
            }
        };
        final ContractProbe probe = ContractProbe.around(eager, "eager-effect", 1,
                PluginContractProfile.PASS_THROUGH);
        final LegacyPluginRuntime runtime = new LegacyPluginRuntime(probe);

        assertThrows(AssertionError.class, () -> PluginContractAssertions.assertRootSubscriptionBoundary(
                runtime, LegacyPluginRuntime.newExchange(), effect::get, "eager-effect"));
    }

    @Test
    public void cancellationProbeObservesRootCancellation() {
        final ContractProbe probe = ContractProbe.around(
                new RecordingPlugin("cancellation", 1, RecordingPlugin.Role.CANCELLATION),
                "cancellation", 1, PluginContractProfile.PASS_THROUGH);
        final LegacyPluginRuntime runtime = new LegacyPluginRuntime(probe);

        PluginContractAssertions.assertProbeCancellation(runtime, LegacyPluginRuntime.newExchange(), probe);
    }

    @Test
    public void repeatedSubscriptionsReachDownstreamAgain() {
        final ContractProbe probe = ContractProbe.around(
                new RecordingPlugin("repeated", 1, RecordingPlugin.Role.PASS_THROUGH),
                "repeated", 1, PluginContractProfile.PASS_THROUGH);
        final LegacyPluginRuntime runtime = new LegacyPluginRuntime(probe);

        PluginContractAssertions.assertProbeRepeatedSubscriptions(runtime, LegacyPluginRuntime.newExchange(), probe);
    }

    @Test
    public void skipProbeDoesNotEnterPlugin() {
        final ContractProbe probe = ContractProbe.around(
                new RecordingPlugin("skipped", 1, RecordingPlugin.Role.PASS_THROUGH, true),
                "skipped", 1, PluginContractProfile.PASS_THROUGH);
        final RecordingPlugin downstream = new RecordingPlugin("downstream", 2,
                RecordingPlugin.Role.PASS_THROUGH);
        final LegacyPluginRuntime runtime = new LegacyPluginRuntime(probe, downstream);

        PluginContractAssertions.assertProbeSkip(runtime, LegacyPluginRuntime.newExchange(), probe, downstream);
    }
}
