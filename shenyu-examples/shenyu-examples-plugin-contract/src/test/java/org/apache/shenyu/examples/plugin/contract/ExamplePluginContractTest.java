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

import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.web.contract.ContractProbe;
import org.apache.shenyu.web.contract.LegacyPluginRuntime;
import org.apache.shenyu.web.contract.PluginContractAssertions;
import org.apache.shenyu.web.contract.PluginContractMetadata;
import org.apache.shenyu.web.contract.PluginContractProfile;
import org.apache.shenyu.web.contract.RecordingPlugin;
import org.apache.shenyu.web.contract.SelectorRuleFixture;
import org.junit.jupiter.api.Test;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.EnumSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Example consumer of the published reactive plugin contract test kit.
 *
 * <p>This module intentionally lives outside {@code shenyu-web} and only depends on the
 * installed {@code shenyu-web} test-jar. It is the compatibility proof for third-party
 * plugin projects.</p>
 */
public class ExamplePluginContractTest {

    private static final String TRANSFORM_ATTRIBUTE = "example.transformed";

    private static final String WRAPPER_ATTRIBUTE = "example.wrapped";

    @Test
    public void transformPluginObservesRootSubscriptionBoundary() {
        final TransformPlugin plugin = new TransformPlugin();
        final ContractProbe probe = ContractProbe.around(plugin, "example-transform", 10,
                PluginContractProfile.TRANSFORM);
        final LegacyPluginRuntime runtime = new LegacyPluginRuntime(probe);
        final ServerWebExchange exchange = LegacyPluginRuntime.newExchange();

        PluginContractAssertions.assertRootSubscriptionBoundary(runtime, exchange,
                plugin::isEffectObserved, "example-transform");
        PluginContractAssertions.assertContinuation(probe);

        assertEquals(Boolean.TRUE, exchange.getAttributes().get(TRANSFORM_ATTRIBUTE));
    }

    @Test
    public void shortCircuitPluginDoesNotReachDownstream() {
        final ContractProbe probe = ContractProbe.around(new ShortCircuitPlugin(),
                "example-short-circuit", 20, PluginContractProfile.SHORT_CIRCUIT);
        final RecordingPlugin terminal = new RecordingPlugin("terminal", 100,
                RecordingPlugin.Role.PASS_THROUGH);
        final LegacyPluginRuntime runtime = new LegacyPluginRuntime(probe, terminal);

        PluginContractAssertions.assertCompletes(runtime, LegacyPluginRuntime.newExchange());
        PluginContractAssertions.assertContinuation(probe);

        assertEquals(0, terminal.getExecuteCount());
    }

    @Test
    public void wrapperPluginMayDecorateDownstream() {
        final ContractProbe probe = ContractProbe.around(new WrapperPlugin(),
                "example-wrapper", 20, PluginContractProfile.DOWNSTREAM_WRAPPER);
        final LegacyPluginRuntime runtime = new LegacyPluginRuntime(probe);
        final ServerWebExchange exchange = LegacyPluginRuntime.newExchange();

        PluginContractAssertions.assertCompletes(runtime, exchange);
        PluginContractAssertions.assertContinuation(probe);

        assertEquals(Boolean.TRUE, exchange.getAttributes().get(WRAPPER_ATTRIBUTE));
    }

    @Test
    public void cancellationPluginObservesRootCancellation() {
        final ContractProbe probe = ContractProbe.around(new CancellationPlugin(),
                "example-cancellation", 30, PluginContractProfile.PASS_THROUGH);
        final LegacyPluginRuntime runtime = new LegacyPluginRuntime(probe);

        PluginContractAssertions.assertProbeCancellation(runtime, LegacyPluginRuntime.newExchange(), probe);
    }

    @Test
    public void badPassThroughPluginIsRejected() {
        final ContractProbe probe = ContractProbe.around(new BadPassThroughPlugin(),
                "example-bad-pass-through", 40, PluginContractProfile.PASS_THROUGH);
        final LegacyPluginRuntime runtime = new LegacyPluginRuntime(probe);

        PluginContractAssertions.assertCompletes(runtime, LegacyPluginRuntime.newExchange());

        assertThrows(AssertionError.class, () -> PluginContractAssertions.assertContinuation(probe));
    }

    @Test
    public void eagerEffectIsRejectedBeforeSubscription() {
        final EagerPlugin plugin = new EagerPlugin();
        final ContractProbe probe = ContractProbe.around(plugin, "example-eager", 40,
                PluginContractProfile.PASS_THROUGH);
        final LegacyPluginRuntime runtime = new LegacyPluginRuntime(probe);

        assertThrows(AssertionError.class, () -> PluginContractAssertions.assertRootSubscriptionBoundary(
                runtime, LegacyPluginRuntime.newExchange(), plugin::isEffectObserved, "example-eager"));
    }

    @Test
    public void selectorRulePluginMatchesAndFallsBackAfterRemoval() {
        final ConditionData condition = new ConditionData();
        condition.setOperator("=");
        condition.setParamType("uri");
        condition.setParamName("/contract");
        condition.setParamValue("/contract");
        final SelectorData selectorData = SelectorData.builder()
                .id("example-selector-1")
                .pluginName("example-selector")
                .enabled(true)
                .matchRestful(false)
                .type(SelectorTypeEnum.CUSTOM_FLOW.getCode())
                .build();
        selectorData.setConditionList(List.of(condition));
        selectorData.setMatchMode(0);
        selectorData.setSort(1);
        selectorData.setLogged(true);
        final RuleData ruleData = RuleData.builder()
                .id("example-rule-1")
                .pluginName("example-selector")
                .selectorId("example-selector-1")
                .enabled(true)
                .matchRestful(false)
                .build();
        ruleData.setConditionDataList(List.of(condition));
        ruleData.setMatchMode(0);
        ruleData.setSort(1);
        ruleData.setLogged(true);
        final PluginData pluginData = PluginData.builder()
                .name("example-selector")
                .enabled(true)
                .build();

        try (SelectorRuleFixture fixture = new SelectorRuleFixture()) {
            fixture.cachePluginData(pluginData);
            fixture.cacheSelectorData(selectorData);
            fixture.cacheRuleData(ruleData);
            final SelectorPlugin plugin = new SelectorPlugin();
            final ContractProbe probe = ContractProbe.around(plugin, "example-selector", 10,
                    PluginContractProfile.SELECTOR_RULE);
            final RecordingPlugin terminal = new RecordingPlugin("terminal", 100,
                    RecordingPlugin.Role.PASS_THROUGH);
            final LegacyPluginRuntime runtime = new LegacyPluginRuntime(probe, terminal);

            PluginContractAssertions.assertCompletes(runtime, LegacyPluginRuntime.newExchange());
            assertEquals(1, plugin.getDoExecuteCount());
            assertEquals("example-selector-1", plugin.getMatchedSelector().getId());
            assertEquals("example-rule-1", plugin.getMatchedRule().getId());
            PluginContractAssertions.assertContinuation(probe);
            assertEquals(1, terminal.getExecuteCount());

            fixture.removeRuleData(ruleData);
            PluginContractAssertions.assertCompletes(runtime, LegacyPluginRuntime.newExchange());
            assertEquals(1, plugin.getDoExecuteCount());
            assertEquals(2, terminal.getExecuteCount());
            PluginContractAssertions.assertContinuation(probe);
        }
    }

    @Test
    public void metadataRecordsKitCoreAndSkippedCapabilities() {
        final PluginContractMetadata metadata = new PluginContractMetadata("example-plugin",
                EnumSet.of(PluginContractProfile.TRANSFORM, PluginContractProfile.DOWNSTREAM_WRAPPER),
                Set.of("fork", "real-gateway-e2e"));
        final String description = metadata.describe();

        assertTrue(description.contains("plugin=example-plugin"));
        assertTrue(description.contains("kit=" + PluginContractMetadata.KIT_VERSION));
        assertTrue(description.contains("core=" + metadata.getCoreVersion()));
        assertTrue(description.contains("skipped=fork,real-gateway-e2e"));
    }

    private static final class TransformPlugin implements ShenyuPlugin {

        private final AtomicBoolean effectObserved = new AtomicBoolean();

        @Override
        public Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
            return Mono.defer(() -> {
                effectObserved.set(true);
                exchange.getAttributes().put(TRANSFORM_ATTRIBUTE, Boolean.TRUE);
                return chain.execute(exchange);
            });
        }

        @Override
        public int getOrder() {
            return 10;
        }

        @Override
        public String named() {
            return "example-transform";
        }

        private boolean isEffectObserved() {
            return effectObserved.get();
        }
    }

    private static final class WrapperPlugin implements ShenyuPlugin {

        @Override
        public Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
            return chain.execute(exchange)
                    .doOnSuccess(ignored -> exchange.getAttributes().put(WRAPPER_ATTRIBUTE, Boolean.TRUE));
        }

        @Override
        public int getOrder() {
            return 20;
        }

        @Override
        public String named() {
            return "example-wrapper";
        }
    }

    private static final class ShortCircuitPlugin implements ShenyuPlugin {

        @Override
        public Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
            return Mono.empty();
        }

        @Override
        public int getOrder() {
            return 20;
        }

        @Override
        public String named() {
            return "example-short-circuit";
        }
    }

    private static final class CancellationPlugin implements ShenyuPlugin {

        @Override
        public Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
            return Mono.never();
        }

        @Override
        public int getOrder() {
            return 30;
        }

        @Override
        public String named() {
            return "example-cancellation";
        }
    }

    private static final class BadPassThroughPlugin implements ShenyuPlugin {

        @Override
        public Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
            return Mono.empty();
        }

        @Override
        public int getOrder() {
            return 40;
        }

        @Override
        public String named() {
            return "example-bad-pass-through";
        }
    }

    private static final class EagerPlugin implements ShenyuPlugin {

        private final AtomicBoolean effectObserved = new AtomicBoolean(true);

        @Override
        public Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
            return Mono.empty();
        }

        @Override
        public int getOrder() {
            return 40;
        }

        @Override
        public String named() {
            return "example-eager";
        }

        private boolean isEffectObserved() {
            return effectObserved.get();
        }
    }

    private static final class SelectorPlugin extends AbstractShenyuPlugin {

        private final AtomicInteger doExecuteCount = new AtomicInteger();

        private SelectorData matchedSelector;

        private RuleData matchedRule;

        @Override
        protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain,
                                       final SelectorData selector, final RuleData rule) {
            doExecuteCount.incrementAndGet();
            this.matchedSelector = selector;
            this.matchedRule = rule;
            return chain.execute(exchange);
        }

        @Override
        public int getOrder() {
            return 10;
        }

        @Override
        public String named() {
            return "example-selector";
        }

        private int getDoExecuteCount() {
            return doExecuteCount.get();
        }

        private SelectorData getMatchedSelector() {
            return Objects.requireNonNull(matchedSelector);
        }

        private RuleData getMatchedRule() {
            return Objects.requireNonNull(matchedRule);
        }
    }
}
