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

package org.apache.shenyu.plugin.base.match;

import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.base.cache.MatchDataCache;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Tests for the matching-decision observation emitted from
 * {@link AbstractShenyuPlugin}: every decision branch reports the real branch, cache hits
 * report their source without claiming condition evaluation, and instrumentation can never
 * change the request outcome.
 */
public final class SelectorRuleMatchObservationTest {

    private static final String PATH = "/http/SHENYU/SHENYU";

    private final List<SelectorRuleMatchObservation> events = new ArrayList<>();

    private TestShenyuPlugin testShenyuPlugin;

    private ShenyuPluginChain shenyuPluginChain;

    private ServerWebExchange exchange;

    private PluginData pluginData;

    private SelectorData selectorData;

    private RuleData ruleData;

    private ConditionData conditionData;

    @BeforeEach
    public void setUp() {
        mockShenyuConfig(false);
        this.pluginData = PluginData.builder().name("SHENYU").enabled(true).build();
        this.conditionData = new ConditionData();
        this.conditionData.setOperator("match");
        this.conditionData.setParamName("/");
        this.conditionData.setParamType("uri");
        this.conditionData.setParamValue("/http/**");
        this.selectorData = SelectorData.builder().id("s1").pluginName("SHENYU")
                .enabled(true).matchRestful(false).logged(true)
                .type(SelectorTypeEnum.CUSTOM_FLOW.getCode())
                .matchMode(0).sort(1).continued(true)
                .conditionList(Collections.singletonList(conditionData)).build();
        this.ruleData = RuleData.builder().id("r1").pluginName("SHENYU").selectorId("s1")
                .enabled(true).logged(true).matchRestful(false)
                .matchMode(0).sort(1)
                .conditionDataList(Collections.singletonList(conditionData)).build();
        this.shenyuPluginChain = mock(ShenyuPluginChain.class);
        this.testShenyuPlugin = spy(new TestShenyuPlugin());
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest.get(PATH).build());
        ShenyuContext context = mock(ShenyuContext.class);
        exchange.getAttributes().put(Constants.CONTEXT, context);
        when(shenyuPluginChain.execute(exchange)).thenReturn(Mono.empty());
        clearCaches();
        SelectorRuleMatchObserverHolder.register(events::add);
    }

    @AfterEach
    public void cleanUp() {
        SelectorRuleMatchObserverHolder.unregister();
        clearCaches();
        MatchDataCache.getInstance().cleanSelectorData();
        MatchDataCache.getInstance().cleanRuleDataData();
    }

    @Test
    public void missingPluginDataReportsNoPluginData() {
        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).verifyComplete();
        verify(shenyuPluginChain).execute(exchange);
        verify(testShenyuPlugin, never()).doExecute(any(), any(), any(), any());
        assertEquals(1, events.size());
        SelectorRuleMatchObservation observation = events.get(0);
        assertEquals(SelectorRuleMatchObservation.Decision.NO_PLUGIN_DATA, observation.getDecision());
        assertEquals("SHENYU", observation.getPluginName());
        assertEquals(SelectorRuleMatchObservation.CacheResolution.NOT_CONSULTED, observation.getSelectorCache());
        assertEquals(SelectorRuleMatchObservation.CacheResolution.NOT_CONSULTED, observation.getRuleCache());
    }

    @Test
    public void disabledPluginReportsPluginDisabled() {
        BaseDataCache.getInstance().cachePluginData(PluginData.builder().name("SHENYU").enabled(false).build());
        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).verifyComplete();
        assertEquals(1, events.size());
        assertEquals(SelectorRuleMatchObservation.Decision.PLUGIN_DISABLED, events.get(0).getDecision());
    }

    @Test
    public void emptySelectorListReportsNoSelector() {
        BaseDataCache.getInstance().cachePluginData(pluginData);
        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).verifyComplete();
        assertEquals(1, events.size());
        SelectorRuleMatchObservation observation = events.get(0);
        assertEquals(SelectorRuleMatchObservation.Decision.NO_SELECTOR, observation.getDecision());
        assertEquals(SelectorRuleMatchObservation.CacheResolution.NOT_CONSULTED, observation.getSelectorCache());
    }

    @Test
    public void unmatchedSelectorReportsSelectorUnmatched() {
        BaseDataCache.getInstance().cachePluginData(pluginData);
        BaseDataCache.getInstance().cacheSelectData(unmatchedSelector());
        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).verifyComplete();
        assertEquals(1, events.size());
        SelectorRuleMatchObservation observation = events.get(0);
        assertEquals(SelectorRuleMatchObservation.Decision.SELECTOR_UNMATCHED, observation.getDecision());
        // the match cache is disabled in this fixture: never consulted, nothing evaluated by it
        assertEquals(SelectorRuleMatchObservation.CacheResolution.NOT_CONSULTED, observation.getSelectorCache());
    }

    @Test
    public void negativeSelectorCacheHitReportsNoSelectorFromCache() {
        mockShenyuConfig(true);
        BaseDataCache.getInstance().cachePluginData(pluginData);
        BaseDataCache.getInstance().cacheSelectData(unmatchedSelector());
        // first request evaluates live and warms the negative entry for this path
        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).verifyComplete();
        assertEquals(SelectorRuleMatchObservation.Decision.SELECTOR_UNMATCHED, events.get(0).getDecision());
        // second request is served from the negative cache entry
        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).verifyComplete();
        assertEquals(2, events.size());
        SelectorRuleMatchObservation observation = events.get(1);
        assertEquals(SelectorRuleMatchObservation.Decision.NO_SELECTOR, observation.getDecision());
        assertEquals(SelectorRuleMatchObservation.CacheResolution.NEGATIVE_HIT, observation.getSelectorCache());
    }

    @Test
    public void successfulMatchReportsSelectedWithIds() {
        seedSuccessfulConfig();
        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).verifyComplete();
        verify(testShenyuPlugin).doExecute(exchange, shenyuPluginChain, selectorData, ruleData);
        assertEquals(1, events.size());
        SelectorRuleMatchObservation observation = events.get(0);
        assertEquals(SelectorRuleMatchObservation.Decision.SELECTED, observation.getDecision());
        assertEquals("s1", observation.getSelectorId());
        assertEquals("r1", observation.getRuleId());
        assertFalse(observation.isDefaultRule());
        // cache disabled: both stages evaluated live without consulting the cache
        assertEquals(SelectorRuleMatchObservation.CacheResolution.NOT_CONSULTED, observation.getSelectorCache());
        assertEquals(SelectorRuleMatchObservation.CacheResolution.NOT_CONSULTED, observation.getRuleCache());
    }

    @Test
    public void positiveCacheHitsReportCachedSourceOnly() {
        mockShenyuConfig(true);
        seedSuccessfulConfig();
        // warm both positive entries with the first request
        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).verifyComplete();
        assertEquals(SelectorRuleMatchObservation.Decision.SELECTED, events.get(0).getDecision());
        events.clear();
        // second request is served entirely from the per-path match cache
        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).verifyComplete();
        verify(testShenyuPlugin, times(2)).doExecute(exchange, shenyuPluginChain, selectorData, ruleData);
        assertEquals(1, events.size());
        SelectorRuleMatchObservation observation = events.get(0);
        assertEquals(SelectorRuleMatchObservation.Decision.SELECTED, observation.getDecision());
        assertEquals(SelectorRuleMatchObservation.CacheResolution.POSITIVE_HIT, observation.getSelectorCache());
        assertEquals(SelectorRuleMatchObservation.CacheResolution.POSITIVE_HIT, observation.getRuleCache());
        assertEquals("s1", observation.getSelectorId());
        assertEquals("r1", observation.getRuleId());
    }

    @Test
    public void notContinuedSelectorReportsSyntheticDefaultRule() {
        seedSuccessfulConfig();
        BaseDataCache.getInstance().cacheSelectData(SelectorData.builder().id("s1").pluginName("SHENYU")
                .enabled(true).matchRestful(false).logged(true)
                .type(SelectorTypeEnum.CUSTOM_FLOW.getCode())
                .matchMode(0).sort(1).continued(false)
                .conditionList(Collections.singletonList(conditionData)).build());
        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).verifyComplete();
        assertEquals(1, events.size());
        SelectorRuleMatchObservation observation = events.get(0);
        assertEquals(SelectorRuleMatchObservation.Decision.SELECTOR_NOT_CONTINUED, observation.getDecision());
        assertEquals(Constants.DEFAULT_RULE, observation.getRuleId());
        assertTrue(observation.isDefaultRule());
    }

    @Test
    public void fullFlowSelectorReportsFullFlowRuleWithoutEvaluation() {
        seedSuccessfulConfig();
        BaseDataCache.getInstance().cacheSelectData(SelectorData.builder().id("s1").pluginName("SHENYU")
                .enabled(true).matchRestful(false).logged(true)
                .type(SelectorTypeEnum.FULL_FLOW.getCode())
                .matchMode(0).sort(1).continued(true).build());
        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).verifyComplete();
        assertEquals(1, events.size());
        SelectorRuleMatchObservation observation = events.get(0);
        assertEquals(SelectorRuleMatchObservation.Decision.FULL_FLOW_RULE, observation.getDecision());
        assertEquals("r1", observation.getRuleId());
        assertFalse(observation.isDefaultRule());
    }

    @Test
    public void fullFlowWithoutEnabledRuleReportsNoRule() {
        seedSuccessfulConfig();
        BaseDataCache.getInstance().cacheSelectData(SelectorData.builder().id("s1").pluginName("SHENYU")
                .enabled(true).matchRestful(false).logged(true)
                .type(SelectorTypeEnum.FULL_FLOW.getCode())
                .matchMode(0).sort(1).continued(true).build());
        BaseDataCache.getInstance().cacheRuleData(RuleData.builder().id("r1").pluginName("SHENYU").selectorId("s1")
                .enabled(false).matchRestful(false).matchMode(0).sort(1)
                .conditionDataList(Collections.singletonList(conditionData)).build());
        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).verifyComplete();
        assertEquals(1, events.size());
        assertEquals(SelectorRuleMatchObservation.Decision.FULL_FLOW_NO_ENABLED_RULE, events.get(0).getDecision());
    }

    @Test
    public void negativeRuleCacheHitReportsNoRuleFromCache() {
        mockShenyuConfig(true);
        BaseDataCache.getInstance().cachePluginData(pluginData);
        BaseDataCache.getInstance().cacheSelectData(selectorData);
        // the rule list carries only non-matching, path-only, uniquely decidable rules,
        // so the first request evaluates live and warms the negative rule entry
        BaseDataCache.getInstance().cacheRuleData(RuleData.builder().id("r2").pluginName("SHENYU").selectorId("s1")
                .enabled(true).logged(true).matchRestful(false).matchMode(0).sort(1)
                .conditionDataList(Collections.singletonList(unmatchedCondition())).build());
        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).verifyComplete();
        assertEquals(1, events.size());
        assertEquals(SelectorRuleMatchObservation.Decision.RULE_UNMATCHED, events.get(0).getDecision());
        events.clear();
        // the second request is served by the negative rule cache entry
        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).verifyComplete();
        assertEquals(1, events.size());
        SelectorRuleMatchObservation observation = events.get(0);
        assertEquals(SelectorRuleMatchObservation.Decision.NO_RULE, observation.getDecision());
        assertEquals(SelectorRuleMatchObservation.CacheResolution.NEGATIVE_HIT, observation.getRuleCache());
    }

    @Test
    public void emptyRuleListReportsNoRule() {
        BaseDataCache.getInstance().cachePluginData(pluginData);
        BaseDataCache.getInstance().cacheSelectData(selectorData);
        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).verifyComplete();
        assertEquals(1, events.size());
        assertEquals(SelectorRuleMatchObservation.Decision.NO_RULE, events.get(0).getDecision());
        assertEquals("s1", events.get(0).getSelectorId());
    }

    @Test
    public void unmatchedRuleReportsRuleUnmatched() {
        BaseDataCache.getInstance().cachePluginData(pluginData);
        BaseDataCache.getInstance().cacheSelectData(selectorData);
        RuleData other = RuleData.builder().id("r2").pluginName("SHENYU").selectorId("s1")
                .enabled(true).logged(true).matchRestful(false)
                .matchMode(0).sort(1)
                .conditionDataList(Collections.singletonList(unmatchedCondition())).build();
        BaseDataCache.getInstance().cacheRuleData(other);
        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).verifyComplete();
        assertEquals(1, events.size());
        SelectorRuleMatchObservation observation = events.get(0);
        assertEquals(SelectorRuleMatchObservation.Decision.RULE_UNMATCHED, observation.getDecision());
        assertEquals(SelectorRuleMatchObservation.CacheResolution.NOT_CONSULTED, observation.getRuleCache());
    }

    @Test
    public void throwingObserverDoesNotAffectRequest() {
        seedSuccessfulConfig();
        SelectorRuleMatchObserverHolder.register(observation -> {
            throw new IllegalStateException("broken consumer");
        });
        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).verifyComplete();
        verify(testShenyuPlugin).doExecute(exchange, shenyuPluginChain, selectorData, ruleData);
    }

    @Test
    public void registrationTokenUnregistersOnlyItsOwnObserver() throws Exception {
        seedSuccessfulConfig();
        AutoCloseable token = SelectorRuleMatchObserverHolder.register(events::add);
        SelectorRuleMatchObserver later = observation -> { };
        final AutoCloseable laterToken = SelectorRuleMatchObserverHolder.register(later);
        token.close();
        // the later registration must survive closing an earlier token
        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).verifyComplete();
        verify(testShenyuPlugin).doExecute(exchange, shenyuPluginChain, selectorData, ruleData);
        assertTrue(events.isEmpty(), "the later observer discards, but stays registered");
        laterToken.close();
        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).verifyComplete();
        verify(testShenyuPlugin, org.mockito.Mockito.times(2)).doExecute(exchange, shenyuPluginChain, selectorData, ruleData);
    }

    @Test
    public void unregisteredObserverProducesNoEventsAndNoFailure() {
        seedSuccessfulConfig();
        SelectorRuleMatchObserverHolder.unregister();
        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).verifyComplete();
        verify(testShenyuPlugin).doExecute(exchange, shenyuPluginChain, selectorData, ruleData);
        assertTrue(events.isEmpty());
    }

    private void seedSuccessfulConfig() {
        BaseDataCache.getInstance().cachePluginData(pluginData);
        BaseDataCache.getInstance().cacheSelectData(selectorData);
        BaseDataCache.getInstance().cacheRuleData(ruleData);
    }

    private SelectorData unmatchedSelector() {
        return SelectorData.builder().id("sx").pluginName("SHENYU")
                .enabled(true).matchRestful(false).logged(true)
                .type(SelectorTypeEnum.CUSTOM_FLOW.getCode())
                .matchMode(0).sort(1).continued(true)
                .conditionList(Collections.singletonList(unmatchedCondition())).build();
    }

    private ConditionData unmatchedCondition() {
        ConditionData data = new ConditionData();
        data.setOperator("match");
        data.setParamName("/");
        data.setParamType("uri");
        data.setParamValue("/other/**");
        return data;
    }

    private void clearCaches() {
        BaseDataCache.getInstance().cleanPluginData();
        BaseDataCache.getInstance().cleanSelectorData();
        BaseDataCache.getInstance().cleanRuleData();
    }

    private void mockShenyuConfig(final boolean cacheEnabled) {
        ShenyuConfig shenyuConfig = new ShenyuConfig();
        shenyuConfig.getSelectorMatchCache().getCache().setEnabled(cacheEnabled);
        shenyuConfig.getRuleMatchCache().getCache().setEnabled(cacheEnabled);
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        when(context.getBean(ShenyuConfig.class)).thenReturn(shenyuConfig);
        SpringBeanUtils.getInstance().setApplicationContext(context);
    }

    static class TestShenyuPlugin extends AbstractShenyuPlugin {

        @Override
        protected Mono<Void> doExecute(final ServerWebExchange serverExchange, final ShenyuPluginChain pluginChain,
                                       final SelectorData selector, final RuleData rule) {
            return Mono.empty();
        }

        @Override
        public int getOrder() {
            return 0;
        }

        @Override
        public String named() {
            return "SHENYU";
        }
    }
}
