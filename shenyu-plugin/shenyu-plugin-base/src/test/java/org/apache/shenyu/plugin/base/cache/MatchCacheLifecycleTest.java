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

package org.apache.shenyu.plugin.base.cache;

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
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;

import reactor.core.publisher.Mono;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Regression coverage for matching across configuration and request changes.
 */
public final class MatchCacheLifecycleTest {

    private CommonPluginDataSubscriber subscriber;

    private Probe plugin;

    private ShenyuConfig config;

    @BeforeEach
    public void setup() {
        clear();
        config = new ShenyuConfig();
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        when(context.getBean(ShenyuConfig.class)).thenReturn(config);
        SpringBeanUtils.getInstance().setApplicationContext(context);
        subscriber = new CommonPluginDataSubscriber(Collections.emptyList(), mock(ApplicationEventPublisher.class),
                config.getSelectorMatchCache(), config.getRuleMatchCache());
        BaseDataCache.getInstance().cachePluginData(PluginData.builder().name("audit").enabled(true).build());
        plugin = new Probe();
    }

    @AfterEach
    public void clear() {
        BaseDataCache.getInstance().cleanPluginData();
        BaseDataCache.getInstance().cleanSelectorData();
        BaseDataCache.getInstance().cleanRuleData();
        MatchDataCache.getInstance().cleanSelectorData();
        MatchDataCache.getInstance().cleanRuleDataData();
    }

    private ConditionData condition(final String type, final String name, final String value) {
        ConditionData c = new ConditionData();
        c.setParamType(type);
        c.setParamName(name);
        c.setParamValue(value);
        c.setOperator("=");
        return c;
    }

    private SelectorData selector(final String id, final int sort, final ConditionData... conditions) {
        return SelectorData.builder().id(id).pluginName("audit").sort(sort).enabled(true).logged(false)
                .continued(true).matchRestful(false).matchMode(0).type(SelectorTypeEnum.CUSTOM_FLOW.getCode())
                .conditionList(Arrays.asList(conditions)).build();
    }

    private RuleData rule(final String id, final String selector, final int sort) {
        return RuleData.builder().id(id).selectorId(selector).pluginName("audit").sort(sort).enabled(true).loged(false)
                .matchRestful(false).matchMode(0).conditionDataList(List.of(condition("uri", "/", "/audit"))).build();
    }

    private String execute(final String header) {
        plugin.result = null;
        plugin.execute(MockServerWebExchange.from(MockServerHttpRequest.get("/audit").header("X-Tenant", header)), e -> Mono.empty()).block();
        return plugin.result;
    }

    @Test
    public void addedHigherPrioritySelectorMustReplaceCachedWinner() {
        subscriber.onSelectorSubscribe(selector("s1", 10, condition("uri", "/", "/audit")));
        subscriber.onRuleSubscribe(rule("r1", "s1", 10));
        assertEquals("s1:r1", execute("a"));
        subscriber.onSelectorSubscribe(selector("s2", 1, condition("uri", "/", "/audit")));
        subscriber.onRuleSubscribe(rule("r2", "s2", 1));
        assertEquals("s2:r2", execute("a"));
    }

    @Test
    public void addedHigherPriorityRuleMustReplaceCachedWinner() {
        subscriber.onSelectorSubscribe(selector("s1", 10, condition("uri", "/", "/audit")));
        subscriber.onRuleSubscribe(rule("r1", "s1", 10));
        assertEquals("s1:r1", execute("a"));
        subscriber.onRuleSubscribe(rule("r2", "s1", 1));
        assertEquals("s1:r2", execute("a"));
    }

    @Test
    public void headerMissMustNotPoisonAnotherRequest() {
        subscriber.onSelectorSubscribe(selector("s1", 1, condition("header", "X-Tenant", "a")));
        subscriber.onRuleSubscribe(rule("r1", "s1", 1));
        assertNull(execute("b"));
        assertEquals("s1:r1", execute("a"));
    }

    @Test
    public void ruleCacheMustBeScopedToSelectedSelector() {
        subscriber.onSelectorSubscribe(selector("s1", 1, condition("header", "X-Tenant", "a")));
        subscriber.onSelectorSubscribe(selector("s2", 2, condition("header", "X-Tenant", "b")));
        subscriber.onRuleSubscribe(rule("r1", "s1", 1));
        subscriber.onRuleSubscribe(rule("r2", "s2", 1));
        assertEquals("s1:r1", execute("a"));
        assertEquals("s2:r2", execute("b"));
    }

    @Test
    public void handlerFailureMustNotLeaveDeletedRuleCached() {
        PluginDataHandler handler = mock(PluginDataHandler.class);
        when(handler.pluginNamed()).thenReturn("audit");
        subscriber = new CommonPluginDataSubscriber(List.of(handler), mock(ApplicationEventPublisher.class),
                config.getSelectorMatchCache(), config.getRuleMatchCache());
        subscriber.onSelectorSubscribe(selector("s1", 1, condition("uri", "/", "/audit")));
        RuleData deleted = rule("r1", "s1", 1);
        subscriber.onRuleSubscribe(deleted);
        assertEquals("s1:r1", execute("a"));
        subscriber.onRuleSubscribe(rule("r2", "s1", 2));
        doThrow(new IllegalStateException("audit failure")).when(handler).removeRule(deleted);
        subscriber.unRuleSubscribe(deleted);
        assertEquals("s1:r2", execute("a"));
    }

    @Test
    public void positiveSelectorCacheMustAccountForDynamicCompetitors() {
        subscriber.onSelectorSubscribe(selector("s1", 10, condition("uri", "/", "/audit")));
        subscriber.onSelectorSubscribe(selector("s2", 1, condition("header", "X-Tenant", "a")));
        subscriber.onRuleSubscribe(rule("r1", "s1", 1));
        subscriber.onRuleSubscribe(rule("r2", "s2", 1));
        assertEquals("s1:r1", execute("b"));
        assertEquals("s2:r2", execute("a"));
    }

    @Test
    public void positiveRuleCacheMustAccountForDynamicCompetitors() {
        subscriber.onSelectorSubscribe(selector("s1", 1, condition("uri", "/", "/audit")));
        subscriber.onRuleSubscribe(rule("r1", "s1", 10));
        RuleData dynamic = rule("r2", "s1", 1);
        dynamic.setConditionDataList(List.of(condition("header", "X-Tenant", "a")));
        subscriber.onRuleSubscribe(dynamic);
        assertEquals("s1:r1", execute("b"));
        assertEquals("s1:r2", execute("a"));
    }

    @Test
    public void negativeRuleCacheMustBeScopedToSelectedSelector() {
        subscriber.onSelectorSubscribe(selector("s1", 1, condition("header", "X-Tenant", "a")));
        subscriber.onSelectorSubscribe(selector("s2", 2, condition("header", "X-Tenant", "b")));
        RuleData nonmatching = rule("r1", "s1", 1);
        nonmatching.setConditionDataList(List.of(condition("uri", "/", "/other")));
        subscriber.onRuleSubscribe(nonmatching);
        subscriber.onRuleSubscribe(rule("r2", "s2", 1));
        assertNull(execute("a"));
        assertEquals("s2:r2", execute("b"));
    }

    @Test
    public void dynamicRuleMissMustNotPoisonAnotherRequest() {
        subscriber.onSelectorSubscribe(selector("s1", 1, condition("uri", "/", "/audit")));
        RuleData dynamic = rule("r1", "s1", 1);
        dynamic.setConditionDataList(List.of(condition("header", "X-Tenant", "a")));
        subscriber.onRuleSubscribe(dynamic);
        assertNull(execute("b"));
        assertEquals("s1:r1", execute("a"));
    }

    @Test
    public void stableUriMatchesAreReused() {
        ConditionData selectorCondition = spy(condition("uri", "/", "/audit"));
        ConditionData ruleCondition = spy(condition("uri", "/", "/audit"));
        subscriber.onSelectorSubscribe(selector("s1", 1, selectorCondition));
        RuleData cached = rule("r1", "s1", 1);
        cached.setConditionDataList(List.of(ruleCondition));
        subscriber.onRuleSubscribe(cached);
        assertEquals("s1:r1", execute("a"));
        assertEquals("s1:r1", execute("b"));
        verify(selectorCondition, times(1)).getParamValue();
        verify(ruleCondition, times(1)).getParamValue();
    }

    @Test
    public void oldRequestCannotRefillCurrentGenerationAfterDeletion() {
        subscriber.onSelectorSubscribe(selector("s1", 1, condition("uri", "/", "/audit")));
        ConditionData delayed = spy(condition("uri", "/", "/audit"));
        RuleData removed = rule("r1", "s1", 1);
        removed.setConditionDataList(List.of(delayed));
        subscriber.onRuleSubscribe(removed);
        doAnswer(invocation -> {
            subscriber.unRuleSubscribe(removed);
            subscriber.onRuleSubscribe(rule("r2", "s1", 1));
            return "/audit";
        }).when(delayed).getParamValue();
        // The first request already holds an immutable old rules list when deletion occurs.
        assertEquals("s1:r1", execute("a"));
        assertEquals("s1:r2", execute("a"));
    }

    @Test
    public void rewrittenPathsUseDistinctMatchKeys() {
        subscriber.onSelectorSubscribe(selector("s1", 1, condition("uri", "/", "/a/audit")));
        subscriber.onSelectorSubscribe(selector("s2", 2, condition("uri", "/", "/b/audit")));
        RuleData first = rule("r1", "s1", 1);
        first.setConditionDataList(List.of(condition("uri", "/", "/a/audit")));
        RuleData second = rule("r2", "s2", 1);
        second.setConditionDataList(List.of(condition("uri", "/", "/b/audit")));
        subscriber.onRuleSubscribe(first);
        subscriber.onRuleSubscribe(second);
        assertEquals("s1:r1", executeRewritten("/a"));
        assertEquals("s2:r2", executeRewritten("/b"));
    }

    @Test
    public void deletingPluginCascadesToSelectorsAndRules() {
        subscriber.onSelectorSubscribe(selector("s1", 1, condition("uri", "/", "/audit")));
        subscriber.onRuleSubscribe(rule("r1", "s1", 1));
        assertEquals("s1:r1", execute("a"));
        subscriber.unSubscribe(PluginData.builder().name("audit").build());
        assertNull(BaseDataCache.getInstance().obtainSelectorData("audit"));
        assertNull(BaseDataCache.getInstance().obtainRuleData("s1"));
        subscriber.onSubscribe(PluginData.builder().name("audit").enabled(true).build());
        assertNull(execute("a"));
    }

    @Test
    public void deletingSelectorCascadesToRules() {
        SelectorData removed = selector("s1", 1, condition("uri", "/", "/audit"));
        subscriber.onSelectorSubscribe(removed);
        subscriber.onRuleSubscribe(rule("r1", "s1", 1));
        assertEquals("s1:r1", execute("a"));
        subscriber.unSelectorSubscribe(removed);
        assertNull(BaseDataCache.getInstance().obtainRuleData("s1"));
        subscriber.onSelectorSubscribe(removed);
        assertNull(execute("a"));
    }

    @Test
    public void cascadeDeletionInvokesChildHandlers() {
        PluginDataHandler handler = mock(PluginDataHandler.class);
        when(handler.pluginNamed()).thenReturn("audit");
        subscriber = new CommonPluginDataSubscriber(List.of(handler), mock(ApplicationEventPublisher.class),
                config.getSelectorMatchCache(), config.getRuleMatchCache());
        SelectorData removedSelector = selector("s1", 1, condition("uri", "/", "/audit"));
        RuleData removedRule = rule("r1", "s1", 1);
        subscriber.onSelectorSubscribe(removedSelector);
        subscriber.onRuleSubscribe(removedRule);
        subscriber.unSubscribe(PluginData.builder().name("audit").build());
        verify(handler).removeSelector(removedSelector);
        verify(handler).removeRule(removedRule);
    }

    @Test
    public void batchRefreshInvalidatesPreviouslyCachedWinner() {
        subscriber.onSelectorSubscribe(selector("s1", 1, condition("uri", "/", "/audit")));
        subscriber.onRuleSubscribe(rule("r1", "s1", 10));
        assertEquals("s1:r1", execute("a"));
        subscriber.onRuleRefresh(List.of(rule("r2", "s1", 1)));
        assertEquals("s1:r2", execute("a"));
    }

    @Test
    public void enablingDynamicCandidateInvalidatesExistingMatch() {
        subscriber.onSelectorSubscribe(selector("s1", 10, condition("uri", "/", "/audit")));
        SelectorData disabled = selector("s2", 1, condition("header", "X-Tenant", "a"));
        disabled.setEnabled(false);
        subscriber.onSelectorSubscribe(disabled);
        subscriber.onRuleSubscribe(rule("r1", "s1", 1));
        subscriber.onRuleSubscribe(rule("r2", "s2", 1));
        assertEquals("s1:r1", execute("a"));
        subscriber.onSelectorSubscribe(selector("s2", 1, condition("header", "X-Tenant", "a")));
        assertEquals("s2:r2", execute("a"));
    }

    private String executeRewritten(final String contextPath) {
        plugin.result = null;
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/audit"));
        ShenyuContext context = new ShenyuContext();
        context.setRealUrl("/audit");
        exchange.getAttributes().put(Constants.CONTEXT, context);
        exchange.getAttributes().put(Constants.CONTEXT_PATH, contextPath);
        plugin.execute(exchange, e -> Mono.empty()).block();
        return plugin.result;
    }

    private static class Probe extends AbstractShenyuPlugin {
        private String result;

        @Override
        public String named() {
            return "audit";
        }

        @Override
        public int getOrder() {
            return 0;
        }

        @Override
        protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
            result = selector.getId() + ":" + rule.getId();
            return Mono.empty();
        }
    }
}
