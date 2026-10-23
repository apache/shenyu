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

package org.apache.shenyu.plugin.base;

import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.base.cache.MatchDataCache;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.clearInvocations;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * The Abstract shenyu plugin test.
 */
public final class AbstractShenyuPluginTest {

    private RuleData ruleData;

    private PluginData pluginData;

    private SelectorData selectorData;

    private ConditionData conditionData;

    private ServerWebExchange exchange;

    private TestShenyuPlugin testShenyuPlugin;

    private ShenyuPluginChain shenyuPluginChain;

    @BeforeEach
    public void setUp() {
        mockShenyuConfig();
        this.ruleData = RuleData.builder()
                .id("1")
                .pluginName("SHENYU")
                .selectorId("1")
                .enabled(true)
                .logged(true)
                .matchRestful(false)
                .sort(1).build();
        this.conditionData = new ConditionData();
        this.conditionData.setOperator("match");
        this.conditionData.setParamName("/");
        this.conditionData.setParamType("uri");
        this.conditionData.setParamValue("/http/**");
        this.shenyuPluginChain = mock(ShenyuPluginChain.class);
        this.pluginData = PluginData.builder()
                .name("SHENYU")
                .enabled(true).build();
        this.selectorData = SelectorData.builder()
                .id("1").pluginName("SHENYU")
                .enabled(true)
                .matchRestful(false)
                .type(SelectorTypeEnum.CUSTOM_FLOW.getCode()).build();
        this.testShenyuPlugin = spy(new TestShenyuPlugin());
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/http/SHENYU/SHENYU")
                .build());
        ShenyuContext context = mock(ShenyuContext.class);
        exchange.getAttributes().put(Constants.CONTEXT, context);
        clearCache();
        when(shenyuPluginChain.execute(exchange)).thenReturn(Mono.empty());
    }

    /**
     * The plugin is null test.
     */
    @Test
    public void printLogToleratesNullLoggedFlag() throws Exception {
        java.lang.reflect.Method printLog = AbstractShenyuPlugin.class
                .getDeclaredMethod("printLog", RuleData.class, String.class);
        printLog.setAccessible(true);
        RuleData nullLogged = RuleData.builder().id("id").name("rule").pluginName("plugin").build();
        printLog.invoke(testShenyuPlugin, nullLogged, "plugin");
    }

    @Test
    public void executePluginIsNullTest() {
        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).expectSubscription().verifyComplete();
        verify(shenyuPluginChain).execute(exchange);
    }

    /**
     * The selector is null test.
     */
    @Test
    public void executeSelectorIsNullTest() {
        BaseDataCache.getInstance().cachePluginData(pluginData);
        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).expectSubscription().verifyComplete();
        verify(shenyuPluginChain).execute(exchange);
    }

    /**
     * The selector data is null test.
     */
    @Test
    public void executeSelectorDataIsNullTest() {
        BaseDataCache.getInstance().cachePluginData(pluginData);
        BaseDataCache.getInstance().cacheSelectData(selectorData);
        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).expectSubscription().verifyComplete();
        verify(shenyuPluginChain).execute(exchange);
    }

    /**
     * The rule is null test.
     */
    @Test
    public void executeRuleIsNullTest() {
        List<ConditionData> conditionDataList = Collections.singletonList(conditionData);
        this.selectorData.setMatchMode(0);
        this.selectorData.setLogged(true);
        this.selectorData.setMatchRestful(false);
        this.selectorData.setConditionList(conditionDataList);
        BaseDataCache.getInstance().cachePluginData(pluginData);
        BaseDataCache.getInstance().cacheSelectData(selectorData);
        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).expectSubscription().verifyComplete();
        verify(shenyuPluginChain).execute(exchange);
    }

    /**
     * The rule is not null test.
     */
    @Test
    public void executeRuleIsNotNullTest() {
        List<ConditionData> conditionDataList = Collections.singletonList(conditionData);
        this.ruleData.setConditionDataList(conditionDataList);
        this.ruleData.setMatchMode(0);
        this.ruleData.setMatchRestful(false);
        this.selectorData.setMatchMode(0);
        this.selectorData.setMatchRestful(false);
        this.selectorData.setLogged(true);
        this.selectorData.setConditionList(conditionDataList);
        BaseDataCache.getInstance().cachePluginData(pluginData);
        BaseDataCache.getInstance().cacheSelectData(selectorData);
        BaseDataCache.getInstance().cacheRuleData(ruleData);
        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).expectSubscription().verifyComplete();
        verify(testShenyuPlugin).doExecute(exchange, shenyuPluginChain, selectorData, ruleData);
    }

    @Test
    public void executeSelectorManyMatch() {
        List<ConditionData> conditionDataList = Collections.singletonList(conditionData);
        this.ruleData.setConditionDataList(conditionDataList);
        this.ruleData.setMatchMode(0);
        this.selectorData.setSort(1);
        this.selectorData.setMatchMode(0);
        this.selectorData.setLogged(true);
        this.selectorData.setConditionList(conditionDataList);
        BaseDataCache.getInstance().cachePluginData(pluginData);
        BaseDataCache.getInstance().cacheSelectData(selectorData);
        BaseDataCache.getInstance().cacheSelectData(SelectorData.builder()
                .id("2").pluginName("SHENYU")
                .enabled(true)
                .matchMode(0)
                .logged(true)
                .sort(2)
                .conditionList(conditionDataList)
                .type(SelectorTypeEnum.CUSTOM_FLOW.getCode()).build());
        BaseDataCache.getInstance().cacheRuleData(ruleData);
        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).expectSubscription().verifyComplete();
        verify(testShenyuPlugin).doExecute(exchange, shenyuPluginChain, selectorData, ruleData);
    }

    @Test
    public void executeRuleManyMatch() {
        List<ConditionData> conditionDataList = Collections.singletonList(conditionData);
        this.ruleData.setConditionDataList(conditionDataList);
        this.ruleData.setMatchMode(0);
        this.ruleData.setMatchRestful(false);
        this.selectorData.setMatchMode(0);
        this.selectorData.setLogged(true);
        this.selectorData.setConditionList(conditionDataList);
        BaseDataCache.getInstance().cachePluginData(pluginData);
        BaseDataCache.getInstance().cacheSelectData(selectorData);

        BaseDataCache.getInstance().cacheRuleData(RuleData.builder()
                .id("1")
                .pluginName("SHENYU")
                .selectorId("1")
                .enabled(true)
                .logged(true)
                .matchMode(0)
                .matchRestful(false)
                .conditionDataList(Collections.singletonList(conditionData))
                .sort(1).build());

        BaseDataCache.getInstance().cacheRuleData(RuleData.builder()
                .id("2")
                .pluginName("SHENYU")
                .selectorId("1")
                .enabled(true)
                .logged(true)
                .matchMode(0)
                .matchRestful(false)
                .conditionDataList(Collections.singletonList(conditionData))
                .sort(2).build());
        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).expectSubscription().verifyComplete();
        verify(testShenyuPlugin).doExecute(exchange, shenyuPluginChain, selectorData, ruleData);
    }

    /**
     * The rule is full test.
     */
    @Test
    public void executeRuleFullTest() {
        List<ConditionData> conditionDataList = Collections.singletonList(conditionData);
        this.ruleData.setConditionDataList(conditionDataList);
        this.ruleData.setMatchMode(1);
        this.ruleData.setMatchRestful(false);
        this.selectorData.setMatchMode(0);
        this.selectorData.setMatchRestful(false);
        this.selectorData.setType(SelectorTypeEnum.FULL_FLOW.getCode());
        this.selectorData.setLogged(true);
        this.selectorData.setConditionList(conditionDataList);
        BaseDataCache.getInstance().cachePluginData(pluginData);
        BaseDataCache.getInstance().cacheSelectData(selectorData);
        BaseDataCache.getInstance().cacheRuleData(ruleData);
        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).expectSubscription().verifyComplete();
        verify(testShenyuPlugin).doExecute(exchange, shenyuPluginChain, selectorData, ruleData);
    }

    @Test
    public void executeRuleFullWithMultipleEnabledRulesTest() {
        this.selectorData.setType(SelectorTypeEnum.FULL_FLOW.getCode());
        this.selectorData.setLogged(true);
        RuleData secondRule = RuleData.builder()
                .id("2")
                .pluginName("SHENYU")
                .selectorId("1")
                .enabled(true)
                .logged(true)
                .sort(2).build();
        BaseDataCache.getInstance().cachePluginData(pluginData);
        BaseDataCache.getInstance().cacheSelectData(selectorData);
        BaseDataCache.getInstance().cacheRuleData(ruleData);
        BaseDataCache.getInstance().cacheRuleData(secondRule);
        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).expectSubscription().verifyComplete();
        verify(testShenyuPlugin).doExecute(exchange, shenyuPluginChain, selectorData, secondRule);
        verify(testShenyuPlugin, never()).doExecute(exchange, shenyuPluginChain, selectorData, ruleData);
    }

    @Test
    public void executeRuleFullWithLastRuleDisabledTest() {
        this.selectorData.setType(SelectorTypeEnum.FULL_FLOW.getCode());
        this.selectorData.setLogged(true);
        RuleData disabledRule = RuleData.builder()
                .id("2")
                .pluginName("SHENYU")
                .selectorId("1")
                .enabled(false)
                .logged(true)
                .sort(2).build();
        BaseDataCache.getInstance().cachePluginData(pluginData);
        BaseDataCache.getInstance().cacheSelectData(selectorData);
        BaseDataCache.getInstance().cacheRuleData(ruleData);
        BaseDataCache.getInstance().cacheRuleData(disabledRule);
        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).expectSubscription().verifyComplete();
        verify(testShenyuPlugin).doExecute(exchange, shenyuPluginChain, selectorData, ruleData);
    }

    @Test
    public void executeRuleFullWithAllRulesDisabledTest() {
        this.selectorData.setType(SelectorTypeEnum.FULL_FLOW.getCode());
        this.selectorData.setLogged(true);
        this.ruleData.setEnabled(false);
        BaseDataCache.getInstance().cachePluginData(pluginData);
        BaseDataCache.getInstance().cacheSelectData(selectorData);
        BaseDataCache.getInstance().cacheRuleData(ruleData);
        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).expectSubscription().verifyComplete();
        verify(shenyuPluginChain).execute(exchange);
        verify(testShenyuPlugin, never()).doExecute(any(), any(), any(), any());
    }

    @Test
    public void defaultStrategySelectorMatchShouldNotLogWhenLoggingDisabled() {
        List<ConditionData> conditionDataList = Collections.singletonList(conditionData);
        this.ruleData.setConditionDataList(conditionDataList);
        this.ruleData.setMatchMode(0);
        this.selectorData.setSort(1);
        this.selectorData.setMatchMode(0);
        this.selectorData.setLogged(false);
        this.selectorData.setConditionList(conditionDataList);
        BaseDataCache.getInstance().cachePluginData(pluginData);
        BaseDataCache.getInstance().cacheSelectData(selectorData);
        BaseDataCache.getInstance().cacheRuleData(ruleData);
        try (LogCapture capture = LogCapture.attach(AbstractShenyuPlugin.class)) {
            StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).expectSubscription().verifyComplete();
            assertTrue(capture.infoMessages().stream().noneMatch(message -> message.contains("selector")),
                    "no selector match line may reach INFO when the selector's logged flag is false, got: " + capture.infoMessages());
        }
    }

    @Test
    public void defaultStrategySelectorMatchShouldLogExactlyOnceWhenLoggingEnabled() {
        List<ConditionData> conditionDataList = Collections.singletonList(conditionData);
        this.ruleData.setConditionDataList(conditionDataList);
        this.ruleData.setMatchMode(0);
        this.selectorData.setSort(1);
        this.selectorData.setMatchMode(0);
        this.selectorData.setLogged(true);
        this.selectorData.setConditionList(conditionDataList);
        BaseDataCache.getInstance().cachePluginData(pluginData);
        BaseDataCache.getInstance().cacheSelectData(selectorData);
        BaseDataCache.getInstance().cacheRuleData(ruleData);
        try (LogCapture capture = LogCapture.attach(AbstractShenyuPlugin.class)) {
            StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).expectSubscription().verifyComplete();
            List<String> selectorLines = capture.infoMessages().stream()
                    .filter(message -> message.contains("selector"))
                    .collect(Collectors.toList());
            assertEquals(1, selectorLines.size(),
                    "exactly one selector match line is expected at INFO when logged is true, got: " + selectorLines);
            assertTrue(selectorLines.get(0).contains("selector success match"),
                    "the surviving line should be the one that carries the selector name, got: " + selectorLines.get(0));
        }
    }

    @Test
    public void defaultStrategyRuleMatchShouldNotLogWhenLoggingDisabled() {
        List<ConditionData> conditionDataList = Collections.singletonList(conditionData);
        this.ruleData.setConditionDataList(conditionDataList);
        this.ruleData.setMatchMode(0);
        this.ruleData.setLogged(false);
        this.selectorData.setSort(1);
        this.selectorData.setMatchMode(0);
        this.selectorData.setLogged(false);
        this.selectorData.setConditionList(conditionDataList);
        BaseDataCache.getInstance().cachePluginData(pluginData);
        BaseDataCache.getInstance().cacheSelectData(selectorData);
        BaseDataCache.getInstance().cacheRuleData(ruleData);
        try (LogCapture capture = LogCapture.attach(AbstractShenyuPlugin.class)) {
            StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).expectSubscription().verifyComplete();
            assertTrue(capture.infoMessages().stream().noneMatch(message -> message.contains("rule")),
                    "no rule match line may reach INFO when the rule's logged flag is false, got: " + capture.infoMessages());
        }
    }

    @Test
    public void defaultStrategyRuleMatchShouldLogExactlyOnceWhenLoggingEnabled() {
        List<ConditionData> conditionDataList = Collections.singletonList(conditionData);
        this.ruleData.setConditionDataList(conditionDataList);
        this.ruleData.setMatchMode(0);
        this.ruleData.setLogged(true);
        this.selectorData.setSort(1);
        this.selectorData.setMatchMode(0);
        this.selectorData.setLogged(true);
        this.selectorData.setConditionList(conditionDataList);
        BaseDataCache.getInstance().cachePluginData(pluginData);
        BaseDataCache.getInstance().cacheSelectData(selectorData);
        BaseDataCache.getInstance().cacheRuleData(ruleData);
        try (LogCapture capture = LogCapture.attach(AbstractShenyuPlugin.class)) {
            StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).expectSubscription().verifyComplete();
            List<String> ruleLines = capture.infoMessages().stream()
                    .filter(message -> message.contains("rule"))
                    .collect(Collectors.toList());
            assertEquals(1, ruleLines.size(),
                    "exactly one rule match line is expected at INFO when logged is true, got: " + ruleLines);
            assertTrue(ruleLines.get(0).contains("rule success match"),
                    "the surviving line should be the one that carries the rule name, got: " + ruleLines.get(0));
        }
    }

    @Test
    public void defaultStrategyMatchLogsShouldNotChangeRoutingResults() {
        List<ConditionData> conditionDataList = Collections.singletonList(conditionData);
        this.ruleData.setConditionDataList(conditionDataList);
        this.ruleData.setMatchMode(0);
        this.selectorData.setSort(1);
        this.selectorData.setMatchMode(0);
        this.selectorData.setLogged(true);
        this.selectorData.setConditionList(conditionDataList);
        BaseDataCache.getInstance().cachePluginData(pluginData);
        BaseDataCache.getInstance().cacheSelectData(selectorData);
        BaseDataCache.getInstance().cacheRuleData(ruleData);
        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).expectSubscription().verifyComplete();
        verify(testShenyuPlugin).doExecute(exchange, shenyuPluginChain, selectorData, ruleData);
    }

    @Test
    public void negativeSelectorCacheMustNotSuppressDynamicMatch() {
        SelectorData dynamicSelector = SelectorData.builder()
                .id("1").pluginName("SHENYU").enabled(true).logged(true)
                .matchRestful(false).matchMode(0).sort(1).continued(false)
                .type(SelectorTypeEnum.CUSTOM_FLOW.getCode())
                .conditionList(Collections.singletonList(condition(ParamTypeEnum.HEADER.getName(), "X-Tenant", "=", "a")))
                .build();
        BaseDataCache.getInstance().cachePluginData(pluginData);
        BaseDataCache.getInstance().cacheSelectData(dynamicSelector);
        when(shenyuPluginChain.execute(any(ServerWebExchange.class))).thenReturn(Mono.empty());

        ServerWebExchange miss = newExchange("/neg-selector", null);
        StepVerifier.create(testShenyuPlugin.execute(miss, shenyuPluginChain)).expectSubscription().verifyComplete();
        verify(shenyuPluginChain).execute(miss);
        assertNull(MatchDataCache.getInstance().obtainSelectorData("SHENYU", "/neg-selector"),
                "a miss caused by a missing header must not be cached across dynamic candidates");

        ServerWebExchange hit = newExchange("/neg-selector", "a");
        StepVerifier.create(testShenyuPlugin.execute(hit, shenyuPluginChain)).expectSubscription().verifyComplete();
        verify(testShenyuPlugin).doExecute(eq(hit), eq(shenyuPluginChain), eq(dynamicSelector), any(RuleData.class));
    }

    @Test
    public void negativeRuleCacheMustNotSuppressDynamicMatch() {
        SelectorData pathSelector = SelectorData.builder()
                .id("1").pluginName("SHENYU").enabled(true).logged(true)
                .matchRestful(false).matchMode(0).sort(1).continued(true)
                .type(SelectorTypeEnum.CUSTOM_FLOW.getCode())
                .conditionList(Collections.singletonList(condition(ParamTypeEnum.URI.getName(), "/", "=", "/neg-rule")))
                .build();
        RuleData dynamicRule = RuleData.builder()
                .id("1").pluginName("SHENYU").selectorId("1").enabled(true).logged(true)
                .matchRestful(false).matchMode(0).sort(1)
                .conditionDataList(Collections.singletonList(condition(ParamTypeEnum.HEADER.getName(), "X-Tenant", "=", "a")))
                .build();
        BaseDataCache.getInstance().cachePluginData(pluginData);
        BaseDataCache.getInstance().cacheSelectData(pathSelector);
        BaseDataCache.getInstance().cacheRuleData(dynamicRule);
        when(shenyuPluginChain.execute(any(ServerWebExchange.class))).thenReturn(Mono.empty());

        ServerWebExchange miss = newExchange("/neg-rule", null);
        StepVerifier.create(testShenyuPlugin.execute(miss, shenyuPluginChain)).expectSubscription().verifyComplete();
        verify(shenyuPluginChain).execute(miss);
        assertNull(MatchDataCache.getInstance().obtainRuleData("SHENYU", "/neg-rule"),
                "a rule miss caused by a missing header must not be cached across dynamic candidates");

        ServerWebExchange hit = newExchange("/neg-rule", "a");
        StepVerifier.create(testShenyuPlugin.execute(hit, shenyuPluginChain)).expectSubscription().verifyComplete();
        verify(testShenyuPlugin).doExecute(eq(hit), eq(shenyuPluginChain), eq(pathSelector), eq(dynamicRule));
    }

    @Test
    public void negativeRuleCacheMustRequirePathDeterminedSelectors() {
        SelectorData firstSelector = SelectorData.builder()
                .id("1").pluginName("SHENYU").enabled(true).logged(true)
                .matchRestful(false).matchMode(0).sort(1).continued(true)
                .type(SelectorTypeEnum.CUSTOM_FLOW.getCode())
                .conditionList(Collections.singletonList(condition(ParamTypeEnum.HEADER.getName(), "X-Tenant", "=", "a")))
                .build();
        SelectorData secondSelector = SelectorData.builder()
                .id("2").pluginName("SHENYU").enabled(true).logged(true)
                .matchRestful(false).matchMode(0).sort(2).continued(true)
                .type(SelectorTypeEnum.CUSTOM_FLOW.getCode())
                .conditionList(Collections.singletonList(condition(ParamTypeEnum.HEADER.getName(), "X-Tenant", "=", "b")))
                .build();
        RuleData pathOnlyRule = RuleData.builder()
                .id("1").pluginName("SHENYU").selectorId("1").enabled(true).logged(true)
                .matchRestful(false).matchMode(0).sort(1)
                .conditionDataList(Collections.singletonList(condition(ParamTypeEnum.URI.getName(), "/", "match", "/elsewhere/**")))
                .build();
        BaseDataCache.getInstance().cachePluginData(pluginData);
        BaseDataCache.getInstance().cacheSelectData(firstSelector);
        BaseDataCache.getInstance().cacheSelectData(secondSelector);
        BaseDataCache.getInstance().cacheRuleData(pathOnlyRule);
        when(shenyuPluginChain.execute(any(ServerWebExchange.class))).thenReturn(Mono.empty());

        ServerWebExchange miss = newExchange("/neg-cross", "a");
        StepVerifier.create(testShenyuPlugin.execute(miss, shenyuPluginChain)).expectSubscription().verifyComplete();
        verify(shenyuPluginChain).execute(miss);
        assertNull(MatchDataCache.getInstance().obtainRuleData("SHENYU", "/neg-cross"),
                "a rule miss is keyed by path only, so it must not be cached while selector matching is dynamic");
    }

    @Test
    public void negativeSelectorCacheStillAppliesToPathOnlyCandidates() {
        SelectorData pathOnlySelector = spy(SelectorData.builder()
                .id("1").pluginName("SHENYU").enabled(true).logged(true)
                .matchRestful(false).matchMode(0).sort(1).continued(false)
                .type(SelectorTypeEnum.CUSTOM_FLOW.getCode())
                .conditionList(Collections.singletonList(condition(ParamTypeEnum.URI.getName(), "/", "match", "/http/**")))
                .build());
        BaseDataCache.getInstance().cachePluginData(pluginData);
        BaseDataCache.getInstance().cacheSelectData(pathOnlySelector);
        when(shenyuPluginChain.execute(any(ServerWebExchange.class))).thenReturn(Mono.empty());

        ServerWebExchange miss = newExchange("/neg-control", null);
        StepVerifier.create(testShenyuPlugin.execute(miss, shenyuPluginChain)).expectSubscription().verifyComplete();

        SelectorData cached = MatchDataCache.getInstance().obtainSelectorData("SHENYU", "/neg-control");
        assertNotNull(cached, "a path-only candidate set may still cache a negative selector result");
        assertNull(cached.getId());
        clearInvocations(pathOnlySelector);

        StepVerifier.create(testShenyuPlugin.execute(miss, shenyuPluginChain)).expectSubscription().verifyComplete();
        verify(pathOnlySelector, never()).getConditionList();
        assertSame(cached, MatchDataCache.getInstance().obtainSelectorData("SHENYU", "/neg-control"));
    }

    @Test
    public void negativeRuleCacheStillAppliesToPathOnlyCandidates() {
        SelectorData pathOnlySelector = SelectorData.builder()
                .id("1").pluginName("SHENYU").enabled(true).logged(true)
                .matchRestful(false).matchMode(0).sort(1).continued(true)
                .type(SelectorTypeEnum.CUSTOM_FLOW.getCode())
                .conditionList(Collections.singletonList(condition(ParamTypeEnum.URI.getName(), "/", "match", "/http/**")))
                .build();
        RuleData pathOnlyRule = spy(RuleData.builder()
                .id("1").pluginName("SHENYU").selectorId("1").enabled(true).logged(true)
                .matchRestful(false).matchMode(0).sort(1)
                .conditionDataList(Collections.singletonList(condition(ParamTypeEnum.URI.getName(), "/", "match", "/elsewhere/**")))
                .build());
        BaseDataCache.getInstance().cachePluginData(pluginData);
        BaseDataCache.getInstance().cacheSelectData(pathOnlySelector);
        BaseDataCache.getInstance().cacheRuleData(pathOnlyRule);
        when(shenyuPluginChain.execute(any(ServerWebExchange.class))).thenReturn(Mono.empty());

        ServerWebExchange miss = newExchange("/http/neg-control", null);
        StepVerifier.create(testShenyuPlugin.execute(miss, shenyuPluginChain)).expectSubscription().verifyComplete();

        RuleData cached = MatchDataCache.getInstance().obtainRuleData("SHENYU", "/http/neg-control");
        assertNotNull(cached, "a path-only candidate set may still cache a negative rule result");
        assertNull(cached.getId());
        clearInvocations(pathOnlyRule);

        StepVerifier.create(testShenyuPlugin.execute(miss, shenyuPluginChain)).expectSubscription().verifyComplete();
        verify(pathOnlyRule, never()).getConditionDataList();
        assertSame(cached, MatchDataCache.getInstance().obtainRuleData("SHENYU", "/http/neg-control"));
    }

    /**
     * #7981: a positive selector match keyed by (plugin, path) must not shadow a
     * more specific dynamic candidate that lost the warming request.
     */
    @Test
    public void positiveSelectorCacheMustNotShadowDynamicMatch() {
        SelectorData generic = SelectorData.builder()
                .id("generic").pluginName("SHENYU").enabled(true).logged(true)
                .matchRestful(false).matchMode(0).sort(2).continued(false)
                .type(SelectorTypeEnum.CUSTOM_FLOW.getCode())
                .conditionList(Collections.singletonList(condition(ParamTypeEnum.URI.getName(), "/", "match", "/shadow/**")))
                .build();
        SelectorData tenant = SelectorData.builder()
                .id("tenant").pluginName("SHENYU").enabled(true).logged(true)
                .matchRestful(false).matchMode(0).sort(1).continued(false)
                .type(SelectorTypeEnum.CUSTOM_FLOW.getCode())
                .conditionList(Arrays.asList(
                        condition(ParamTypeEnum.URI.getName(), "/", "match", "/shadow/**"),
                        condition(ParamTypeEnum.HEADER.getName(), "X-Tenant", "=", "a")))
                .build();
        BaseDataCache.getInstance().cachePluginData(pluginData);
        BaseDataCache.getInstance().cacheSelectData(generic);
        BaseDataCache.getInstance().cacheSelectData(tenant);
        when(shenyuPluginChain.execute(any(ServerWebExchange.class))).thenReturn(Mono.empty());

        ServerWebExchange warm = newExchange("/shadow/x", "b");
        StepVerifier.create(testShenyuPlugin.execute(warm, shenyuPluginChain)).expectSubscription().verifyComplete();
        verify(testShenyuPlugin).doExecute(eq(warm), eq(shenyuPluginChain), eq(generic), any(RuleData.class));
        assertNull(MatchDataCache.getInstance().obtainSelectorData("SHENYU", "/shadow/x"),
                "a positive match must not be cached while a dynamic candidate could win another request");

        ServerWebExchange vip = newExchange("/shadow/x", "a");
        StepVerifier.create(testShenyuPlugin.execute(vip, shenyuPluginChain)).expectSubscription().verifyComplete();
        verify(testShenyuPlugin).doExecute(eq(vip), eq(shenyuPluginChain), eq(tenant), any(RuleData.class));
    }

    /**
     * #7981 for rules: the rule cache is keyed by (plugin, path) without the
     * selector id, so a dynamic rule candidate must not be shadowed either.
     */
    @Test
    public void positiveRuleCacheMustNotShadowDynamicMatch() {
        SelectorData pathSelector = SelectorData.builder()
                .id("1").pluginName("SHENYU").enabled(true).logged(true)
                .matchRestful(false).matchMode(0).sort(1).continued(true)
                .type(SelectorTypeEnum.CUSTOM_FLOW.getCode())
                .conditionList(Collections.singletonList(condition(ParamTypeEnum.URI.getName(), "/", "match", "/shadow-rule/**")))
                .build();
        RuleData generic = RuleData.builder()
                .id("generic").pluginName("SHENYU").selectorId("1").enabled(true).logged(true)
                .matchRestful(false).matchMode(0).sort(2)
                .conditionDataList(Collections.singletonList(condition(ParamTypeEnum.URI.getName(), "/", "match", "/shadow-rule/**")))
                .build();
        RuleData tenant = RuleData.builder()
                .id("tenant").pluginName("SHENYU").selectorId("1").enabled(true).logged(true)
                .matchRestful(false).matchMode(0).sort(1)
                .conditionDataList(Arrays.asList(
                        condition(ParamTypeEnum.URI.getName(), "/", "match", "/shadow-rule/**"),
                        condition(ParamTypeEnum.HEADER.getName(), "X-Tenant", "=", "a")))
                .build();
        BaseDataCache.getInstance().cachePluginData(pluginData);
        BaseDataCache.getInstance().cacheSelectData(pathSelector);
        BaseDataCache.getInstance().cacheRuleData(generic);
        BaseDataCache.getInstance().cacheRuleData(tenant);
        when(shenyuPluginChain.execute(any(ServerWebExchange.class))).thenReturn(Mono.empty());

        ServerWebExchange warm = newExchange("/shadow-rule/x", "b");
        StepVerifier.create(testShenyuPlugin.execute(warm, shenyuPluginChain)).expectSubscription().verifyComplete();
        verify(testShenyuPlugin).doExecute(eq(warm), eq(shenyuPluginChain), eq(pathSelector), eq(generic));
        assertNull(MatchDataCache.getInstance().obtainRuleData("SHENYU", "/shadow-rule/x"),
                "a positive rule match must not be cached while a dynamic candidate could win another request");

        ServerWebExchange vip = newExchange("/shadow-rule/x", "a");
        StepVerifier.create(testShenyuPlugin.execute(vip, shenyuPluginChain)).expectSubscription().verifyComplete();
        verify(testShenyuPlugin).doExecute(eq(vip), eq(shenyuPluginChain), eq(pathSelector), eq(tenant));
    }

    /**
     * A path-only candidate set keeps caching positive results: the gate must not
     * turn the match cache off for the configurations it was built for.
     */
    @Test
    public void positiveSelectorCacheStillAppliesToPathOnlyCandidates() {
        SelectorData pathOnly = spy(SelectorData.builder()
                .id("1").pluginName("SHENYU").enabled(true).logged(true)
                .matchRestful(false).matchMode(0).sort(1).continued(false)
                .type(SelectorTypeEnum.CUSTOM_FLOW.getCode())
                .conditionList(Collections.singletonList(condition(ParamTypeEnum.URI.getName(), "/", "match", "/http/**")))
                .build());
        BaseDataCache.getInstance().cachePluginData(pluginData);
        BaseDataCache.getInstance().cacheSelectData(pathOnly);
        when(shenyuPluginChain.execute(any(ServerWebExchange.class))).thenReturn(Mono.empty());

        ServerWebExchange exchange = newExchange("/http/ctrl", null);
        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).expectSubscription().verifyComplete();

        SelectorData cached = MatchDataCache.getInstance().obtainSelectorData("SHENYU", "/http/ctrl");
        assertNotNull(cached, "a path-only candidate set may still cache a positive selector result");
        assertEquals("1", cached.getId());
        clearInvocations(pathOnly);

        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).expectSubscription().verifyComplete();
        verify(pathOnly, never()).getConditionList();
        assertSame(cached, MatchDataCache.getInstance().obtainSelectorData("SHENYU", "/http/ctrl"));
    }

    /**
     * Path-only rules keep caching positive results too.
     */
    @Test
    public void positiveRuleCacheStillAppliesToPathOnlyCandidates() {
        SelectorData pathOnlySelector = SelectorData.builder()
                .id("1").pluginName("SHENYU").enabled(true).logged(true)
                .matchRestful(false).matchMode(0).sort(1).continued(true)
                .type(SelectorTypeEnum.CUSTOM_FLOW.getCode())
                .conditionList(Collections.singletonList(condition(ParamTypeEnum.URI.getName(), "/", "match", "/http/**")))
                .build();
        RuleData pathOnlyRule = spy(RuleData.builder()
                .id("2").pluginName("SHENYU").selectorId("1").enabled(true).logged(true)
                .matchRestful(false).matchMode(0).sort(1)
                .conditionDataList(Collections.singletonList(condition(ParamTypeEnum.URI.getName(), "/", "match", "/http/**")))
                .build());
        BaseDataCache.getInstance().cachePluginData(pluginData);
        BaseDataCache.getInstance().cacheSelectData(pathOnlySelector);
        BaseDataCache.getInstance().cacheRuleData(pathOnlyRule);
        when(shenyuPluginChain.execute(any(ServerWebExchange.class))).thenReturn(Mono.empty());

        ServerWebExchange exchange = newExchange("/http/ctrl", null);
        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).expectSubscription().verifyComplete();

        RuleData cached = MatchDataCache.getInstance().obtainRuleData("SHENYU", "/http/ctrl");
        assertNotNull(cached, "a path-only candidate set may still cache a positive rule result");
        assertEquals("2", cached.getId());
        clearInvocations(pathOnlyRule);

        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).expectSubscription().verifyComplete();
        verify(pathOnlyRule, never()).getConditionDataList();
        assertSame(cached, MatchDataCache.getInstance().obtainRuleData("SHENYU", "/http/ctrl"));
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    public void restfulSelectorCandidateShouldPreventResultCaching(final boolean matched) {
        SelectorData candidate = SelectorData.builder()
                .id("candidate").pluginName("SHENYU").enabled(true).logged(true)
                .matchRestful(false).matchMode(0).sort(1).continued(false)
                .type(SelectorTypeEnum.CUSTOM_FLOW.getCode())
                .conditionList(Collections.singletonList(
                        condition(ParamTypeEnum.URI.getName(), "/", "match", matched ? "/http/**" : "/other/**")))
                .build();
        SelectorData restful = SelectorData.builder()
                .id("restful").pluginName("SHENYU").enabled(true).logged(true)
                .matchRestful(true).matchMode(0).sort(2).continued(false)
                .type(SelectorTypeEnum.CUSTOM_FLOW.getCode())
                .conditionList(Collections.singletonList(
                        condition(ParamTypeEnum.URI.getName(), "/", "match", "/restful/{id}")))
                .build();
        BaseDataCache.getInstance().cachePluginData(pluginData);
        BaseDataCache.getInstance().cacheSelectData(candidate);
        BaseDataCache.getInstance().cacheSelectData(restful);

        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).expectSubscription().verifyComplete();

        assertNull(MatchDataCache.getInstance().obtainSelectorData("SHENYU", exchange.getRequest().getURI().getRawPath()),
                "a RESTful selector makes the candidate set path-ambiguous, so no entry may be cached");
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    public void restfulRuleCandidateShouldPreventResultCaching(final boolean matched) {
        SelectorData selector = SelectorData.builder()
                .id("selector").pluginName("SHENYU").enabled(true).logged(true)
                .matchRestful(false).matchMode(0).sort(1).continued(true)
                .type(SelectorTypeEnum.CUSTOM_FLOW.getCode())
                .conditionList(Collections.singletonList(
                        condition(ParamTypeEnum.URI.getName(), "/", "match", "/http/**")))
                .build();
        RuleData candidate = RuleData.builder()
                .id("candidate").pluginName("SHENYU").selectorId("selector").enabled(true).logged(true)
                .matchRestful(false).matchMode(0).sort(1)
                .conditionDataList(Collections.singletonList(
                        condition(ParamTypeEnum.URI.getName(), "/", "match", matched ? "/http/**" : "/other/**")))
                .build();
        RuleData restful = RuleData.builder()
                .id("restful").pluginName("SHENYU").selectorId("selector").enabled(true).logged(true)
                .matchRestful(true).matchMode(0).sort(2)
                .conditionDataList(Collections.singletonList(
                        condition(ParamTypeEnum.URI.getName(), "/", "match", "/restful/{id}")))
                .build();
        BaseDataCache.getInstance().cachePluginData(pluginData);
        BaseDataCache.getInstance().cacheSelectData(selector);
        BaseDataCache.getInstance().cacheRuleData(candidate);
        BaseDataCache.getInstance().cacheRuleData(restful);

        StepVerifier.create(testShenyuPlugin.execute(exchange, shenyuPluginChain)).expectSubscription().verifyComplete();

        assertNull(MatchDataCache.getInstance().obtainRuleData("SHENYU", exchange.getRequest().getURI().getRawPath()),
                "a RESTful rule makes the candidate set path-ambiguous, so no entry may be cached");
    }

    private ConditionData condition(final String paramType, final String paramName, final String operator, final String paramValue) {
        ConditionData condition = new ConditionData();
        condition.setParamType(paramType);
        condition.setParamName(paramName);
        condition.setOperator(operator);
        condition.setParamValue(paramValue);
        return condition;
    }

    private ServerWebExchange newExchange(final String path, final String tenant) {
        MockServerHttpRequest request = Objects.isNull(tenant)
                ? MockServerHttpRequest.get(path).build()
                : MockServerHttpRequest.get(path).header("X-Tenant", tenant).build();
        return MockServerWebExchange.from(request);
    }

    @AfterEach
    public void clear() {
        MatchDataCache.getInstance().cleanSelectorData();
        MatchDataCache.getInstance().cleanRuleDataData();
    }

    private void clearCache() {
        BaseDataCache.getInstance().cleanPluginData();
        BaseDataCache.getInstance().cleanSelectorData();
        BaseDataCache.getInstance().cleanRuleData();
        MatchDataCache.getInstance().cleanSelectorData();
        MatchDataCache.getInstance().cleanRuleDataData();
    }

    private void mockShenyuConfig() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        when(context.getBean(ShenyuConfig.class)).thenReturn(new ShenyuConfig());
        SpringBeanUtils.getInstance().setApplicationContext(context);
    }

    static class TestShenyuPlugin extends AbstractShenyuPlugin {

        @Override
        protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
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
