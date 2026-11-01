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
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.base.cache.MatchDataCache;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.MockedStatic;
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.StaticApplicationContext;
import org.springframework.http.HttpMethod;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.time.LocalDateTime;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mockStatic;

/**
 * Negative path cache entries must respect every competing candidate and operator.
 */
class NegativeMatchCacheEligibilityTest {

    private static final String PLUGIN = "negative-match-cache-test";

    private static final String TIMED_BOUNDARY = "2026-01-01 12:00:00";

    private final RecordingPlugin plugin = new RecordingPlugin();

    private final StaticApplicationContext context = new StaticApplicationContext();

    private ApplicationContext previousContext;

    @BeforeEach
    void setUp() {
        previousContext = SpringBeanUtils.getInstance().getApplicationContext();
        context.getBeanFactory().registerSingleton("shenyuConfig", new ShenyuConfig());
        SpringBeanUtils.getInstance().setApplicationContext(context);
        clearCaches();
        BaseDataCache.getInstance().cachePluginData(PluginData.builder().name(PLUGIN).enabled(true).build());
    }

    @AfterEach
    void tearDown() {
        clearCaches();
        SpringBeanUtils.getInstance().setApplicationContext(previousContext);
        context.close();
    }

    @ParameterizedTest
    @ValueSource(strings = {"header", "query", "req_method"})
    void dynamicSelectorMissMustNotBeCached(final String paramType) {
        SelectorData dynamic = selector("dynamic", List.of(uriCondition(), dynamicCondition(paramType)), false);
        BaseDataCache.getInstance().cacheSelectData(dynamic);

        execute(false);
        assertNull(MatchDataCache.getInstance().obtainSelectorData(PLUGIN, "/x"));
        assertNull(plugin.selector);

        execute(true);
        assertSame(dynamic, plugin.selector);
    }

    @ParameterizedTest
    @ValueSource(strings = {"header", "query", "req_method"})
    void dynamicRuleMissMustNotBeCached(final String paramType) {
        SelectorData selector = selector("selector", List.of(uriCondition()), true);
        RuleData dynamic = rule("dynamic", List.of(uriCondition(), dynamicCondition(paramType)));
        BaseDataCache.getInstance().cacheSelectData(selector);
        BaseDataCache.getInstance().cacheRuleData(dynamic);

        execute(false);
        assertNull(MatchDataCache.getInstance().obtainRuleData(PLUGIN, "selector", "/x"));
        assertNull(plugin.rule);

        execute(true);
        assertSame(dynamic, plugin.rule);
    }

    @Test
    void ruleMissMustNotBeCachedAcrossDynamicSelectors() {
        SelectorData first = selector("first", List.of(uriCondition(), condition("header", "X-Tenant", "a")), true);
        BaseDataCache.getInstance().cacheSelectData(first);
        RuleData firstRule = rule("first-rule", List.of(condition("uri", "/", "/other")));
        firstRule.setSelectorId("first");
        BaseDataCache.getInstance().cacheRuleData(firstRule);
        SelectorData second = selector("second", List.of(uriCondition(), condition("header", "X-Tenant", "b")), true);
        BaseDataCache.getInstance().cacheSelectData(second);
        RuleData secondRule = rule("second-rule", List.of(uriCondition()));
        secondRule.setSelectorId("second");
        BaseDataCache.getInstance().cacheRuleData(secondRule);

        execute(true);
        assertNotNull(MatchDataCache.getInstance().obtainRuleData(PLUGIN, first.getId(), "/x"));
        assertNull(MatchDataCache.getInstance().obtainRuleData(PLUGIN, second.getId(), "/x"));

        execute(false);
        assertSame(secondRule, plugin.rule);
    }

    @ParameterizedTest
    @ValueSource(strings = {"TimeAfter", "TimeBefore"})
    void timedSelectorMissMustNotBeCached(final String operator) {
        LocalDateTime boundary = LocalDateTime.of(2026, 1, 1, 12, 0);
        SelectorData special = selector("special", List.of(uriCondition(), timedCondition(operator)), false);
        BaseDataCache.getInstance().cacheSelectData(special);

        try (MockedStatic<LocalDateTime> time = mockStatic(LocalDateTime.class, CALLS_REAL_METHODS)) {
            AtomicReference<LocalDateTime> clock = new AtomicReference<>();
            time.when(LocalDateTime::now).thenAnswer(invocation -> clock.get());

            clock.set(predicateFalse(operator, boundary));
            execute(false);
            assertNull(MatchDataCache.getInstance().obtainSelectorData(PLUGIN, "/x"));
            assertNull(plugin.selector);

            clock.set(predicateTrue(operator, boundary));
            execute(false);
            assertSame(special, plugin.selector);
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"TimeAfter", "TimeBefore"})
    void timedRuleMissMustNotBeCached(final String operator) {
        LocalDateTime boundary = LocalDateTime.of(2026, 1, 1, 12, 0);
        SelectorData selector = selector("selector", List.of(uriCondition()), true);
        RuleData timed = rule("timed", List.of(uriCondition(), timedCondition(operator)));
        BaseDataCache.getInstance().cacheSelectData(selector);
        BaseDataCache.getInstance().cacheRuleData(timed);

        try (MockedStatic<LocalDateTime> time = mockStatic(LocalDateTime.class, CALLS_REAL_METHODS)) {
            AtomicReference<LocalDateTime> clock = new AtomicReference<>();
            time.when(LocalDateTime::now).thenAnswer(invocation -> clock.get());

            clock.set(predicateFalse(operator, boundary));
            execute(false);
            assertNull(MatchDataCache.getInstance().obtainRuleData(PLUGIN, "selector", "/x"));
            assertNull(plugin.rule);

            clock.set(predicateTrue(operator, boundary));
            execute(false);
            assertSame(timed, plugin.rule);
        }
    }

    @Test
    void pathOnlySelectorMissStillCaches() {
        BaseDataCache.getInstance().cacheSelectData(selector("other", List.of(condition("uri", "/", "/other")), false));

        execute(false);

        SelectorData cached = MatchDataCache.getInstance().obtainSelectorData(PLUGIN, "/x");
        assertNotNull(cached);
        assertNull(cached.getId());
    }

    @Test
    void pathOnlyRuleMissStillCaches() {
        BaseDataCache.getInstance().cacheSelectData(selector("selector", List.of(uriCondition()), true));
        BaseDataCache.getInstance().cacheRuleData(rule("other", List.of(condition("uri", "/", "/other"))));

        execute(false);

        RuleData cached = MatchDataCache.getInstance().obtainRuleData(PLUGIN, "selector", "/x");
        assertNotNull(cached);
        assertNull(cached.getId());
    }

    private LocalDateTime predicateFalse(final String operator, final LocalDateTime boundary) {
        return "TimeAfter".equals(operator) ? boundary.minusSeconds(1) : boundary.plusSeconds(1);
    }

    private LocalDateTime predicateTrue(final String operator, final LocalDateTime boundary) {
        return "TimeAfter".equals(operator) ? boundary.plusSeconds(1) : boundary.minusSeconds(1);
    }

    private void execute(final boolean dynamicPresent) {
        plugin.execute(MockServerWebExchange.from(MockServerHttpRequest.method(dynamicPresent ? HttpMethod.POST : HttpMethod.GET,
                dynamicPresent ? "/x?tenant=a" : "/x?tenant=b").header("X-Tenant", dynamicPresent ? "a" : "b")), exchange -> Mono.empty()).block();
    }

    private SelectorData selector(final String id, final List<ConditionData> conditions, final boolean continued) {
        return SelectorData.builder().id(id).pluginName(PLUGIN).enabled(true).logged(false).matchRestful(false)
                .type(SelectorTypeEnum.CUSTOM_FLOW.getCode()).matchMode(0).sort(0).continued(continued).conditionList(conditions).build();
    }

    private RuleData rule(final String id, final List<ConditionData> conditions) {
        return RuleData.builder().id(id).selectorId("selector").pluginName(PLUGIN).enabled(true).logged(false)
                .matchRestful(false).matchMode(0).sort(0).conditionDataList(conditions).build();
    }

    private ConditionData uriCondition() {
        return condition("uri", "/", "/x");
    }

    private ConditionData timedCondition(final String operator) {
        ConditionData condition = condition("uri", "", TIMED_BOUNDARY);
        condition.setOperator(operator);
        return condition;
    }

    private ConditionData dynamicCondition(final String type) {
        return condition(type, "header".equals(type) ? "X-Tenant" : "tenant", "req_method".equals(type) ? "POST" : "a");
    }

    private ConditionData condition(final String type, final String name, final String value) {
        ConditionData condition = new ConditionData();
        condition.setParamType(type);
        condition.setParamName(name);
        condition.setOperator("=");
        condition.setParamValue(value);
        return condition;
    }

    private void clearCaches() {
        BaseDataCache.getInstance().cleanPluginData();
        BaseDataCache.getInstance().cleanSelectorData();
        BaseDataCache.getInstance().cleanRuleData();
        MatchDataCache.getInstance().cleanSelectorData();
        MatchDataCache.getInstance().cleanRuleDataData();
    }

    private static final class RecordingPlugin extends AbstractShenyuPlugin {

        private SelectorData selector;

        private RuleData rule;

        @Override
        protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selectorData, final RuleData ruleData) {
            selector = selectorData;
            rule = ruleData;
            return Mono.empty();
        }

        @Override
        public int getOrder() {
            return 0;
        }

        @Override
        public String named() {
            return PLUGIN;
        }
    }
}
