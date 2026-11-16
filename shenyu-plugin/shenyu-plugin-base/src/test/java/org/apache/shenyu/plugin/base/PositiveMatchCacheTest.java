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
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.base.cache.MatchDataCache;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mockStatic;

/**
 * Positive path cache entries must respect all competing candidates.
 */
class PositiveMatchCacheTest {

    private static final String PLUGIN = "positive-cache-test";

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
    void genericSelectorMustNotShadowDynamicCompetitor(final String paramType) {
        SelectorData generic = selector("generic", List.of(uriCondition()), false);
        SelectorData special = selector("special", List.of(uriCondition(), dynamicCondition(paramType)), false);
        BaseDataCache.getInstance().cacheSelectData(generic);
        BaseDataCache.getInstance().cacheSelectData(special);

        execute(false);
        assertSame(generic, plugin.selector);
        execute(true);
        assertSame(special, plugin.selector);
        assertNull(MatchDataCache.getInstance().obtainSelectorData(PLUGIN, "/x"));
        MatchDataCache.getInstance().cleanSelectorData();
        execute(true);
        assertSame(special, plugin.selector);
        execute(false);
        assertSame(generic, plugin.selector);
    }

    @ParameterizedTest
    @ValueSource(strings = {"header", "query", "req_method"})
    void genericRuleMustNotShadowDynamicCompetitor(final String paramType) {
        SelectorData selector = selector("selector", List.of(uriCondition()), true);
        RuleData generic = rule("generic", List.of(uriCondition()));
        RuleData special = rule("special", List.of(uriCondition(), dynamicCondition(paramType)));
        BaseDataCache.getInstance().cacheSelectData(selector);
        BaseDataCache.getInstance().cacheRuleData(generic);
        BaseDataCache.getInstance().cacheRuleData(special);

        execute(false);
        assertSame(generic, plugin.rule);
        execute(true);
        assertSame(special, plugin.rule);
        assertNull(MatchDataCache.getInstance().obtainRuleData(PLUGIN, "/x"));
        MatchDataCache.getInstance().cleanRuleDataData();
        execute(true);
        assertSame(special, plugin.rule);
        execute(false);
        assertSame(generic, plugin.rule);
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void pathOnlyCandidatesStillCacheWithDisabledDynamicCompetitors(final boolean includeDisabled) {
        SelectorData generic = selector("generic", List.of(uriCondition()), true);
        RuleData genericRule = rule("generic", List.of(uriCondition()));
        genericRule.setSelectorId(generic.getId());
        BaseDataCache.getInstance().cacheSelectData(generic);
        BaseDataCache.getInstance().cacheRuleData(genericRule);
        BaseDataCache.getInstance().cacheSelectData(selector("other", List.of(condition("uri", "/", "/other")), false));
        RuleData otherRule = rule("other", List.of(condition("uri", "/", "/other")));
        otherRule.setSelectorId(generic.getId());
        BaseDataCache.getInstance().cacheRuleData(otherRule);
        if (includeDisabled) {
            SelectorData disabled = selector("disabled", List.of(uriCondition(), dynamicCondition("header")), false);
            disabled.setEnabled(false);
            BaseDataCache.getInstance().cacheSelectData(disabled);
            RuleData disabledRule = rule("disabled", List.of(uriCondition(), dynamicCondition("header")));
            disabledRule.setEnabled(false);
            disabledRule.setSelectorId(generic.getId());
            BaseDataCache.getInstance().cacheRuleData(disabledRule);
        }
        execute(false);
        assertSame(generic, MatchDataCache.getInstance().obtainSelectorData(PLUGIN, "/x"));
        assertSame(genericRule, MatchDataCache.getInstance().obtainRuleData(PLUGIN, "/x"));
        execute(true);
        assertSame(generic, plugin.selector);
        assertSame(genericRule, plugin.rule);
        assertEquals(2, plugin.executions);
    }

    @ParameterizedTest
    @CsvSource({"TimeAfter,false", "TimeAfter,true", "TimeBefore,false", "TimeBefore,true"})
    void uriTimePredicatesMustNotMakePositiveResultsReusable(final String operator, final boolean continued) {
        LocalDateTime boundary = LocalDateTime.of(2026, 1, 1, 12, 0);
        ConditionData timed = condition("uri", "", "2026-01-01 12:00:00");
        timed.setOperator(operator);
        SelectorData genericSelector = selector("selector", List.of(uriCondition()), continued);
        BaseDataCache.getInstance().cacheSelectData(genericSelector);
        if (continued) {
            BaseDataCache.getInstance().cacheRuleData(rule("generic", List.of(uriCondition())));
            BaseDataCache.getInstance().cacheRuleData(rule("special", List.of(uriCondition(), timed)));
        } else {
            BaseDataCache.getInstance().cacheSelectData(selector("special", List.of(uriCondition(), timed), false));
        }
        try (MockedStatic<LocalDateTime> time = mockStatic(LocalDateTime.class, CALLS_REAL_METHODS)) {
            time.when(LocalDateTime::now).thenReturn("TimeAfter".equals(operator) ? boundary.minusSeconds(1) : boundary.plusSeconds(1));
            execute(false);
            assertEquals(continued ? "generic" : "selector", continued ? plugin.rule.getId() : plugin.selector.getId());
            time.when(LocalDateTime::now).thenReturn("TimeAfter".equals(operator) ? boundary.plusSeconds(1) : boundary.minusSeconds(1));
            execute(false);
            assertEquals("special", continued ? plugin.rule.getId() : plugin.selector.getId());
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void pathCacheMustUseTheSameRewrittenUriAsMatching(final boolean continued) {
        ConditionData first = condition("uri", "/", "/rewritten/a");
        ConditionData second = condition("uri", "/", "/rewritten/b");
        if (continued) {
            ConditionData either = condition("uri", "/", "/rewritten/**");
            either.setOperator("match");
            BaseDataCache.getInstance().cacheSelectData(selector("selector", List.of(either), true));
            BaseDataCache.getInstance().cacheRuleData(rule("first", List.of(first)));
            BaseDataCache.getInstance().cacheRuleData(rule("second", List.of(second)));
        } else {
            BaseDataCache.getInstance().cacheSelectData(selector("first", List.of(first), false));
            BaseDataCache.getInstance().cacheSelectData(selector("second", List.of(second), false));
        }
        executeRewritten("/a");
        assertEquals("first", continued ? plugin.rule.getId() : plugin.selector.getId());
        executeRewritten("/b");
        assertEquals("second", continued ? plugin.rule.getId() : plugin.selector.getId());
        executeRewritten("/a");
        assertEquals("first", continued ? plugin.rule.getId() : plugin.selector.getId());
    }

    private void executeRewritten(final String realUrl) {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/entry"));
        ShenyuContext context = new ShenyuContext();
        context.setRealUrl(realUrl);
        exchange.getAttributes().put(Constants.CONTEXT, context);
        exchange.getAttributes().put(Constants.CONTEXT_PATH, "/rewritten");
        plugin.execute(exchange, next -> Mono.empty()).block();
    }

    private void execute(final boolean special) {
        plugin.execute(MockServerWebExchange.from(MockServerHttpRequest.method(special ? HttpMethod.POST : HttpMethod.GET,
                special ? "/x?tenant=a" : "/x?tenant=b").header("X-Tenant", special ? "a" : "b")), exchange -> Mono.empty()).block();
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

        private int executions;

        @Override
        protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selectorData, final RuleData ruleData) {
            selector = selectorData;
            rule = ruleData;
            executions++;
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
