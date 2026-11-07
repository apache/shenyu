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

package org.apache.shenyu.plugin.divide;

import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.DivideRuleHandle;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.base.cache.MatchDataCache;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.base.cache.CommonPluginDataSubscriber;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.divide.handler.DividePluginDataHandler;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Exercises production matching and forwarding without opening network connections.
 */
class DividePublishedConfigurationTest {

    private final BaseDataCache cache = BaseDataCache.getInstance();

    private final MatchDataCache matches = MatchDataCache.getInstance();

    private final AtomicInteger forwarded = new AtomicInteger();

    private final ShenyuConfig config = new ShenyuConfig();

    private SelectorData selector;

    private RuleData rule;

    @BeforeEach
    void setUp() {
        cleanup();
        config.getSelectorMatchCache().getCache().setEnabled(true);
        config.getRuleMatchCache().getCache().setEnabled(true);
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        when(context.getBean(ShenyuConfig.class)).thenReturn(config);
        when(context.getBean(org.apache.shenyu.plugin.api.result.ShenyuResult.class)).thenReturn(new org.apache.shenyu.plugin.api.result.DefaultShenyuResult());
        SpringBeanUtils.getInstance().setApplicationContext(context);
        cache.cachePluginData(PluginData.builder().name("divide").enabled(true).build());
        selector = SelectorData.builder().id("published-selector").pluginName("divide").name("published")
                .type(SelectorTypeEnum.CUSTOM_FLOW.getCode()).matchMode(0).enabled(true).logged(false).sort(0)
                .conditionList(List.of(condition())).continued(true).build();
        rule = RuleData.builder().id("published-rule").selectorId(selector.getId()).pluginName("divide").name("published")
                .enabled(true).logged(false).sort(0).matchMode(0).conditionDataList(List.of(condition())).handle("{}").build();
        cache.cacheSelectData(selector);
        cache.cacheRuleData(rule);
        Upstream upstream = Upstream.builder().protocol("http://").url("127.0.0.1:18001").weight(50).healthCheckEnabled(false).build();
        UpstreamCacheManager.getInstance().submit(selector.getId(), List.of(upstream));
        DivideRuleHandle handle = DivideRuleHandle.newInstance();
        handle.setRetry(0);
        DividePluginDataHandler.CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(rule), handle);
    }

    @AfterEach
    void cleanup() {
        cache.cleanPluginData();
        cache.cleanSelectorData();
        cache.cleanRuleData();
        matches.cleanSelectorData();
        matches.cleanRuleDataData();
        UpstreamCacheManager.getInstance().removeByKey("published-selector");
        DividePluginDataHandler.CACHED_HANDLE.get().removeHandle(CacheKeyUtils.INST.getKey("published-selector", "published-rule"));
    }

    @Test
    void divideMatchesFrozenValuesAndIgnoresLegacyMutationsOnCacheHit() {
        ProbingDivide plugin = new ProbingDivide();
        execute(plugin, "/original");
        SelectorData seen = plugin.selector;
        RuleData seenRule = plugin.rule;
        assertTrue(seen.frozen());
        assertTrue(seenRule.frozen());
        assertThrows(UnsupportedOperationException.class, () -> seen.setEnabled(false));
        assertThrows(UnsupportedOperationException.class, () -> seenRule.getConditionDataList().get(0).setParamValue("/polluted"));
        selector.setEnabled(false);
        selector.getConditionList().get(0).setParamValue("/polluted");
        rule.setEnabled(false);
        matches.getSelectorMatchCache().clear();
        matches.getRuleMatchCache().clear();
        execute(plugin, "/original");
        assertSame(seen, plugin.selector);
        assertSame(seenRule, plugin.rule);
        assertEquals(2, forwarded.get());
        assertSame(seen, matches.readOnly().obtainSelectorData("divide", "/original"));
        assertSame(seenRule, matches.readOnly().obtainRuleData("divide", "/original"));
    }

    @Test
    void subscriberUpdateInvalidatesFrozenPositiveAndNegativeMatches() {
        ProbingDivide plugin = new ProbingDivide();
        execute(plugin, "/original");
        execute(plugin, "/missing");
        assertNull(matches.readOnly().obtainSelectorData("divide", "/missing").getId());
        CommonPluginDataSubscriber subscriber = new CommonPluginDataSubscriber(List.<PluginDataHandler>of(new DividePluginDataHandler()),
                config.getSelectorMatchCache(), config.getRuleMatchCache());
        selector.getConditionList().get(0).setParamValue("/missing");
        rule.getConditionDataList().get(0).setParamValue("/missing");
        subscriber.onSelectorSubscribe(selector);
        subscriber.onRuleSubscribe(rule);
        execute(plugin, "/missing");
        assertEquals("/missing", plugin.selector.getConditionList().get(0).getParamValue());
        assertEquals(2, forwarded.get());
        subscriber.unRuleSubscribe(rule);
        plugin.rule = null;
        execute(plugin, "/missing");
        assertNull(plugin.rule);
    }

    @Test
    void defaultRuleIsRequestOwnedAndDoesNotMutateThePublishedSelector() {
        selector.setContinued(false);
        cache.cacheSelectData(selector);
        DividePluginDataHandler.CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(selector.getId(), Constants.DEFAULT_RULE), DivideRuleHandle.newInstance());
        ProbingDivide plugin = new ProbingDivide();
        execute(plugin, "/original");
        RuleData first = plugin.rule;
        assertFalse(first.frozen());
        first.setHandle("request override");
        execute(plugin, "/original");
        assertNull(plugin.rule.getHandle());
        assertTrue(plugin.selector.frozen());
        DividePluginDataHandler.CACHED_HANDLE.get().removeHandle(CacheKeyUtils.INST.getKey(selector.getId(), Constants.DEFAULT_RULE));
    }

    @Test
    void legacyPluginKeepsMutableConfigurationAndHealthCountersRemainLive() {
        AtomicReference<SelectorData> observed = new AtomicReference<>();
        AbstractShenyuPlugin legacy = new AbstractShenyuPlugin() {
            @Override
            protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData data, final RuleData matched) {
                observed.set(data);
                return Mono.empty();
            }

            @Override
            public String named() {
                return "divide";
            }

            @Override
            public int getOrder() {
                return 0;
            }
        };
        legacy.execute(exchange("/original"), ignored -> Mono.empty()).block();
        assertSame(selector, observed.get());
        observed.get().setHandle("legacy mutation");
        assertNull(cache.readOnly().obtainSelectorData("divide").get(0).getHandle());
        Upstream upstream = UpstreamCacheManager.getInstance().findUpstreamListBySelectorId(selector.getId()).get(0);
        long before = upstream.getInflight().get();
        assertEquals(before + 1, upstream.getInflight().incrementAndGet());
        upstream.getInflight().decrementAndGet();
        upstream.setStatus(false);
        assertFalse(upstream.isStatus());
    }

    private void execute(final DividePlugin plugin, final String path) {
        ServerWebExchange exchange = exchange(path);
        plugin.execute(exchange, current -> {
            if (java.util.Objects.nonNull(current.getAttribute(Constants.HTTP_DOMAIN))) {
                forwarded.incrementAndGet();
            }
            return Mono.empty();
        }).block();
    }

    private ServerWebExchange exchange(final String path) {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get(path).build());
        ShenyuContext context = new ShenyuContext();
        context.setRealUrl(path);
        context.setRpcType("http");
        exchange.getAttributes().put(Constants.CONTEXT, context);
        return exchange;
    }

    private ConditionData condition() {
        ConditionData data = new ConditionData();
        data.setParamType("uri");
        data.setOperator("=");
        data.setParamValue("/original");
        return data;
    }

    private static final class ProbingDivide extends DividePlugin {

        private SelectorData selector;

        private RuleData rule;

        @Override
        protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selected, final RuleData matched) {
            selector = selected;
            rule = matched;
            return super.doExecute(exchange, chain, selected, matched);
        }
    }
}
