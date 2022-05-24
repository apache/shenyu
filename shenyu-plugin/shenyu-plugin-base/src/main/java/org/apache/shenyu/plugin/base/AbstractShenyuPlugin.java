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

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.base.cache.MatchDataCache;
import org.apache.shenyu.plugin.base.condition.strategy.MatchStrategyFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Collection;
import java.util.List;
import java.util.Objects;

/**
 * abstract shenyu plugin please extends.
 */
public abstract class AbstractShenyuPlugin implements ShenyuPlugin {

    private static final Logger LOG = LoggerFactory.getLogger(AbstractShenyuPlugin.class);

    private static ShenyuConfig.MatchCache matchCacheConfig;

    private static SelectorData nullSelectorData = new SelectorData();

    private static RuleData nullRuleData = new RuleData();

    /**
     * this is Template Method child has Implement your own logic.
     *
     * @param exchange exchange the current server exchange {@linkplain ServerWebExchange}
     * @param chain    chain the current chain  {@linkplain ServerWebExchange}
     * @param selector selector    {@linkplain SelectorData}
     * @param rule     rule    {@linkplain RuleData}
     * @return {@code Mono<Void>} to indicate when request handling is complete
     */
    protected abstract Mono<Void> doExecute(ServerWebExchange exchange, ShenyuPluginChain chain, SelectorData selector, RuleData rule);

    /**
     * Process the Web request and (optionally) delegate to the next
     * {@code ShenyuPlugin} through the given {@link ShenyuPluginChain}.
     *
     * @param exchange the current server exchange
     * @param chain    provides a way to delegate to the next plugin
     * @return {@code Mono<Void>} to indicate when request processing is complete
     */
    @Override
    public Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        initMatchCacheConfig();
        String pluginName = named();
        PluginData pluginData = BaseDataCache.getInstance().obtainPluginData(pluginName);
        if (pluginData != null && pluginData.getEnabled()) {
            final String path = exchange.getRequest().getURI().getPath();

            Pair<Boolean, SelectorData> resultSelectorData = obtainSelectorDataCacheIfEnabled(exchange);
            SelectorData selectorData = resultSelectorData.getRight();
            if (Boolean.TRUE.equals(resultSelectorData.getLeft())) {
                List<SelectorData> selectors = BaseDataCache.getInstance().obtainSelectorData(pluginName);
                if (CollectionUtils.isEmpty(selectors)) {
                    return handleSelectorIfNull(pluginName, exchange, chain);
                }
                selectorData = matchSelector(exchange, selectors);
                cacheSelectorDataIfEnabled(path, selectorData);
            }
            if (Objects.isNull(selectorData)) {
                return handleSelectorIfNull(pluginName, exchange, chain);
            }
            selectorLog(selectorData, pluginName);

            Pair<Boolean, RuleData> resultRuleData = obtainRuleDataCacheIfEnabled(exchange);
            RuleData rule = resultRuleData.getRight();
            if (Boolean.TRUE.equals(resultRuleData.getLeft())) {
                List<RuleData> rules = BaseDataCache.getInstance().obtainRuleData(selectorData.getId());
                if (CollectionUtils.isEmpty(rules)) {
                    return handleRuleIfNull(pluginName, exchange, chain);
                }
                if (selectorData.getType() == SelectorTypeEnum.FULL_FLOW.getCode()) {
                    //get last
                    rule = rules.get(rules.size() - 1);
                } else {
                    rule = matchRule(exchange, rules);
                    cacheRuleDataIfEnabled(path, rule);
                }
            }
            if (Objects.isNull(rule)) {
                return handleRuleIfNull(pluginName, exchange, chain);
            }
            ruleLog(rule, pluginName);

            return doExecute(exchange, chain, selectorData, rule);
        }
        return chain.execute(exchange);
    }

    private void initMatchCacheConfig() {
        if (Objects.isNull(matchCacheConfig)) {
            matchCacheConfig = SpringBeanUtils.getInstance().getBean(ShenyuConfig.class).getMatchCache();
        }
    }

    private void cacheRuleDataIfEnabled(final String path, final RuleData rule) {
        if (matchCacheConfig.getEnabled()) {
            if (Objects.isNull(rule)) {
                nullRuleData.setPluginName(named());
                MatchDataCache.getInstance().cacheRuleData(path, nullRuleData, matchCacheConfig.getMaxFreeMemory());
            } else {
                MatchDataCache.getInstance().cacheRuleData(path, rule, matchCacheConfig.getMaxFreeMemory());
            }
        }
    }

    private Pair<Boolean, RuleData> obtainRuleDataCacheIfEnabled(final ServerWebExchange exchange) {
        if (matchCacheConfig.getEnabled()) {
            List<RuleData> rules = MatchDataCache.getInstance().obtainRuleData(named(), exchange.getRequest().getURI().getPath());

            if (Objects.isNull(rules)) {
                return Pair.of(Boolean.TRUE, null);
            }

            if (rules.size() == 0) {
                return Pair.of(Boolean.FALSE, null);
            }

            RuleData rule;
            if (rules.size() == 1) {
                rule = rules.get(0);
            } else {
                rule = matchRule(exchange, rules);
            }

            if (Objects.nonNull(rule)) {
                return Pair.of(Boolean.FALSE, rule);
            }

        }
        return Pair.of(Boolean.TRUE, null);
    }

    private void cacheSelectorDataIfEnabled(final String path, final SelectorData selectorData) {
        if (matchCacheConfig.getEnabled()) {
            if (Objects.isNull(selectorData)) {
                nullSelectorData.setPluginName(named());
                MatchDataCache.getInstance().cacheSelectorData(path, nullSelectorData, matchCacheConfig.getMaxFreeMemory());
            } else {
                MatchDataCache.getInstance().cacheSelectorData(path, selectorData, matchCacheConfig.getMaxFreeMemory());
            }
        }
    }

    private Pair<Boolean, SelectorData> obtainSelectorDataCacheIfEnabled(final ServerWebExchange exchange) {
        if (matchCacheConfig.getEnabled()) {
            List<SelectorData> selectors = MatchDataCache.getInstance().obtainSelectorData(named(), exchange.getRequest().getURI().getPath());

            if (Objects.isNull(selectors)) {
                return Pair.of(Boolean.TRUE, null);
            }

            if (selectors.size() == 0) {
                return Pair.of(Boolean.FALSE, null);
            }

            SelectorData selectorData;
            if (selectors.size() == 1) {
                selectorData = selectors.get(0);
            } else {
                selectorData = matchSelector(exchange, selectors);
            }

            if (Objects.nonNull(selectorData)) {
                return Pair.of(Boolean.FALSE, selectorData);
            }
        }
        return Pair.of(Boolean.TRUE, null);
    }

    protected Mono<Void> handleSelectorIfNull(final String pluginName, final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        return chain.execute(exchange);
    }

    protected Mono<Void> handleRuleIfNull(final String pluginName, final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        return chain.execute(exchange);
    }

    private SelectorData matchSelector(final ServerWebExchange exchange, final Collection<SelectorData> selectors) {
        return selectors.stream()
                .filter(selector -> selector.getEnabled() && filterSelector(selector, exchange))
                .findFirst().orElse(null);
    }

    private Boolean filterSelector(final SelectorData selector, final ServerWebExchange exchange) {
        if (selector.getType() == SelectorTypeEnum.CUSTOM_FLOW.getCode()) {
            if (CollectionUtils.isEmpty(selector.getConditionList())) {
                return false;
            }
            return MatchStrategyFactory.match(selector.getMatchMode(), selector.getConditionList(), exchange);
        }
        return true;
    }

    private RuleData matchRule(final ServerWebExchange exchange, final Collection<RuleData> rules) {
        return rules.stream().filter(rule -> filterRule(rule, exchange)).findFirst().orElse(null);
    }

    private Boolean filterRule(final RuleData ruleData, final ServerWebExchange exchange) {
        return ruleData.getEnabled() && MatchStrategyFactory.match(ruleData.getMatchMode(), ruleData.getConditionDataList(), exchange);
    }

    private void selectorLog(final SelectorData selectorData, final String pluginName) {
        if (selectorData.getLogged()) {
            LOG.info("{} selector success match , selector name :{}", pluginName, selectorData.getName());
        }
    }

    private void ruleLog(final RuleData ruleData, final String pluginName) {
        if (ruleData.getLoged()) {
            LOG.info("{} rule success match , rule name :{}", pluginName, ruleData.getName());
        }
    }
}
