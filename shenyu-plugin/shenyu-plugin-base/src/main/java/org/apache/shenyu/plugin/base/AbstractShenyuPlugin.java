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
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.MatchModeEnum;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.common.utils.LogUtils;
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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * abstract shenyu plugin please extends.
 */
public abstract class AbstractShenyuPlugin implements ShenyuPlugin {

    private static final Logger LOG = LoggerFactory.getLogger(AbstractShenyuPlugin.class);

    private static final String URI_CONDITION_TYPE = "uri";

    private volatile ShenyuConfig.SelectorMatchCache selectorMatchConfig;

    private volatile ShenyuConfig.RuleMatchCache ruleMatchConfig;

    /**
     * this is Template Method child has implements your own logic.
     *
     * @param exchange exchange the current server exchange {@linkplain ServerWebExchange}
     * @param chain chain the current chain  {@linkplain ServerWebExchange}
     * @param selector selector    {@linkplain SelectorData}
     * @param rule rule    {@linkplain RuleData}
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
        initCacheConfig();
        final String pluginName = named();
        PluginData pluginData = BaseDataCache.getInstance().obtainPluginData(pluginName);
        if (Objects.isNull(pluginData) || !pluginData.getEnabled()) {
            return chain.execute(exchange);
        }
        final String path = getRawPath(exchange);
        List<SelectorData> selectors = BaseDataCache.getInstance().obtainSelectorData(pluginName);
        if (CollectionUtils.isEmpty(selectors)) {
            return handleSelectorIfNull(pluginName, exchange, chain);
        }
        SelectorData selectorData = twoLevelCacheLookupSelector(exchange, selectors, path);
        if (Objects.isNull(selectorData) || StringUtils.isBlank(selectorData.getId())) {
            return handleSelectorIfNull(pluginName, exchange, chain);
        }
        printLog(selectorData, pluginName);
        if (!selectorData.getContinued()) {
            return doExecute(exchange, chain, selectorData, defaultRuleData(selectorData));
        }
        List<RuleData> rules = BaseDataCache.getInstance().obtainRuleData(selectorData.getId());
        if (CollectionUtils.isEmpty(rules)) {
            return handleRuleIfNull(pluginName, exchange, chain);
        }
        if (selectorData.getType() == SelectorTypeEnum.FULL_FLOW.getCode()) {
            //get last
            RuleData rule = rules.get(rules.size() - 1);
            printLog(rule, pluginName);
            return doExecute(exchange, chain, selectorData, rule);
        }
        RuleData ruleData = twoLevelCacheLookupRule(exchange, rules, path);
        if (Objects.isNull(ruleData) || StringUtils.isBlank(ruleData.getId())) {
            return handleRuleIfNull(pluginName, exchange, chain);
        }
        printLog(ruleData, pluginName);
        return doExecute(exchange, chain, selectorData, ruleData);
    }

    private SelectorData twoLevelCacheLookupSelector(final ServerWebExchange exchange,
                                                     final List<SelectorData> selectors,
                                                     final String path) {
        // L1 cache hit: return directly
        SelectorData cached = obtainSelectorDataCacheIfEnabled(path);
        if (Objects.nonNull(cached)) {
            return cached;
        }
        // L1 miss: fall through to L2 full matching (also populates L1)
        return defaultMatchSelector(exchange, selectors, path);
    }

    private RuleData twoLevelCacheLookupRule(final ServerWebExchange exchange,
                                             final List<RuleData> rules,
                                             final String path) {
        // L1 cache hit: return directly
        RuleData cached = obtainRuleDataCacheIfEnabled(path);
        if (Objects.nonNull(cached)) {
            return cached;
        }
        // L1 miss: fall through to L2 full matching (also populates L1)
        return defaultMatchRule(exchange, rules, path);
    }
    
    protected String getRawPath(final ServerWebExchange exchange) {
        return exchange.getRequest().getURI().getRawPath();
    }
    
    private void initCacheConfig() {
        if (Objects.nonNull(selectorMatchConfig) && Objects.nonNull(ruleMatchConfig)) {
            return;
        }
        synchronized (this) {
            if (Objects.isNull(selectorMatchConfig) || Objects.isNull(ruleMatchConfig)) {
                ShenyuConfig shenyuConfig = SpringBeanUtils.getInstance().getBean(ShenyuConfig.class);
                selectorMatchConfig = shenyuConfig.getSelectorMatchCache();
                ruleMatchConfig = shenyuConfig.getRuleMatchCache();
            }
        }
    }
    
    private SelectorData obtainSelectorDataCacheIfEnabled(final String path) {
        return selectorMatchConfig.getCache().getEnabled() ? MatchDataCache.getInstance().obtainSelectorData(named(), path) : null;
    }
    
    private RuleData obtainRuleDataCacheIfEnabled(final String path) {
        return ruleMatchConfig.getCache().getEnabled() ? MatchDataCache.getInstance().obtainRuleData(named(), path) : null;
    }

    private void cacheSelectorData(final String path, final SelectorData selectorData) {
        cacheMatchData(
                path,
                selectorData,
                selectorMatchConfig.getCache(),
                SelectorData::getId,
                SelectorData::getMatchRestful,
                SelectorData::getConditionList,
                MatchDataCache.getInstance()::cacheSelectorData);
    }

    private void cacheRuleData(final String path, final RuleData ruleData) {
        cacheMatchData(
                path,
                ruleData,
                ruleMatchConfig.getCache(),
                RuleData::getId,
                RuleData::getMatchRestful,
                RuleData::getConditionDataList,
                MatchDataCache.getInstance()::cacheRuleData);
    }

    private <T> void cacheMatchData(final String path,
                                    final T data,
                                    final ShenyuConfig.MatchCacheConfig cacheConfig,
                                    final Function<T, String> idGetter,
                                    final Function<T, Boolean> restfulGetter,
                                    final Function<T, List<ConditionData>> conditionGetter,
                                    final CacheWriter<T> cacheWriter) {
        // if the cache is disabled or data is null or matchRestful, do not cache.
        if (Boolean.FALSE.equals(cacheConfig.getEnabled())
                || Objects.isNull(data)
                || Boolean.TRUE.equals(restfulGetter.apply(data))) {
            return;
        }
        final int initialCapacity = cacheConfig.getInitialCapacity();
        final long maximumSize = cacheConfig.getMaximumSize();
        // empty-id sentinel: always cached to short-circuit the next miss.
        if (StringUtils.isBlank(idGetter.apply(data))) {
            cacheWriter.write(path, data, initialCapacity, maximumSize);
            return;
        }
        // otherwise only cache when all conditions are uri-type.
        List<ConditionData> conditionList = conditionGetter.apply(data);
        if (CollectionUtils.isNotEmpty(conditionList)
                && conditionList.stream().allMatch(v -> URI_CONDITION_TYPE.equals(v.getParamType()))) {
            cacheWriter.write(path, data, initialCapacity, maximumSize);
        }
    }

    private RuleData defaultRuleData(final SelectorData selectorData) {
        RuleData ruleData = new RuleData();
        ruleData.setSelectorId(selectorData.getId());
        ruleData.setPluginName(selectorData.getPluginName());
        ruleData.setId(Constants.DEFAULT_RULE);
        return ruleData;
    }
    
    /**
     * Handle selector if null mono.
     *
     * @param pluginName the plugin name
     * @param exchange the exchange
     * @param chain the chain
     * @return the mono
     */
    protected Mono<Void> handleSelectorIfNull(final String pluginName, final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        return chain.execute(exchange);
    }
    
    /**
     * Handle rule if null mono.
     *
     * @param pluginName the plugin name
     * @param exchange the exchange
     * @param chain the chain
     * @return the mono
     */
    protected Mono<Void> handleRuleIfNull(final String pluginName, final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        return chain.execute(exchange);
    }

    private Pair<Boolean, SelectorData> matchSelector(final ServerWebExchange exchange, final Collection<SelectorData> selectors) {
        // hot path: prefer plain loop over stream to reduce allocations.
        // de-duplicate via LinkedHashSet to preserve original semantics (equals/hashCode based).
        Set<SelectorData> matched = null;
        for (SelectorData selector : selectors) {
            if (selector.getEnabled() && filterSelector(selector, exchange)) {
                if (Objects.isNull(matched)) {
                    matched = new LinkedHashSet<>(4);
                }
                matched.add(selector);
            }
        }
        if (Objects.isNull(matched) || matched.isEmpty()) {
            return Pair.of(Boolean.TRUE, null);
        }
        if (matched.size() == 1) {
            return Pair.of(Boolean.TRUE, matched.iterator().next());
        }
        return Pair.of(Boolean.FALSE, manyMatchSelector(new ArrayList<>(matched)));
    }

    private SelectorData manyMatchSelector(final List<SelectorData> filterCollectors) {
        //What needs to be dealt with here is the and condition. If the number of and conditions is the same and is matched at the same time,
        // it will be sorted by the sort field.
        Map<Integer, List<SelectorData>> collect = filterCollectors.stream()
                .collect(Collectors.groupingBy(this::andConditionSortKeyOfSelector));
        Integer max = Collections.max(collect.keySet());
        return collect.get(max).stream().min(Comparator.comparing(SelectorData::getSort)).orElse(null);
    }

    private int andConditionSortKeyOfSelector(final SelectorData selector) {
        return MatchModeEnum.match(selector.getMatchMode(), MatchModeEnum.AND)
                ? selector.getConditionList().size() : 0;
    }

    private Boolean filterSelector(final SelectorData selector, final ServerWebExchange exchange) {
        if (selector.getType() != SelectorTypeEnum.CUSTOM_FLOW.getCode()) {
            return true;
        }
        if (CollectionUtils.isEmpty(selector.getConditionList())) {
            return false;
        }
        return MatchStrategyFactory.match(selector.getMatchMode(), selector.getConditionList(), exchange);
    }

    private Pair<Boolean, RuleData> matchRule(final ServerWebExchange exchange, final Collection<RuleData> rules) {
        // hot path: prefer plain loop over stream to reduce allocations.
        // de-duplicate via LinkedHashSet to preserve original semantics (equals/hashCode based).
        Set<RuleData> matched = null;
        for (RuleData rule : rules) {
            if (filterRule(rule, exchange)) {
                if (Objects.isNull(matched)) {
                    matched = new LinkedHashSet<>(4);
                }
                matched.add(rule);
            }
        }
        if (Objects.isNull(matched) || matched.isEmpty()) {
            return Pair.of(Boolean.TRUE, null);
        }
        if (matched.size() == 1) {
            return Pair.of(Boolean.TRUE, matched.iterator().next());
        }
        return Pair.of(Boolean.FALSE, manyMatchRule(new ArrayList<>(matched)));
    }

    private RuleData manyMatchRule(final List<RuleData> filterRuleData) {
        //What needs to be dealt with here is the and condition. If the number of and conditions is the same and is matched at the same time,
        // it will be sorted by the sort field.
        Map<Integer, List<RuleData>> collect = filterRuleData.stream()
                .collect(Collectors.groupingBy(this::andConditionSortKeyOfRule));
        Integer max = Collections.max(collect.keySet());
        return collect.get(max).stream().min(Comparator.comparing(RuleData::getSort)).orElse(null);
    }

    private int andConditionSortKeyOfRule(final RuleData rule) {
        return MatchModeEnum.match(rule.getMatchMode(), MatchModeEnum.AND)
                ? rule.getConditionDataList().size() : 0;
    }

    private Boolean filterRule(final RuleData ruleData, final ServerWebExchange exchange) {
        return ruleData.getEnabled() && MatchStrategyFactory.match(ruleData.getMatchMode(), ruleData.getConditionDataList(), exchange);
    }
    
    private SelectorData defaultMatchSelector(final ServerWebExchange exchange, final List<SelectorData> selectors, final String path) {
        Pair<Boolean, SelectorData> matchSelectorPair = matchSelector(exchange, selectors);
        SelectorData selectorData = matchSelectorPair.getRight();
        final boolean cacheable = matchSelectorPair.getLeft();
        if (Objects.nonNull(selectorData)) {
            LogUtils.info(LOG, "{} selector match success from default strategy", named());
            if (cacheable) {
                // cache selector data
                cacheSelectorData(path, selectorData);
            }
            return selectorData;
        }
        if (cacheable) {
            // if not match selector, cache empty selector data.
            cacheSelectorData(path, emptySelectorData());
        }
        return null;
    }

    private SelectorData emptySelectorData() {
        return SelectorData.builder().pluginName(named()).build();
    }
    
    private RuleData defaultMatchRule(final ServerWebExchange exchange, final List<RuleData> rules, final String path) {
        Pair<Boolean, RuleData> matchRulePair = matchRule(exchange, rules);
        RuleData ruleData = matchRulePair.getRight();
        final boolean cacheable = matchRulePair.getLeft();
        if (Objects.nonNull(ruleData)) {
            LOG.info("{} rule match path from default strategy", named());
            if (cacheable) {
                // cache rule data
                cacheRuleData(path, ruleData);
            }
            return ruleData;
        }
        if (cacheable) {
            // if not match rule, cache empty rule data.
            cacheRuleData(path, emptyRuleData());
        }
        return null;
    }

    private RuleData emptyRuleData() {
        return RuleData.builder().pluginName(named()).build();
    }
    
    /**
     * print selector log.
     * please don't delete this method or refactor {@linkplain org.apache.shenyu.plugin.base.AbstractShenyuPlugin#printLog}
     * because instanceof and class cast waste 10% cpu.
     *
     * @param selectorData selector data
     * @param pluginName plugin name
     */
    private void printLog(final SelectorData selectorData, final String pluginName) {
        if (selectorData.getLogged()) {
            LOG.info("{} selector success match , selector name :{}", pluginName, selectorData.getName());
        }
    }
    
    /**
     * print rule log.
     *
     * @param ruleData rule data
     * @param pluginName plugin name
     */
    private void printLog(final RuleData ruleData, final String pluginName) {
        if (ruleData.getLoged()) {
            LOG.info("{} rule success match , rule name :{}", pluginName, ruleData.getName());
        }
    }

    @FunctionalInterface
    private interface CacheWriter<T> {
        void write(String path, T data, int initialCapacity, long maximumSize);
    }

}
