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
import org.apache.shenyu.common.enums.TrieCacheTypeEnum;
import org.apache.shenyu.common.utils.ListUtil;
import org.apache.shenyu.common.utils.LogUtils;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.base.cache.MatchDataCache;
import org.apache.shenyu.plugin.base.condition.strategy.MatchStrategyFactory;
import org.apache.shenyu.plugin.base.trie.ShenyuTrie;
import org.apache.shenyu.plugin.base.trie.ShenyuTrieNode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * abstract shenyu plugin please extends.
 */
public abstract class AbstractShenyuPlugin implements ShenyuPlugin {

    private static final Logger LOG = LoggerFactory.getLogger(AbstractShenyuPlugin.class);

    private static final String URI_CONDITION_TYPE = "uri";
    
    private ShenyuTrie selectorTrie;
    
    private ShenyuTrie ruleTrie;
    
    private ShenyuConfig.SelectorMatchCache selectorMatchConfig;
    
    private ShenyuConfig.RuleMatchCache ruleMatchConfig;

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
        // early exit
        if (Objects.isNull(pluginData) || !pluginData.getEnabled()) {
            return chain.execute(exchange);
        }
        final String path = getRawPath(exchange);
        List<SelectorData> selectors = BaseDataCache.getInstance().obtainSelectorData(pluginName);
        if (CollectionUtils.isEmpty(selectors)) {
            return handleSelectorIfNull(pluginName, exchange, chain);
        }
        SelectorData selectorData = obtainSelectorDataCacheIfEnabled(path);
        // handle Selector
        if (Objects.nonNull(selectorData) && StringUtils.isBlank(selectorData.getId())) {
            return handleSelectorIfNull(pluginName, exchange, chain);
        }
        if (Objects.isNull(selectorData)) {
            selectorData = trieMatchSelector(exchange, pluginName, path);
            if (Objects.isNull(selectorData)) {
                selectorData = defaultMatchSelector(exchange, selectors, path);
                if (Objects.isNull(selectorData)) {
                    return handleSelectorIfNull(pluginName, exchange, chain);
                }
            }
        }
        printLog(selectorData, pluginName);
        if (!selectorData.getContinued()) {
            // if continued， not match rules
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
        // lru map as L1 cache,the cache is enabled by default.
        // if the L1 cache fails to hit, using L2 cache based on trie cache.
        // if the L2 cache fails to hit, execute default strategy.
        RuleData ruleData = obtainRuleDataCacheIfEnabled(path);
        if (Objects.nonNull(ruleData) && Objects.isNull(ruleData.getId())) {
            return handleRuleIfNull(pluginName, exchange, chain);
        }
        if (Objects.isNull(ruleData)) {
            // L1 cache not exist data, try to get data through trie cache
            ruleData = trieMatchRule(exchange, selectorData, path);
            // trie cache fails to hit, execute default strategy
            if (Objects.isNull(ruleData)) {
                ruleData = defaultMatchRule(exchange, rules, path);
                if (Objects.isNull(ruleData)) {
                    return handleRuleIfNull(pluginName, exchange, chain);
                }
            }
        }
        printLog(ruleData, pluginName);
        return doExecute(exchange, chain, selectorData, ruleData);
    }
    
    protected String getRawPath(final ServerWebExchange exchange) {
        return exchange.getRequest().getURI().getRawPath();
    }
    
    private void initCacheConfig() {
        if (Objects.isNull(selectorMatchConfig) || Objects.isNull(ruleMatchConfig)) {
            ShenyuConfig shenyuConfig = SpringBeanUtils.getInstance().getBean(ShenyuConfig.class);
            selectorMatchConfig = shenyuConfig.getSelectorMatchCache();
            ruleMatchConfig = shenyuConfig.getRuleMatchCache();
        }
        if (Objects.isNull(selectorTrie) || Objects.isNull(ruleTrie)) {
            selectorTrie = SpringBeanUtils.getInstance().getBean(TrieCacheTypeEnum.SELECTOR.getTrieType());
            ruleTrie = SpringBeanUtils.getInstance().getBean(TrieCacheTypeEnum.RULE.getTrieType());
        }
    }
    
    private SelectorData obtainSelectorDataCacheIfEnabled(final String path) {
        return selectorMatchConfig.getCache().getEnabled() ? MatchDataCache.getInstance().obtainSelectorData(named(), path) : null;
    }
    
    private RuleData obtainRuleDataCacheIfEnabled(final String path) {
        return ruleMatchConfig.getCache().getEnabled() ? MatchDataCache.getInstance().obtainRuleData(named(), path) : null;
    }

    private void cacheSelectorData(final String path, final SelectorData selectorData) {
        if (Boolean.FALSE.equals(selectorMatchConfig.getCache().getEnabled()) || Objects.isNull(selectorData)
                || Boolean.TRUE.equals(selectorData.getMatchRestful())) {
            return;
        }
        int initialCapacity = selectorMatchConfig.getCache().getInitialCapacity();
        long maximumSize = selectorMatchConfig.getCache().getMaximumSize();
        if (StringUtils.isBlank(selectorData.getId())) {
            MatchDataCache.getInstance().cacheSelectorData(path, selectorData, initialCapacity, maximumSize);
            return;
        }
        List<ConditionData> conditionList = selectorData.getConditionList();
        if (CollectionUtils.isNotEmpty(conditionList)) {
            boolean isUriCondition = conditionList.stream().allMatch(v -> URI_CONDITION_TYPE.equals(v.getParamType()));
            if (isUriCondition) {
                MatchDataCache.getInstance().cacheSelectorData(path, selectorData, initialCapacity, maximumSize);
            }
        }
    }
    
    private void cacheRuleData(final String path, final RuleData ruleData) {
        // if the ruleCache is disabled or rule data is null, not cache rule data.
        if (Boolean.FALSE.equals(ruleMatchConfig.getCache().getEnabled()) || Objects.isNull(ruleData)
                || Boolean.TRUE.equals(ruleData.getMatchRestful())) {
            return;
        }
        int initialCapacity = ruleMatchConfig.getCache().getInitialCapacity();
        long maximumSize = ruleMatchConfig.getCache().getMaximumSize();
        if (StringUtils.isBlank(ruleData.getId())) {
            MatchDataCache.getInstance().cacheRuleData(path, ruleData, initialCapacity, maximumSize);
            return;
        }
        List<ConditionData> conditionList = ruleData.getConditionDataList();
        if (CollectionUtils.isNotEmpty(conditionList)) {
            boolean isUriCondition = conditionList.stream().allMatch(v -> URI_CONDITION_TYPE.equals(v.getParamType()));
            if (isUriCondition) {
                MatchDataCache.getInstance().cacheRuleData(path, ruleData, initialCapacity, maximumSize);
            }
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
        List<SelectorData> filterCollectors = selectors.stream()
                .filter(selector -> selector.getEnabled() && filterSelector(selector, exchange))
                .distinct()
                .collect(Collectors.toList());
        if (filterCollectors.size() > 1) {
            return Pair.of(Boolean.FALSE, manyMatchSelector(filterCollectors));
        } else {
            return Pair.of(Boolean.TRUE, filterCollectors.stream().findFirst().orElse(null));
        }
    }

    private SelectorData manyMatchSelector(final List<SelectorData> filterCollectors) {
        //What needs to be dealt with here is the and condition. If the number of and conditions is the same and is matched at the same time,
        // it will be sorted by the sort field.
        Map<Integer, List<Pair<Integer, SelectorData>>> collect =
                filterCollectors.stream().map(selector -> {
                    boolean match = MatchModeEnum.match(selector.getMatchMode(), MatchModeEnum.AND);
                    int sort = 0;
                    if (match) {
                        sort = selector.getConditionList().size();
                    }
                    return Pair.of(sort, selector);
                }).collect(Collectors.groupingBy(Pair::getLeft));
        Integer max = Collections.max(collect.keySet());
        List<Pair<Integer, SelectorData>> pairs = collect.get(max);
        return pairs.stream().map(Pair::getRight).min(Comparator.comparing(SelectorData::getSort)).orElse(null);
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

    private Pair<Boolean, RuleData> matchRule(final ServerWebExchange exchange, final Collection<RuleData> rules) {
        List<RuleData> filterRuleData = rules.stream()
                .filter(rule -> filterRule(rule, exchange))
                .distinct()
                .collect(Collectors.toList());
        if (filterRuleData.size() > 1) {
            return Pair.of(Boolean.FALSE, manyMatchRule(filterRuleData));
        } else {
            return Pair.of(Boolean.TRUE, filterRuleData.stream().findFirst().orElse(null));
        }
    }

    private RuleData manyMatchRule(final List<RuleData> filterRuleData) {
        Map<Integer, List<Pair<Integer, RuleData>>> collect =
                filterRuleData.stream().map(rule -> {
                    boolean match = MatchModeEnum.match(rule.getMatchMode(), MatchModeEnum.AND);
                    int sort = 0;
                    if (match) {
                        sort = rule.getConditionDataList().size();
                    }
                    return Pair.of(sort, rule);
                }).collect(Collectors.groupingBy(Pair::getLeft));
        Integer max = Collections.max(collect.keySet());
        List<Pair<Integer, RuleData>> pairs = collect.get(max);
        return pairs.stream().map(Pair::getRight).min(Comparator.comparing(RuleData::getSort)).orElse(null);
    }

    private Boolean filterRule(final RuleData ruleData, final ServerWebExchange exchange) {
        return ruleData.getEnabled() && MatchStrategyFactory.match(ruleData.getMatchMode(), ruleData.getConditionDataList(), exchange);
    }
    
    private SelectorData trieMatchSelector(final ServerWebExchange exchange, final String pluginName, final String path) {
        if (!selectorMatchConfig.getTrie().getEnabled()) {
            return null;
        }
        SelectorData selectorData = null;
        ShenyuTrieNode shenyuTrieNode = selectorTrie.match(path, pluginName);
        if (Objects.nonNull(shenyuTrieNode)) {
            LogUtils.info(LOG, "{} selector match path from shenyu trie, path:{}", pluginName, path);
            List<?> collection = shenyuTrieNode.getPathCache().get(pluginName);
            if (CollectionUtils.isNotEmpty(collection)) {
                Pair<Boolean, SelectorData> selectorDataPair;
                if (collection.size() > 1) {
                    selectorDataPair = matchSelector(exchange, ListUtil.castList(collection, SelectorData.class::cast));
                } else {
                    Object selectorObj = collection.stream().findFirst().orElse(null);
                    SelectorData selector = Objects.nonNull(selectorObj) ? (SelectorData) selectorObj : null;
                    boolean cached = Objects.nonNull(selector) && selector.getConditionList().stream().allMatch(condition -> URI_CONDITION_TYPE.equals(condition.getParamType()));
                    selectorDataPair = Pair.of(cached, selector);
                }
                selectorData = selectorDataPair.getRight();
                if (selectorDataPair.getLeft() && Objects.nonNull(selectorData)) {
                    cacheSelectorData(path, selectorData);
                }
            }
        }
        return selectorData;
    }
    
    private RuleData trieMatchRule(final ServerWebExchange exchange, final SelectorData selectorData, final String path) {
        if (!ruleMatchConfig.getTrie().getEnabled()) {
            return null;
        }
        RuleData ruleData = null;
        ShenyuTrieNode shenyuTrieNode = ruleTrie.match(path, selectorData.getId());
        if (Objects.nonNull(shenyuTrieNode)) {
            LogUtils.info(LOG, "{} rule match path from shenyu trie", named());
            List<?> collection = shenyuTrieNode.getPathCache().get(selectorData.getId());
            if (CollectionUtils.isNotEmpty(collection)) {
                Pair<Boolean, RuleData> ruleDataPair;
                if (collection.size() > 1) {
                    ruleDataPair = matchRule(exchange, ListUtil.castList(collection, RuleData.class::cast));
                } else {
                    Object ruleObj = collection.stream().findFirst().orElse(null);
                    RuleData rule = Objects.nonNull(ruleObj) ? (RuleData) ruleObj : null;
                    boolean cached = Objects.nonNull(rule) && rule.getConditionDataList().stream().allMatch(condition -> URI_CONDITION_TYPE.equals(condition.getParamType()));
                    ruleDataPair = Pair.of(cached, rule);
                }
                ruleData = ruleDataPair.getRight();
                if (ruleDataPair.getLeft() && Objects.nonNull(ruleData)) {
                    // exist only one rule data, cache rule
                    cacheRuleData(path, ruleData);
                }
            }
        }
        return ruleData;
    }
    
    private SelectorData defaultMatchSelector(final ServerWebExchange exchange, final List<SelectorData> selectors, final String path) {
        Pair<Boolean, SelectorData> matchSelectorPair = matchSelector(exchange, selectors);
        SelectorData selectorData = matchSelectorPair.getRight();
        if (Objects.nonNull(selectorData)) {
            LogUtils.info(LOG, "{} selector match success from default strategy", named());
            // cache selector data
            if (matchSelectorPair.getLeft()) {
                cacheSelectorData(path, selectorData);
            }
            return selectorData;
        } else {
            // if not match selector, cache empty selector data.
            if (matchSelectorPair.getLeft()) {
                SelectorData emptySelectorData = SelectorData.builder().pluginName(named()).build();
                cacheSelectorData(path, emptySelectorData);
            }
            return null;
        }
    }
    
    private RuleData defaultMatchRule(final ServerWebExchange exchange, final List<RuleData> rules, final String path) {
        Pair<Boolean, RuleData> matchRulePair = matchRule(exchange, rules);
        RuleData ruleData = matchRulePair.getRight();
        if (Objects.nonNull(ruleData)) {
            LOG.info("{} rule match path from default strategy", named());
            // cache rule data
            if (matchRulePair.getLeft()) {
                cacheRuleData(path, ruleData);
            }
            return ruleData;
        } else {
            // if not match rule, cache empty rule data.
            if (matchRulePair.getLeft()) {
                RuleData emptyRuleData = RuleData.builder().pluginName(named()).build();
                cacheRuleData(path, emptyRuleData);
            }
            return null;
        }
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
    
}
