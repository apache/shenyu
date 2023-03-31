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

import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.MatchModeEnum;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
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

/**
 * abstract shenyu plugin please extends.
 */
public abstract class AbstractShenyuPlugin implements ShenyuPlugin {

    private static final Logger LOG = LoggerFactory.getLogger(AbstractShenyuPlugin.class);

    private static final String URI_CONDITION_TYPE = "uri";

    private ShenyuConfig.MatchCache matchCacheConfig;
    
    private ShenyuTrie trie;
    
    private ShenyuConfig.ShenyuTrieConfig trieConfig;

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
        initCacheConfig();
        final String pluginName = named();
        PluginData pluginData = BaseDataCache.getInstance().obtainPluginData(pluginName);
        // early exit
        if (Objects.isNull(pluginData) || !pluginData.getEnabled()) {
            return chain.execute(exchange);
        }
        final String path = exchange.getRequest().getURI().getPath();
        List<SelectorData> selectors = BaseDataCache.getInstance().obtainSelectorData(pluginName);
        SelectorData selectorData = obtainSelectorDataCacheIfEnabled(path);
        // handle Selector
        if (Objects.nonNull(selectorData) && StringUtils.isBlank(selectorData.getId())) {
            return handleSelectorIfNull(pluginName, exchange, chain);
        }
        if (Objects.isNull(selectorData)) {
            if (CollectionUtils.isEmpty(selectors)) {
                return handleSelectorIfNull(pluginName, exchange, chain);
            }
            Pair<Boolean, SelectorData> matchSelectorData = matchSelector(exchange, selectors);
            selectorData = matchSelectorData.getRight();
            if (Objects.isNull(selectorData)) {
                if (matchCacheConfig.getSelector().getSelectorEnabled() && matchSelectorData.getLeft()) {
                    selectorData = new SelectorData();
                    selectorData.setPluginName(pluginName);
                    cacheSelectorData(path, selectorData);
                }
                return handleSelectorIfNull(pluginName, exchange, chain);
            } else {
                if (matchCacheConfig.getSelector().getSelectorEnabled() && matchSelectorData.getLeft()) {
                    cacheSelectorData(path, selectorData);
                }
            }
        }
        printLog(selectorData, pluginName);
        if (Objects.nonNull(selectorData.getContinued()) && !selectorData.getContinued()) {
            // if continued， not match rules
            return doExecute(exchange, chain, selectorData, defaultRuleData(selectorData));
        }
        List<RuleData> rules = BaseDataCache.getInstance().obtainRuleData(selectorData.getId());
        if (CollectionUtils.isEmpty(rules)) {
            return handleRuleIfNull(pluginName, exchange, chain);
        }
        RuleData ruleData = obtainRuleDataCache(path);
        if (Objects.nonNull(ruleData) && Objects.isNull(ruleData.getId())) {
            return handleRuleIfNull(pluginName, exchange, chain);
        }
        if (selectorData.getType() == SelectorTypeEnum.FULL_FLOW.getCode()) {
            //get last
            RuleData rule = rules.get(rules.size() - 1);
            printLog(rule, pluginName);
            return doExecute(exchange, chain, selectorData, rule);
        } else {
            // lru map as L1 cache,the cache is enabled by default.
            // if the L1 cache fails to hit, using L2 cache based on trie cache.
            // if the L2 cache fails to hit, execute default strategy.
            if (Objects.isNull(ruleData)) {
                // L1 cache not exist data, try to get data through trie cache
                ruleData = trieMatchRule(exchange, selectorData, path);
                // trie cache fails to hit, execute default strategy
                if (Objects.isNull(ruleData)) {
                    LOG.info("{} rule match path from default strategy", named());
                    Pair<Boolean, RuleData> matchRuleData = matchRule(exchange, rules);
                    ruleData = matchRuleData.getRight();
                    if (matchRuleData.getLeft()) {
                        ruleData = Optional.ofNullable(ruleData)
                                .orElse(RuleData.builder().pluginName(pluginName).matchRestful(false).build());
                        cacheRuleData(path, ruleData);
                    }
                }
            }
        }
        if (Objects.isNull(ruleData) || Objects.isNull(ruleData.getId())) {
            return handleRuleIfNull(pluginName, exchange, chain);
        }
        printLog(ruleData, pluginName);
        return doExecute(exchange, chain, selectorData, ruleData);
    }

    private void initCacheConfig() {
        if (Objects.isNull(matchCacheConfig)) {
            matchCacheConfig = SpringBeanUtils.getInstance().getBean(ShenyuConfig.class).getMatchCache();
        }
        if (Objects.isNull(trie)) {
            trie = SpringBeanUtils.getInstance().getBean(ShenyuTrie.class);
        }
        if (Objects.isNull(trieConfig)) {
            trieConfig = SpringBeanUtils.getInstance().getBean(ShenyuConfig.class).getTrie();
        }
    }

    private void cacheSelectorData(final String path, final SelectorData selectorData) {
        if (Objects.isNull(selectorData)) {
            return;
        }
        if (Objects.isNull(selectorData.getMatchRestful())
                || (Objects.nonNull(selectorData.getMatchRestful()) && selectorData.getMatchRestful())) {
            return;
        }
        int initialCapacity = matchCacheConfig.getSelector().getInitialCapacity();
        long maximumSize = matchCacheConfig.getSelector().getMaximumSize();
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

    private SelectorData obtainSelectorDataCacheIfEnabled(final String path) {
        return matchCacheConfig.getSelector().getSelectorEnabled() ? MatchDataCache.getInstance().obtainSelectorData(named(), path) : null;
    }

    protected RuleData defaultRuleData(final SelectorData selectorData) {
        RuleData ruleData = new RuleData();
        ruleData.setSelectorId(selectorData.getId());
        ruleData.setPluginName(selectorData.getPluginName());
        return ruleData;
    }

    protected Mono<Void> handleSelectorIfNull(final String pluginName, final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        return chain.execute(exchange);
    }

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
    
    private RuleData obtainRuleDataCache(final String path) {
        return MatchDataCache.getInstance().obtainRuleData(named(), path);
    }
    
    private void cacheRuleData(final String path, final RuleData ruleData) {
        if (Objects.isNull(ruleData)) {
            return;
        }
        // if the field of matchRestful is true, not cache rule data. the field is false, cache rule data.
        if (Objects.isNull(ruleData.getMatchRestful())
                || (Objects.nonNull(ruleData.getMatchRestful()) && ruleData.getMatchRestful())) {
            return;
        }
        int initialCapacity = matchCacheConfig.getRule().getInitialCapacity();
        long maximumSize = matchCacheConfig.getRule().getMaximumSize();
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
    
    private RuleData trieMatchRule(final ServerWebExchange exchange, final SelectorData selectorData, final String path) {
        if (!trieConfig.getEnabled()) {
            return null;
        }
        RuleData ruleData = null;
        ShenyuTrieNode shenyuTrieNode = trie.match(path, selectorData.getId());
        if (Objects.nonNull(shenyuTrieNode)) {
            LOG.info("{} rule match path from shenyu trie", named());
            List<RuleData> ruleDataList = shenyuTrieNode.getPathRuleCache().get(selectorData.getId());
            if (CollectionUtils.isNotEmpty(ruleDataList)) {
                Pair<Boolean, RuleData> ruleDataPair;
                if (ruleDataList.size() > 1) {
                    ruleDataPair = matchRule(exchange, ruleDataList);
                } else {
                    ruleDataPair = Pair.of(Boolean.TRUE, ruleDataList.stream().findFirst().orElse(null));
                }
                ruleData = ruleDataPair.getRight();
                if (ruleDataPair.getLeft()) {
                    // exist only one rule data, cache rule
                    cacheRuleData(path, ruleData);
                }
            }
        }
        return ruleData;
    }

    private void printLog(final Object data, final String pluginName) {
        if (data instanceof SelectorData) {
            SelectorData selector = (SelectorData) data;
            if (selector.getLogged()) {
                LOG.info("{} selector success match , selector name :{}", pluginName, selector.getName());
            }
        }
        if (data instanceof RuleData) {
            RuleData rule = (RuleData) data;
            if (rule.getLoged()) {
                LOG.info("{} rule success match , rule name :{}", pluginName, rule.getName());
            }
        }
    }
}
