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
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.base.maker.PluginDataDecisionMaker;
import org.apache.shenyu.plugin.base.maker.RuleDataDecisionMaker;
import org.apache.shenyu.plugin.base.maker.SelectorDataDecisionMaker;
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

//    private static final String URI_CONDITION_TYPE = "uri";



    private ShenyuTrie ruleTrie;

    private ShenyuConfig.RuleMatchCache ruleMatchConfig;

    private SelectorDataDecisionMaker selectorDataDecisionMaker = new SelectorDataDecisionMaker();

    private RuleDataDecisionMaker ruleDataDecisionMaker = new RuleDataDecisionMaker();

    private PluginDataDecisionMaker pluginDataDecisionMaker = new PluginDataDecisionMaker();

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
        final String path = getRawPath(exchange);


        List<PluginData> pluginDataList = pluginDataDecisionMaker.getData(pluginName);
        if (CollectionUtils.isEmpty(pluginDataList) || !pluginDataDecisionMaker.shouldContinue(pluginDataList.get(0))) {
            return pluginDataDecisionMaker.handleEmpty(pluginName, exchange, chain);
        }

        List<SelectorData> selectorDataList = selectorDataDecisionMaker.getData(pluginName);
        if (CollectionUtils.isEmpty(selectorDataList)) {
            return selectorDataDecisionMaker.handleEmpty(pluginName, exchange, chain);
        }

        SelectorData selectorData = selectorDataDecisionMaker.matchData(exchange,pluginName, selectorDataList, path, selectorMatchConfig, selectorTrie);
        if (selectorData == null) {
            return selectorDataDecisionMaker.handleEmpty(pluginName, exchange, chain);
        }

        printLog(selectorData, pluginName);
        if (!selectorDataDecisionMaker.shouldContinue(selectorData)) {
            return doExecute(exchange, chain, selectorData, defaultRuleData(selectorData));
        }



        List<RuleData> ruleDataList = ruleDataDecisionMaker.getData(selectorData.getId());
        if (CollectionUtils.isEmpty(ruleDataList)) {
            return ruleDataDecisionMaker.handleEmpty(pluginName, exchange, chain);
        }

        if (selectorData.getType() == SelectorTypeEnum.FULL_FLOW.getCode()) {
            RuleData rule = ruleDataList.get(ruleDataList.size() - 1);
            printLog(rule, pluginName);
            return doExecute(exchange, chain, selectorData, rule);
        }

        RuleData ruleData = ruleDataDecisionMaker.matchData(exchange, ruleDataList, path);
        if (ruleData == null) {
            return ruleDataDecisionMaker.handleEmpty(pluginName, exchange, chain);
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
