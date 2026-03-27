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

package org.apache.shenyu.plugin.base.maker;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.MatchModeEnum;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.common.enums.TrieCacheTypeEnum;
import org.apache.shenyu.common.utils.ListUtil;
import org.apache.shenyu.common.utils.LogUtils;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.cache.MatchDataCache;
import org.apache.shenyu.plugin.base.condition.strategy.MatchStrategyFactory;
import org.apache.shenyu.plugin.base.provider.SelectorDataProvider;
import org.apache.shenyu.plugin.base.trie.ShenyuTrie;
import org.apache.shenyu.plugin.base.trie.ShenyuTrieNode;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import static org.apache.shenyu.plugin.api.ShenyuPlugin.LOG;

/**
 * The core decision-making engine for selector matching in Apache ShenYu gateway.
 *
 * <p>This class implements the matching logic that determines which selector should handle
 * incoming requests based on request attributes, path patterns, and configured conditions.
 * It extends the abstract matching framework and specializes in selector-level routing decisions.</p>
 *
 * <p>In the ShenYu architecture, selectors represent the first level of request routing where
 * coarse-grained filtering occurs before more specific rule matching. This decision maker
 * evaluates various matching strategies including trie-based path matching, condition-based
 * filtering, and custom flow selectors.</p>
 *
 * <p>The class employs multiple matching techniques:
 * <ul>
 *   <li><strong>Trie-based matching</strong>: For efficient path pattern matching using prefix trees</li>
 *   <li><strong>Condition-based filtering</strong>: Using match strategies from {@link MatchStrategyFactory}</li>
 *   <li><strong>Custom flow selectors</strong>: For advanced, user-defined routing logic</li>
 * </ul>
 * </p>
 *
 * <p>Caching is extensively used to optimize performance, with configurable cache settings
 * for both trie structures and match results. The caching behavior can be tuned through
 * {@link ShenyuConfig.SelectorMatchCache} configuration.</p>
 */
public class SelectorDataDecisionMaker extends AbstractMatchDecisionMaker<SelectorData> {

    private ShenyuConfig.SelectorMatchCache selectorMatchConfig;

    private ShenyuTrie selectorTrie;

    /**
     * Constructs a new SelectorDataDecisionMaker with a SelectorDataProvider.
     * Initializes cache configuration and trie structures for selector matching.
     */
    public SelectorDataDecisionMaker() {
        super(new SelectorDataProvider());
        initCacheConfig();
    }

    /**
     * Handles the case when no selector data is found for the given plugin.
     *
     * <p>When no matching selectors are found, this method allows the request to continue
     * through the plugin chain without selector-specific processing. This ensures that
     * requests can still be processed by subsequent plugins even when selector matching
     * doesn't yield results.</p>
     *
     * @param pluginName the name of the plugin being executed
     * @param exchange the current server web exchange
     * @param chain the plugin chain for continued processing
     * @return a Mono indicating completion of the empty handling operation
     */
    @Override
    public Mono<Void> handleEmpty(final String pluginName, final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        return chain.execute(exchange);
    }

    /**
     * Performs the core selector matching logic for incoming requests.
     *
     * <p>This method implements the selector matching algorithm that determines which
     * selector should process the current request. It supports both direct list-based
     * matching and advanced trie-based matching for path patterns.</p>
     *
     * <p>The matching process follows this hierarchy:
     * <ol>
     *   <li>If pre-filtered data is available in the dataList, use the first match</li>
     *   <li>Otherwise, perform trie-based matching on the request path</li>
     *   <li>Apply condition filtering for custom flow selectors</li>
     *   <li>Handle multiple matches using priority-based selection</li>
     * </ol>
     * </p>
     *
     * @param exchange the current server web exchange containing request information
     * @param pluginName the name of the plugin requesting selector matching
     * @param dataList pre-filtered list of selector data (may be empty)
     * @param path the request path used for matching
     * @param selectorData additional selector context (currently unused in base implementation)
     * @return the matched SelectorData, or null if no match found
     *
     */
    @Override
    public SelectorData matchData(final ServerWebExchange exchange, final String pluginName, final List<SelectorData> dataList, final String path, final SelectorData selectorData) {
        return dataList.isEmpty() ? trieMatchSelector(exchange, pluginName, path) : dataList.get(0);
    }

    /**
     * Determines whether the plugin chain should continue processing after selector matching.
     *
     * <p>A selector can control the flow of the plugin chain by specifying continuation
     * behavior. This method checks the selector's enabled status and continuation flag
     * to determine if processing should proceed to the next plugin.</p>
     *
     * @param data the selector data to evaluate for continuation
     * @return true if the selector is enabled and configured to continue processing,
     *         false otherwise
     */
    @Override
    public boolean shouldContinue(final SelectorData data) {
        return data.getEnabled() && data.getContinued();
    }

    /**
     * Initializes the cache configuration and trie structures for selector matching.
     *
     * <p>This method lazily initializes the caching infrastructure by retrieving
     * configuration from Spring context. It ensures that cache settings and trie
     * instances are properly configured before matching operations begin.</p>
     */
    private void initCacheConfig() {
        if (Objects.isNull(selectorMatchConfig)) {
            ShenyuConfig shenyuConfig = SpringBeanUtils.getInstance().getBean(ShenyuConfig.class);
            selectorMatchConfig = shenyuConfig.getSelectorMatchCache();
        }

        if (Objects.isNull(selectorTrie)) {
            selectorTrie = SpringBeanUtils.getInstance().getBean(TrieCacheTypeEnum.SELECTOR.getTrieType());
        }
    }

    /**
     * Performs trie-based matching for selectors when no pre-filtered data is available.
     *
     * <p>This method implements efficient path matching using a trie data structure,
     * which provides O(m) time complexity where m is the path length. It's particularly
     * effective for routing scenarios with large numbers of path-based selectors.</p>
     *
     * <p>The matching process includes:
     * <ul>
     *   <li>Checking if trie matching is enabled in configuration</li>
     *   <li>Finding the matching trie node for the given path</li>
     *   <li>Handling single and multiple matches appropriately</li>
     *   <li>Caching results for future requests when conditions allow</li>
     * </ul>
     * </p>
     *
     * @param exchange the current server web exchange
     * @param pluginName the plugin name for context-specific matching
     * @param path the request path to match against selector patterns
     * @return the matched SelectorData, or null if no match found
     */
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
                    cacheSelectorData(path, selectorData, selectorMatchConfig);
                }
            }
        }
        return selectorData;
    }

    /**
     * Filters and matches a collection of selectors against the current exchange.
     *
     * <p>This method applies condition-based filtering to determine which selectors
     * are appropriate for the current request. It handles both single and multiple
     * matches, with special logic for resolving conflicts when multiple selectors
     * match the same request.</p>
     *
     * @param exchange the server web exchange to match against
     * @param selectors the collection of selectors to filter
     * @return a Pair containing a Boolean indicating if the result can be cached,
     *         and the matched SelectorData
     */
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

    /**
     * Filters individual selectors based on their type and conditions.
     *
     * <p>Custom flow selectors undergo comprehensive condition matching using the
     * configured match strategy, while other selector types are accepted by default
     * assuming they've passed earlier filtering stages.</p>
     *
     * @param selector the selector to evaluate
     * @param exchange the server web exchange for condition evaluation
     * @return true if the selector should be considered for matching, false otherwise
     */
    private Boolean filterSelector(final SelectorData selector, final ServerWebExchange exchange) {
        if (selector.getType() == SelectorTypeEnum.CUSTOM_FLOW.getCode()) {
            if (CollectionUtils.isEmpty(selector.getConditionList())) {
                return false;
            }
            return MatchStrategyFactory.match(selector.getMatchMode(), selector.getConditionList(), exchange);
        }
        return true;
    }

    /**
     * Resolves conflicts when multiple selectors match the same request.
     *
     * <p>This method implements a priority-based selection algorithm that considers:
     * <ul>
     *   <li>Match mode (AND conditions are prioritized)</li>
     *   <li>Number of matching conditions</li>
     *   <li>Selector sort order for tie-breaking</li>
     * </ul>
     * </p>
     *
     * @param filterCollectors the list of matching selectors
     * @return the highest priority selector according to the conflict resolution rules
     */
    private SelectorData manyMatchSelector(final List<SelectorData> filterCollectors) {
        // What needs to be dealt with here is the and condition. If the number of and conditions is the same and is matched at the same time,
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

    /**
     * Caches selector matching results for performance optimization.
     *
     * <p>This method implements the caching strategy for selector matches, with
     * configurable cache size and eligibility criteria. Only selectors with
     * URI-based conditions are cached to ensure cache consistency.</p>
     *
     * @param path the request path used as cache key
     * @param selectorData the selector data to cache
     * @param selectorMatchConfig the cache configuration parameters
     */
    private void cacheSelectorData(final String path, final SelectorData selectorData, final ShenyuConfig.SelectorMatchCache selectorMatchConfig) {
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
}