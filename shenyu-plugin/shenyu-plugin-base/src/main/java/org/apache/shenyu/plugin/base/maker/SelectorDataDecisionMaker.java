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

import java.util.*;
import java.util.stream.Collectors;

import static org.apache.shenyu.plugin.api.ShenyuPlugin.LOG;

public class SelectorDataDecisionMaker extends AbstractMatchDecisionMaker<SelectorData> {

    private static final String URI_CONDITION_TYPE = "uri";

    private ShenyuConfig.SelectorMatchCache selectorMatchConfig;

    private ShenyuTrie selectorTrie;

    public SelectorDataDecisionMaker() {
        super(new SelectorDataProvider());
    }

    @Override
    public Mono<Void> handleEmpty(String pluginName, ServerWebExchange exchange, ShenyuPluginChain chain) {
        return chain.execute(exchange);
    }

    @Override
    public SelectorData matchData(ServerWebExchange exchange,String pluginName, List<SelectorData> dataList, String path,ShenyuConfig.SelectorMatchCache selectorMatchConfig,ShenyuTrie selectorTrie) {
        return dataList.isEmpty() ? trieMatchSelector(exchange, pluginName, path, selectorMatchConfig, selectorTrie) : dataList.get(0);
    }

    @Override
    public boolean shouldContinue(SelectorData data) {
        return data != null && data.getEnabled() && data.getContinued();
    }

    private void initCacheConfig() {
        if (Objects.isNull(selectorMatchConfig)) {
            ShenyuConfig shenyuConfig = SpringBeanUtils.getInstance().getBean(ShenyuConfig.class);
            selectorMatchConfig = shenyuConfig.getSelectorMatchCache();
        }

        if (Objects.isNull(selectorTrie)) {
            selectorTrie = SpringBeanUtils.getInstance().getBean(TrieCacheTypeEnum.SELECTOR.getTrieType());
        }
    }


    private SelectorData trieMatchSelector(final ServerWebExchange exchange, final String pluginName, final String path, ShenyuConfig.SelectorMatchCache selectorMatchConfig, ShenyuTrie selectorTrie) {
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
                    cacheSelectorData(path, selectorData,selectorMatchConfig);
                }
            }
        }
        return selectorData;
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


    private Boolean filterSelector(final SelectorData selector, final ServerWebExchange exchange) {
        if (selector.getType() == SelectorTypeEnum.CUSTOM_FLOW.getCode()) {
            if (CollectionUtils.isEmpty(selector.getConditionList())) {
                return false;
            }
            return MatchStrategyFactory.match(selector.getMatchMode(), selector.getConditionList(), exchange);
        }
        return true;
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

    private void cacheSelectorData(final String path, final SelectorData selectorData,ShenyuConfig.SelectorMatchCache selectorMatchConfig) {
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