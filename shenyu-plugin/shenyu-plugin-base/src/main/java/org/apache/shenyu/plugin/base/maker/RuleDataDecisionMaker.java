package org.apache.shenyu.plugin.base.maker;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.utils.ListUtil;
import org.apache.shenyu.common.utils.LogUtils;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.provider.RuleDataProvider;
import org.apache.shenyu.plugin.base.trie.ShenyuTrie;
import org.apache.shenyu.plugin.base.trie.ShenyuTrieNode;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Objects;

public class RuleDataDecisionMaker extends AbstractMatchDecisionMaker<RuleData> {
    public RuleDataDecisionMaker() {
        super(new RuleDataProvider());
    }

    @Override
    public Mono<Void> handleEmpty(String pluginName, ServerWebExchange exchange, ShenyuPluginChain chain) {
        return chain.execute(exchange);
    }

    @Override
    public RuleData matchData(ServerWebExchange exchange, String ruleName, List<RuleData> dataList, String path, ShenyuConfig.SelectorMatchCache selectorMatchConfig, ShenyuTrie selectorTrie) {
        return dataList.isEmpty() ? trieMatchRule() : dataList.get(0);
    }

    @Override
    public boolean shouldContinue(RuleData data) {
        return data != null && data.getEnabled();
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
}