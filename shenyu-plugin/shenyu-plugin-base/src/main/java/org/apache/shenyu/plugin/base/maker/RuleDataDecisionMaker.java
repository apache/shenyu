package org.apache.shenyu.plugin.base.maker;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.MatchModeEnum;
import org.apache.shenyu.common.enums.TrieCacheTypeEnum;
import org.apache.shenyu.common.utils.ListUtil;
import org.apache.shenyu.common.utils.LogUtils;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.cache.MatchDataCache;
import org.apache.shenyu.plugin.base.condition.strategy.MatchStrategyFactory;
import org.apache.shenyu.plugin.base.provider.RuleDataProvider;
import org.apache.shenyu.plugin.base.trie.ShenyuTrie;
import org.apache.shenyu.plugin.base.trie.ShenyuTrieNode;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.*;
import java.util.stream.Collectors;

import static org.apache.shenyu.plugin.api.ShenyuPlugin.LOG;

/**
 * A specialized decision maker implementation that handles rule matching logic within the Apache ShenYu gateway.
 *
 * <p>This class extends {@link AbstractMatchDecisionMaker} to provide sophisticated rule matching
 * capabilities for {@link RuleData} objects. It implements the core decision-making logic for determining
 * which specific rules should be applied to incoming requests based on path matching, conditions, and
 * configurable matching strategies.</p>
 *
 * <p>The RuleDataDecisionMaker operates as the second level in ShenYu's two-tier routing system:
 * <ol>
 *   <li><strong>Selector Level</strong>: Coarse-grained routing using {@link SelectorData}</li>
 *   <li><strong>Rule Level</strong>: Fine-grained processing logic using {@link RuleData}</li>
 * </ol>
 * After a selector matches a request, this component determines the specific rule to apply within that selector.
 * </p>
 *
 * <p>The decision maker employs multiple matching strategies:
 * <ul>
 *   <li><strong>Trie-based path matching</strong>: Efficient URI path lookup using {@link ShenyuTrie}</li>
 *   <li><strong>Condition evaluation</strong>: Rule condition validation using {@link MatchStrategyFactory}</li>
 *   <li><strong>Priority resolution</strong>: Conflict resolution when multiple rules match the same request</li>
 *   <li><strong>Configurable caching</strong>: Performance optimization through {@link MatchDataCache}</li>
 * </ul>
 * </p>
 *
 * <p>This component integrates with ShenYu's plugin architecture to provide precise rule-based
 * request processing, supporting complex routing scenarios with conditional logic and performance
 * optimization through intelligent caching mechanisms.</p>
 */
public class RuleDataDecisionMaker extends AbstractMatchDecisionMaker<RuleData> {

    private ShenyuConfig.RuleMatchCache ruleMatchConfig;
    private ShenyuTrie ruleTrie;

    /**
     * Constructs a new RuleDataDecisionMaker with a RuleDataProvider.
     *
     * <p>Initializes the cache configuration and trie data structure specifically optimized
     * for rule matching operations. The constructor sets up the necessary infrastructure
     * for efficient rule lookup and matching performance.</p>
     */
    public RuleDataDecisionMaker() {
        super(new RuleDataProvider());
        initCacheConfig();
    }

    /**
     * Handles the scenario when no rule data is found for the given rule name.
     *
     * <p>When no matching rules are found, this method allows the request to continue
     * through the plugin chain without rule-specific processing. This ensures that
     * the gateway can handle requests even when no specific rules are configured,
     * maintaining system reliability and avoiding request processing interruptions.</p>
     *
     * @param pluginName the name of the plugin being executed
     * @param exchange the current server web exchange containing request and response data
     * @param chain the plugin chain for continued request processing
     * @return a Mono indicating completion of the empty handler operation
     */
    @Override
    public Mono<Void> handleEmpty(String pluginName, ServerWebExchange exchange, ShenyuPluginChain chain) {
        return chain.execute(exchange);
    }

    /**
     * Matches rule data against the current request context.
     *
     * <p>This method implements the core rule matching logic, providing two matching strategies:
     * <ol>
     *   <li><strong>Direct matching</strong>: Uses pre-loaded rule data from the provided list when available</li>
     *   <li><strong>Trie-based matching</strong>: Falls back to sophisticated trie-based path matching
     *       when the data list is empty</li>
     * </ol>
     * The method ensures optimal performance by prioritizing cached data while maintaining
     * comprehensive matching capabilities through the trie structure.
     * </p>
     *
     * @param exchange the current server web exchange containing request information
     * @param ruleName the name of the rule requesting matching
     * @param dataList the list of candidate rule data for matching (may be empty)
     * @param path the request path to match against rules
     * @param selectorData the parent selector data context for rule matching
     * @return the matched RuleData, or null if no suitable rule is found
     */
    @Override
    public RuleData matchData(ServerWebExchange exchange, String ruleName, List<RuleData> dataList, String path, SelectorData selectorData) {
        return dataList.isEmpty() ? trieMatchRule(exchange, ruleName, selectorData, path) : dataList.get(0);
    }

    /**
     * Determines whether the plugin chain should continue processing after the current rule.
     *
     * <p>Rule continuation is based on the rule's enabled status. Only enabled rules
     * permit further processing in the plugin chain. This allows for complex rule
     * sequences where specific rules can terminate processing while others allow
     * continuation to subsequent rules or plugins.</p>
     *
     * @param data the rule data to evaluate for continuation
     * @return true if the rule is enabled and should continue processing, false otherwise
     */
    @Override
    public boolean shouldContinue(RuleData data) {
        return data != null && data.getEnabled();
    }

    /**
     * Initializes the cache configuration and trie data structure for rule matching.
     *
     * <p>This method performs lazy initialization of the rule matching infrastructure,
     * retrieving configuration from ShenYu's global configuration and obtaining the
     * appropriate trie implementation based on the rule cache type enumeration.</p>
     */
    private void initCacheConfig() {
        if (Objects.isNull(ruleMatchConfig)) {
            ShenyuConfig shenyuConfig = SpringBeanUtils.getInstance().getBean(ShenyuConfig.class);
            ruleMatchConfig = shenyuConfig.getRuleMatchCache();
        }

        if (Objects.isNull(ruleTrie)) {
            ruleTrie = SpringBeanUtils.getInstance().getBean(TrieCacheTypeEnum.RULE.getTrieType());
        }
    }

    /**
     * Performs trie-based matching for rules when no pre-loaded data is available.
     *
     * <p>This method implements sophisticated trie-based matching that:
     * <ul>
     *   <li>Validates that trie matching is enabled in the configuration</li>
     *   <li>Uses the ShenyuTrie to find path matches based on selector context</li>
     *   <li>Handles both single and multiple rule matches with appropriate resolution</li>
     *   <li>Applies caching for performance optimization when conditions permit</li>
     * </ul>
     * </p>
     *
     * @param exchange the current server web exchange
     * @param ruleName the name of the rule being processed
     * @param selectorData the parent selector data providing context for rule matching
     * @param path the request path to match
     * @return the matched RuleData, or null if no match is found or trie matching is disabled
     */
    private RuleData trieMatchRule(final ServerWebExchange exchange, String ruleName, final SelectorData selectorData, final String path) {
        if (!ruleMatchConfig.getTrie().getEnabled()) {
            return null;
        }
        RuleData ruleData = null;
        ShenyuTrieNode shenyuTrieNode = ruleTrie.match(path, selectorData.getId());
        if (Objects.nonNull(shenyuTrieNode)) {
            LogUtils.info(LOG, "{} rule match path from shenyu trie", ruleName);
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

    /**
     * Filters and matches a collection of rules against the current exchange.
     *
     * <p>This method applies rule conditions and matching strategies to determine
     * which rules are appropriate for the current request. It handles conflict
     * resolution when multiple rules match and provides caching recommendations
     * based on the matching results.</p>
     *
     * @param exchange the current server web exchange
     * @param rules the collection of rules to filter and match
     * @return a Pair where the left value indicates cacheability and the right value contains the matched RuleData
     */
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

    /**
     * Filters a single rule based on its enabled status and conditions.
     *
     * <p>This method validates that a rule is enabled and applies the appropriate
     * match strategy using the MatchStrategyFactory. It serves as the core
     * condition evaluation mechanism for rule matching.</p>
     *
     * @param ruleData the rule to filter
     * @param exchange the current server web exchange for condition evaluation
     * @return true if the rule is enabled and matches the current request conditions, false otherwise
     */
    private Boolean filterRule(final RuleData ruleData, final ServerWebExchange exchange) {
        return ruleData.getEnabled() && MatchStrategyFactory.match(ruleData.getMatchMode(), ruleData.getConditionDataList(), exchange);
    }

    /**
     * Resolves conflicts when multiple rules match the same request.
     *
     * <p>This method implements sophisticated priority resolution logic that:
     * <ol>
     *   <li>Groups rules by the number of AND conditions matched</li>
     *   <li>Selects the group with the highest number of AND conditions</li>
     *   <li>Within that group, chooses the rule with the lowest sort value</li>
     * </ol>
     * This ensures that more specific rules (with more conditions) take precedence
     * over general ones, providing predictable and configurable rule prioritization.
     * </p>
     *
     * @param filterRuleData the list of matching rules to resolve
     * @return the highest priority rule according to the resolution rules
     */
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

    /**
     * Caches matched rule data for improved performance on subsequent requests.
     *
     * <p>This method implements configurable caching that stores rule data when:
     * <ul>
     *   <li>Caching is enabled in the configuration</li>
     *   <li>The rule data is not null</li>
     *   <li>The rule doesn't represent a RESTful match</li>
     *   <li>All conditions are URI-based (ensuring cache validity)</li>
     * </ul>
     * The caching mechanism respects the configured initial capacity and maximum
     * size parameters to optimize memory usage while maintaining performance.
     * </p>
     *
     * @param path the request path used as cache key
     * @param ruleData the rule data to cache
     */
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
}