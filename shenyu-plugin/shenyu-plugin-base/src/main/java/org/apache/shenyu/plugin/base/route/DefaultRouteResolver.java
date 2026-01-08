package org.apache.shenyu.plugin.base.route;

import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.base.cache.SmartCacheManager;
import org.apache.shenyu.plugin.base.condition.strategy.MatchStrategyFactory;
import org.apache.shenyu.plugin.base.context.PluginExecutionContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Default implementation of RouteResolver.
 *
 * This implementation provides caching-aware route resolution with support for:
 * - Smart caching of selector and rule matching results
 * - Conditional matching based on request attributes
 * - Full compatibility with existing ShenYu matching logic
 */
@Component
public class DefaultRouteResolver implements RouteResolver {

    private static final Logger LOG = LoggerFactory.getLogger(DefaultRouteResolver.class);

    private final SmartCacheManager cacheManager;

    public DefaultRouteResolver(SmartCacheManager cacheManager) {
        this.cacheManager = cacheManager;
    }

    @Override
    public Mono<Optional<SelectorData>> matchSelector(
            PluginExecutionContext context,
            List<SelectorData> selectors) {

        if (selectors == null || selectors.isEmpty()) {
            return Mono.just(Optional.empty());
        }

        // Try cache first
        String cacheKey = buildSelectorCacheKey(context);

        return cacheManager.getOrLoad(
                SmartCacheManager.CacheTypes.SELECTOR,
                cacheKey,
                Mono.fromCallable(() -> performSelectorMatch(context, selectors))
        ).map(Optional::ofNullable);
    }

    @Override
    public Mono<Optional<RuleData>> matchRule(
            PluginExecutionContext context,
            SelectorData selector,
            List<RuleData> rules) {

        if (selector == null || rules == null || rules.isEmpty()) {
            return Mono.just(Optional.empty());
        }

        // Try cache first
        String cacheKey = buildRuleCacheKey(context, selector);

        return cacheManager.getOrLoad(
                SmartCacheManager.CacheTypes.RULE,
                cacheKey,
                Mono.fromCallable(() -> performRuleMatch(context, rules))
        ).map(Optional::ofNullable);
    }

    @Override
    public Mono<Boolean> isPluginApplicable(PluginExecutionContext context, String pluginName) {
        return Mono.fromCallable(() -> {
            PluginData pluginData = BaseDataCache.getInstance().obtainPluginData(pluginName);
            return Objects.nonNull(pluginData) && pluginData.getEnabled();
        });
    }

    @Override
    public Mono<RouteResult> resolveRoute(PluginExecutionContext context, String pluginName) {
        return isPluginApplicable(context, pluginName)
                .filter(applicable -> applicable)
                .flatMap(applicable -> {
                    // Get selectors for this plugin
                    List<SelectorData> selectors = BaseDataCache.getInstance()
                            .obtainSelectorData(pluginName);

                    return matchSelector(context, selectors)
                            .flatMap(selectorOpt -> {
                                if (!selectorOpt.isPresent()) {
                                    return Mono.just(RouteResult.noMatch());
                                }

                                SelectorData selector = selectorOpt.get();

                                // Check if selector continues to rules
                                if (!selector.getContinued()) {
                                    return Mono.just(RouteResult.selectorOnly(
                                            selector, RouteResult.MatchType.CACHE_HIT));
                                }

                                // Get and match rules
                                List<RuleData> rules = BaseDataCache.getInstance()
                                        .obtainRuleData(selector.getId());

                                return matchRule(context, selector, rules)
                                        .map(ruleOpt -> {
                                            if (ruleOpt.isPresent()) {
                                                return RouteResult.success(
                                                        selector,
                                                        ruleOpt.get(),
                                                        RouteResult.MatchType.CACHE_HIT
                                                );
                                            }
                                            return RouteResult.selectorOnly(
                                                    selector, RouteResult.MatchType.CACHE_HIT);
                                        });
                            });
                })
                .defaultIfEmpty(RouteResult.noMatch());
    }

    /**
     * Perform actual selector matching logic.
     */
    private SelectorData performSelectorMatch(PluginExecutionContext context, List<SelectorData> selectors) {
        for (SelectorData selector : selectors) {
            if (selector.getEnabled() && matchSelector(context, selector)) {
                LOG.debug("Matched selector: {}", selector.getName());
                return selector;
            }
        }
        return null;
    }

    /**
     * Perform actual rule matching logic.
     */
    private RuleData performRuleMatch(PluginExecutionContext context, List<RuleData> rules) {
        for (RuleData rule : rules) {
            if (rule.getEnabled() && matchRule(context, rule)) {
                LOG.debug("Matched rule: {}", rule.getName());
                return rule;
            }
        }
        return null;
    }

    /**
     * Match selector against context.
     */
    private boolean matchSelector(PluginExecutionContext context, SelectorData selector) {
        if (selector.getConditionList() == null || selector.getConditionList().isEmpty()) {
            return true;
        }
        return MatchStrategyFactory.match(
                selector.getMatchMode(),
                selector.getConditionList(),
                context.getExchange()
        );
    }

    /**
     * Match rule against context.
     */
    private boolean matchRule(PluginExecutionContext context, RuleData rule) {
        if (rule.getConditionDataList() == null || rule.getConditionDataList().isEmpty()) {
            return true;
        }
        return MatchStrategyFactory.match(
                rule.getMatchMode(),
                rule.getConditionDataList(),
                context.getExchange()
        );
    }

    /**
     * Build cache key for selector matching.
     */
    private String buildSelectorCacheKey(PluginExecutionContext context) {
        return context.getRequestPath() + ":" + context.getMethod();
    }

    /**
     * Build cache key for rule matching.
     */
    private String buildRuleCacheKey(PluginExecutionContext context, SelectorData selector) {
        return selector.getId() + ":" + context.getRequestPath() + ":" + context.getMethod();
    }
}
