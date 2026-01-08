package org.apache.shenyu.plugin.base.route;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;

import java.util.Objects;
import java.util.Optional;

/**
 * Route resolution result containing matched selector and rule data.
 */
public final class RouteResult {

    private final SelectorData selector;
    private final RuleData rule;
    private final boolean matched;
    private final MatchType matchType;

    private RouteResult(SelectorData selector, RuleData rule, boolean matched, MatchType matchType) {
        this.selector = selector;
        this.rule = rule;
        this.matched = matched;
        this.matchType = matchType;
    }

    /**
     * Create a successful route result.
     */
    public static RouteResult success(SelectorData selector, RuleData rule, MatchType matchType) {
        return new RouteResult(
                Objects.requireNonNull(selector, "Selector cannot be null"),
                Objects.requireNonNull(rule, "Rule cannot be null"),
                true,
                matchType);
    }

    /**
     * Create a no-match route result.
     */
    public static RouteResult noMatch() {
        return new RouteResult(null, null, false, MatchType.NONE);
    }

    /**
     * Create a selector-only match result (for continued = false scenarios).
     */
    public static RouteResult selectorOnly(SelectorData selector, MatchType matchType) {
        return new RouteResult(
                Objects.requireNonNull(selector, "Selector cannot be null"),
                null,
                true,
                matchType);
    }

    // Getters
    public Optional<SelectorData> getSelector() {
        return Optional.ofNullable(selector);
    }

    public Optional<RuleData> getRule() {
        return Optional.ofNullable(rule);
    }

    public boolean isMatched() {
        return matched;
    }

    public MatchType getMatchType() {
        return matchType;
    }

    public boolean hasRule() {
        return rule != null;
    }

    public boolean hasSelector() {
        return selector != null;
    }

    /**
     * Match type indicating how the route was resolved.
     */
    public enum MatchType {
        CACHE_HIT,      // Matched from L1 cache
        TRIE_MATCH,     // Matched from L2 Trie cache
        DEFAULT_MATCH,  // Matched using default strategy
        NONE           // No match found
    }

    @Override
    public String toString() {
        return String.format("RouteResult{matched=%s, matchType=%s, hasSelector=%s, hasRule=%s}",
                matched, matchType, hasSelector(), hasRule());
    }
}