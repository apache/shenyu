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

package org.apache.shenyu.plugin.base.match;

/**
 * Immutable, bounded summary of one real selector/rule matching decision taken by
 * {@code AbstractShenyuPlugin}.
 *
 * <p>The record is emitted from the decision branches themselves, so it never re-runs a
 * predicate, consumes a request body or touches a cache. A {@link CacheResolution} of
 * {@link CacheResolution#POSITIVE_HIT} or {@link CacheResolution#NEGATIVE_HIT} states only
 * that the result came from the per-path match cache: it does <b>not</b> claim that any
 * condition was evaluated for this request, and no winner reason is recomputed for cached
 * decisions. A {@link Decision#SELECTED} outcome means matching succeeded, not that the
 * subsequent business or authorization handling succeeded.</p>
 *
 * <p>Instances carry plugin identity, the selected ids and the decision reason only. They
 * never contain request paths, credentials, condition payloads or bodies, so they are safe
 * to forward to metric labels or log pipelines.</p>
 *
 * <p>Observations are dispatched synchronously on the request's event-loop thread; the
 * observer contract requires returning immediately, because blocking there directly slows
 * the real request.</p>
 */
public final class SelectorRuleMatchObservation {

    private final String pluginName;

    private final Decision decision;

    private final String selectorId;

    private final String ruleId;

    private final boolean defaultRule;

    private final CacheResolution selectorCache;

    private final CacheResolution ruleCache;

    private SelectorRuleMatchObservation(final String pluginName, final Decision decision, final String selectorId,
                                        final String ruleId, final boolean defaultRule,
                                        final CacheResolution selectorCache, final CacheResolution ruleCache) {
        this.pluginName = pluginName;
        this.decision = decision;
        this.selectorId = selectorId;
        this.ruleId = ruleId;
        this.defaultRule = defaultRule;
        this.selectorCache = selectorCache;
        this.ruleCache = ruleCache;
    }

    /**
     * Build an immutable observation of one matching decision.
     *
     * @param pluginName the plugin that took the decision
     * @param decision the decision reason
     * @param selectorId the selected selector id, or null when none was selected
     * @param ruleId the selected rule id, or null when none was selected
     * @param defaultRule whether the rule is the synthetic default rule
     * @param selectorCache how the selector stage resolved its cache
     * @param ruleCache how the rule stage resolved its cache
     * @return the observation
     */
    public static SelectorRuleMatchObservation of(final String pluginName, final Decision decision, final String selectorId,
                                                 final String ruleId, final boolean defaultRule,
                                                 final CacheResolution selectorCache, final CacheResolution ruleCache) {
        return new SelectorRuleMatchObservation(pluginName, decision, selectorId, ruleId, defaultRule, selectorCache, ruleCache);
    }

    /**
     * Gets the plugin name.
     *
     * @return the plugin name
     */
    public String getPluginName() {
        return pluginName;
    }

    /**
     * Gets the decision reason.
     *
     * @return the decision
     */
    public Decision getDecision() {
        return decision;
    }

    /**
     * Gets the selected selector id.
     *
     * @return the selector id, or null when no selector was selected
     */
    public String getSelectorId() {
        return selectorId;
    }

    /**
     * Gets the selected rule id.
     *
     * @return the rule id, or null when no rule was selected
     */
    public String getRuleId() {
        return ruleId;
    }

    /**
     * Whether the selected rule is the synthetic default rule.
     *
     * @return true when the rule was synthesized, not selected from the rule list
     */
    public boolean isDefaultRule() {
        return defaultRule;
    }

    /**
     * Gets the selector stage cache resolution.
     *
     * @return the selector cache resolution
     */
    public CacheResolution getSelectorCache() {
        return selectorCache;
    }

    /**
     * Gets the rule stage cache resolution.
     *
     * @return the rule cache resolution
     */
    public CacheResolution getRuleCache() {
        return ruleCache;
    }

    /**
     * The reason a matching decision ended where it did.
     *
     * <p>The values name the branch of the decision tree, so a cache-sourced outcome is
     * distinguishable from a freshly evaluated one without ever re-deriving a winner.</p>
     */
    public enum Decision {

        /**
         * A selector and a rule were selected and the plugin's doExecute will run. This is
         * a matching success only; it says nothing about the business outcome.
         */
        SELECTED,

        /**
         * The selector matched but does not continue to rule selection; the synthetic
         * default rule is handed to doExecute (defaultRule is true).
         */
        SELECTOR_NOT_CONTINUED,

        /**
         * A full-flow selector supplied the rule (the last enabled rule) without any
         * condition evaluation for this request.
         */
        FULL_FLOW_RULE,

        /**
         * The plugin has no configuration data in the cache at all.
         */
        NO_PLUGIN_DATA,

        /**
         * The plugin configuration exists but is disabled.
         */
        PLUGIN_DISABLED,

        /**
         * The selector stage found no candidate and the plugin entered its
         * {@code handleSelectorIfNull} fallback hook (the default hook continues the
         * chain; plugins overriding the hook decide their own route). Covers an empty
         * selector list and a negative match-cache hit for this path.
         */
        NO_SELECTOR,

        /**
         * Live selector matching evaluated the candidate set and found no candidate.
         */
        SELECTOR_UNMATCHED,

        /**
         * The rule stage found no candidate and the plugin entered its
         * {@code handleRuleIfNull} fallback hook (the default hook continues the chain;
         * plugins overriding the hook decide their own route). Covers an empty rule list
         * and a negative match-cache hit for this path.
         */
        NO_RULE,

        /**
         * Live rule matching evaluated the candidate set and found no candidate.
         */
        RULE_UNMATCHED,

        /**
         * A full-flow selector supplied no rule because none of its rules is enabled.
         * Operationally distinct from {@link #NO_RULE}: the configuration exists but is
         * entirely disabled.
         */
        FULL_FLOW_NO_ENABLED_RULE
    }

    /**
     * How one matching stage resolved against the per-path match cache.
     *
     * <p>A hit value declares the source only; the conditions behind a cached decision were
     * not re-evaluated for the current request and no per-request predicate ran.</p>
     */
    public enum CacheResolution {

        /**
         * The stage never consulted the cache (disabled cache, or the stage was not reached).
         */
        NOT_CONSULTED,

        /**
         * The stage found a matching entry in the cache and used it.
         */
        POSITIVE_HIT,

        /**
         * The stage found the cache's negative (no-match) entry for this path.
         */
        NEGATIVE_HIT,

        /**
         * The cache was consulted but missed, so the stage matched live. Conditions were
         * evaluated only in this state (and in the hit-free unmatch paths that report the
         * corresponding UNMATCHED decisions).
         */
        EVALUATED
    }
}
