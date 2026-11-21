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

package org.apache.shenyu.web.contract;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.base.cache.MatchDataCache;
import org.junit.jupiter.api.Test;
import org.springframework.context.ApplicationContext;

import java.util.HashMap;
import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Verifies fixture ownership without clearing unrelated global cache entries.
 * Other tests using these globals must run serially with this test class.
 */
public class SelectorRuleFixtureTest {

    @Test
    public void constructionClearAndClosePreserveUnrelatedCachesAndRestoreContext() {
        final PluginData sentinel = plugin();
        final SelectorData sentinelSelector = selector(sentinel.getName(), uniqueName());
        final RuleData sentinelRule = rule(sentinelSelector);
        final ApplicationContext previous = SpringBeanUtils.getInstance().getApplicationContext();
        cacheDirectly(sentinel, sentinelSelector, sentinelRule);
        try {
            final SelectorRuleFixture fixture = new SelectorRuleFixture();
            final PluginData owned = plugin();
            final SelectorData ownedSelector = selector(owned.getName(), uniqueName());
            final RuleData ownedRule = rule(ownedSelector);
            try {
                assertNotSame(previous, SpringBeanUtils.getInstance().getApplicationContext());
                assertCached(sentinel, sentinelSelector, sentinelRule);
                fixture.cachePluginData(owned);
                fixture.cacheSelectorData(ownedSelector);
                fixture.cacheRuleData(ownedRule);
                cacheMatches(ownedSelector, ownedRule);
                fixture.clear();
                assertAbsent(owned, ownedSelector);
                assertCached(sentinel, sentinelSelector, sentinelRule);
                fixture.cachePluginData(owned);
                fixture.cacheSelectorData(ownedSelector);
                fixture.cacheRuleData(ownedRule);
                cacheMatches(ownedSelector, ownedRule);
            } finally {
                fixture.close();
            }
            assertAbsent(owned, ownedSelector);
            assertCached(sentinel, sentinelSelector, sentinelRule);
            assertSame(previous, SpringBeanUtils.getInstance().getApplicationContext());
            fixture.close();
            assertSame(previous, SpringBeanUtils.getInstance().getApplicationContext());
            assertThrows(IllegalStateException.class, () -> fixture.cachePluginData(owned));
        } finally {
            removeDirectly(sentinel, sentinelSelector);
        }
    }

    @Test
    public void nestedFixturesPreserveOuterCachesAndRestoreContextsInLifoOrder() {
        final ApplicationContext previous = SpringBeanUtils.getInstance().getApplicationContext();
        final PluginData outerPlugin = plugin();
        final SelectorData outerSelector = selector(outerPlugin.getName(), uniqueName());
        final RuleData outerRule = rule(outerSelector);
        try (SelectorRuleFixture outer = new SelectorRuleFixture()) {
            outer.cachePluginData(outerPlugin);
            outer.cacheSelectorData(outerSelector);
            outer.cacheRuleData(outerRule);
            cacheMatches(outerSelector, outerRule);
            final ApplicationContext outerContext = SpringBeanUtils.getInstance().getApplicationContext();
            final PluginData innerPlugin = plugin();
            final SelectorData innerSelector = selector(innerPlugin.getName(), uniqueName());
            try (SelectorRuleFixture inner = new SelectorRuleFixture()) {
                inner.cachePluginData(innerPlugin);
                inner.cacheSelectorData(innerSelector);
                inner.cacheRuleData(rule(innerSelector));
                assertNotSame(outerContext, SpringBeanUtils.getInstance().getApplicationContext());
                assertCached(outerPlugin, outerSelector, outerRule);
                assertThrows(IllegalArgumentException.class, () -> inner.cachePluginData(outerPlugin));
                assertThrows(IllegalStateException.class, outer::close);
            }
            assertAbsent(innerPlugin, innerSelector);
            assertCached(outerPlugin, outerSelector, outerRule);
            assertSame(outerContext, SpringBeanUtils.getInstance().getApplicationContext());
        }
        assertAbsent(outerPlugin, outerSelector);
        assertSame(previous, SpringBeanUtils.getInstance().getApplicationContext());
    }

    @Test
    public void pluginClaimsRejectEveryPreExistingBucketIncludingEmptyBuckets() {
        final BaseDataCache cache = BaseDataCache.getInstance();
        final MatchDataCache matchCache = MatchDataCache.getInstance();
        final PluginData pluginEntry = plugin();
        final PluginData selectorBucket = plugin();
        final PluginData selectorMatchBucket = plugin();
        final PluginData ruleMatchBucket = plugin();
        cache.cachePluginData(pluginEntry);
        cache.getSelectorMap().put(selectorBucket.getName(), List.of());
        matchCache.getSelectorMatchCache().put(selectorMatchBucket.getName(), new HashMap<>());
        matchCache.getRuleMatchCache().put(ruleMatchBucket.getName(), new HashMap<>());
        try {
            try (SelectorRuleFixture fixture = new SelectorRuleFixture()) {
                assertThrows(IllegalArgumentException.class, () -> fixture.cachePluginData(pluginEntry));
                assertThrows(IllegalArgumentException.class, () -> fixture.cachePluginData(selectorBucket));
                assertThrows(IllegalArgumentException.class, () -> fixture.cachePluginData(selectorMatchBucket));
                assertThrows(IllegalArgumentException.class, () -> fixture.cachePluginData(ruleMatchBucket));
            }
            assertSame(pluginEntry, cache.obtainPluginData(pluginEntry.getName()));
            assertEquals(List.of(), cache.obtainSelectorData(selectorBucket.getName()));
            assertEquals(new HashMap<>(), matchCache.getSelectorMatchCache().get(selectorMatchBucket.getName()));
            assertEquals(new HashMap<>(), matchCache.getRuleMatchCache().get(ruleMatchBucket.getName()));
        } finally {
            cache.removePluginDataByPluginName(pluginEntry.getName());
            cache.removeSelectDataByPluginName(selectorBucket.getName());
            matchCache.removeSelectorData(selectorMatchBucket.getName());
            matchCache.removeRuleData(ruleMatchBucket.getName());
        }
    }

    @Test
    public void selectorsRejectExistingSelectorIdsAndUnownedRuleBuckets() {
        final BaseDataCache cache = BaseDataCache.getInstance();
        final PluginData sentinelPlugin = plugin();
        final SelectorData sentinelSelector = selector(sentinelPlugin.getName(), uniqueName());
        final String orphanSelectorId = uniqueName();
        cache.cacheSelectData(sentinelSelector);
        cache.getRuleMap().put(orphanSelectorId, List.of());
        try {
            final PluginData owned = plugin();
            try (SelectorRuleFixture fixture = new SelectorRuleFixture()) {
                fixture.cachePluginData(owned);
                assertThrows(IllegalArgumentException.class,
                        () -> fixture.cacheSelectorData(selector(owned.getName(), sentinelSelector.getId())));
                assertThrows(IllegalArgumentException.class,
                        () -> fixture.cacheSelectorData(selector(owned.getName(), orphanSelectorId)));
            }
            assertEquals(List.of(sentinelSelector), cache.obtainSelectorData(sentinelPlugin.getName()));
            assertEquals(List.of(), cache.obtainRuleData(orphanSelectorId));
        } finally {
            cache.removeSelectDataByPluginName(sentinelPlugin.getName());
            cache.removeRuleDataBySelectorId(orphanSelectorId);
        }
    }

    @Test
    public void mutationsRequireClaimedPluginAndOwnedSelector() {
        final PluginData owned = plugin();
        final PluginData unclaimed = plugin();
        final SelectorData ownedSelector = selector(owned.getName(), uniqueName());
        final SelectorData unknownSelector = selector(owned.getName(), uniqueName());
        final SelectorData unclaimedSelector = selector(unclaimed.getName(), uniqueName());
        try (SelectorRuleFixture fixture = new SelectorRuleFixture()) {
            assertThrows(IllegalArgumentException.class, () -> fixture.cacheSelectorData(ownedSelector));
            assertThrows(IllegalArgumentException.class, () -> fixture.removePluginData(unclaimed.getName()));
            fixture.cachePluginData(owned);
            assertThrows(IllegalArgumentException.class, () -> fixture.cacheSelectorData(unclaimedSelector));
            assertThrows(IllegalArgumentException.class, () -> fixture.cacheRuleData(rule(ownedSelector)));
            assertThrows(IllegalArgumentException.class, () -> fixture.removeSelectorData(unknownSelector));
            assertThrows(IllegalArgumentException.class, () -> fixture.removeRuleData(rule(unknownSelector)));
            fixture.cacheSelectorData(ownedSelector);
            assertThrows(IllegalArgumentException.class, () -> fixture.cacheRuleData(rule(unclaimedSelector)));
            fixture.cachePluginData(unclaimed);
            assertThrows(IllegalArgumentException.class,
                    () -> fixture.cacheSelectorData(selector(unclaimed.getName(), ownedSelector.getId())));
        }
    }

    @Test
    public void clearingOuterFixtureRetainsItsClaimsUntilClose() {
        final PluginData outerPlugin = plugin();
        final SelectorData outerSelector = selector(outerPlugin.getName(), uniqueName());
        try (SelectorRuleFixture outer = new SelectorRuleFixture()) {
            outer.cachePluginData(outerPlugin);
            outer.cacheSelectorData(outerSelector);
            outer.clear();
            try (SelectorRuleFixture inner = new SelectorRuleFixture()) {
                assertThrows(IllegalArgumentException.class, () -> inner.cachePluginData(outerPlugin));
                final PluginData innerPlugin = plugin();
                inner.cachePluginData(innerPlugin);
                assertThrows(IllegalArgumentException.class,
                        () -> inner.cacheSelectorData(selector(innerPlugin.getName(), outerSelector.getId())));
            }
        }
    }

    @Test
    public void selectorUpdatesAndRemovalsInvalidateSelectorAndDependentRuleMatches() {
        final PluginData plugin = plugin();
        final SelectorData selector = selector(plugin.getName(), uniqueName());
        final RuleData rule = rule(selector);
        final MatchDataCache matchCache = MatchDataCache.getInstance();
        try (SelectorRuleFixture fixture = new SelectorRuleFixture()) {
            fixture.cachePluginData(plugin);
            fixture.cacheSelectorData(selector);
            fixture.cacheRuleData(rule);
            cacheMatches(selector, rule);
            cacheEmptyMatches(plugin.getName());
            final SelectorData updated = selector(plugin.getName(), selector.getId());
            fixture.cacheSelectorData(updated);
            assertEquals(List.of(updated), BaseDataCache.getInstance().obtainSelectorData(plugin.getName()));
            assertNull(matchCache.obtainSelectorData(plugin.getName(), "/matched"));
            assertNull(matchCache.obtainRuleData(plugin.getName(), "/matched"));
            assertEmptyMatchesAbsent(plugin.getName());
            cacheMatches(updated, rule);
            cacheEmptyMatches(plugin.getName());
            fixture.removeSelectorData(updated);
            assertNull(BaseDataCache.getInstance().obtainSelectorData(plugin.getName()));
            assertNull(matchCache.obtainSelectorData(plugin.getName(), "/matched"));
            assertNull(matchCache.obtainRuleData(plugin.getName(), "/matched"));
            assertEmptyMatchesAbsent(plugin.getName());
        }
    }

    @Test
    public void ruleUpdatesAndRemovalsInvalidateRuleMatchesAndCachedMisses() {
        final PluginData plugin = plugin();
        final SelectorData selector = selector(plugin.getName(), uniqueName());
        final RuleData rule = rule(selector);
        final MatchDataCache matchCache = MatchDataCache.getInstance();
        try (SelectorRuleFixture fixture = new SelectorRuleFixture()) {
            fixture.cachePluginData(plugin);
            fixture.cacheSelectorData(selector);
            fixture.cacheRuleData(rule);
            cacheMatches(selector, rule);
            cacheEmptyMatches(plugin.getName());
            final RuleData updated = rule(selector);
            updated.setId(rule.getId());
            fixture.cacheRuleData(updated);
            assertEquals(List.of(updated), BaseDataCache.getInstance().obtainRuleData(selector.getId()));
            assertNull(matchCache.obtainRuleData(plugin.getName(), "/matched"));
            assertNull(matchCache.obtainRuleData(plugin.getName(), "/miss"));
            assertSame(selector, matchCache.obtainSelectorData(plugin.getName(), "/matched"));
            cacheMatches(selector, updated);
            cacheEmptyMatches(plugin.getName());
            fixture.removeRuleData(updated);
            assertNull(BaseDataCache.getInstance().obtainRuleData(selector.getId()));
            assertNull(matchCache.obtainRuleData(plugin.getName(), "/matched"));
            assertNull(matchCache.obtainRuleData(plugin.getName(), "/miss"));
        }
    }

    @Test
    public void addingCompetingDefinitionsInvalidatesPreviouslySelectedCandidates() {
        final PluginData plugin = plugin();
        final SelectorData firstSelector = selector(plugin.getName(), uniqueName());
        final RuleData firstRule = rule(firstSelector);
        final MatchDataCache matches = MatchDataCache.getInstance();
        try (SelectorRuleFixture fixture = new SelectorRuleFixture()) {
            fixture.cachePluginData(plugin);
            fixture.cacheSelectorData(firstSelector);
            fixture.cacheRuleData(firstRule);
            cacheMatches(firstSelector, firstRule);
            final RuleData higherPriorityRule = rule(firstSelector);
            higherPriorityRule.setSort(0);
            fixture.cacheRuleData(higherPriorityRule);
            assertNull(matches.obtainRuleData(plugin.getName(), "/matched"),
                    "a newly winning rule must invalidate a different rule's positive match");
            assertSame(firstSelector, matches.obtainSelectorData(plugin.getName(), "/matched"));
            cacheMatches(firstSelector, firstRule);
            final SelectorData higherPrioritySelector = selector(plugin.getName(), uniqueName());
            higherPrioritySelector.setSort(0);
            fixture.cacheSelectorData(higherPrioritySelector);
            assertNull(matches.obtainSelectorData(plugin.getName(), "/matched"),
                    "a newly winning selector must invalidate a different selector's positive match");
            assertNull(matches.obtainRuleData(plugin.getName(), "/matched"));
        }
    }

    private PluginData plugin() {
        return PluginData.builder().name(uniqueName()).enabled(true).build();
    }

    private SelectorData selector(final String pluginName, final String selectorId) {
        return SelectorData.builder().pluginName(pluginName).id(selectorId).sort(1).enabled(true).build();
    }

    private RuleData rule(final SelectorData selector) {
        return RuleData.builder().pluginName(selector.getPluginName()).selectorId(selector.getId())
                .id(uniqueName()).sort(1).enabled(true).build();
    }

    private String uniqueName() {
        return "selector-rule-fixture-" + UUID.randomUUID();
    }

    private void cacheDirectly(final PluginData plugin, final SelectorData selector, final RuleData rule) {
        final BaseDataCache cache = BaseDataCache.getInstance();
        cache.cachePluginData(plugin);
        cache.cacheSelectData(selector);
        cache.cacheRuleData(rule);
        cacheMatches(selector, rule);
    }

    private void cacheMatches(final SelectorData selector, final RuleData rule) {
        MatchDataCache.getInstance().cacheSelectorData("/matched", selector, 16, 64);
        MatchDataCache.getInstance().cacheRuleData("/matched", rule, 16, 64);
    }

    private void cacheEmptyMatches(final String pluginName) {
        MatchDataCache.getInstance().cacheSelectorData("/miss", SelectorData.builder().pluginName(pluginName).build(), 16, 64);
        MatchDataCache.getInstance().cacheRuleData("/miss", RuleData.builder().pluginName(pluginName).build(), 16, 64);
    }

    private void assertEmptyMatchesAbsent(final String pluginName) {
        assertNull(MatchDataCache.getInstance().obtainSelectorData(pluginName, "/miss"));
        assertNull(MatchDataCache.getInstance().obtainRuleData(pluginName, "/miss"));
    }

    private void assertCached(final PluginData plugin, final SelectorData selector, final RuleData rule) {
        final BaseDataCache cache = BaseDataCache.getInstance();
        assertSame(plugin, cache.obtainPluginData(plugin.getName()));
        assertEquals(List.of(selector), cache.obtainSelectorData(plugin.getName()));
        assertEquals(List.of(rule), cache.obtainRuleData(selector.getId()));
        assertSame(selector, MatchDataCache.getInstance().obtainSelectorData(plugin.getName(), "/matched"));
        assertSame(rule, MatchDataCache.getInstance().obtainRuleData(plugin.getName(), "/matched"));
    }

    private void assertAbsent(final PluginData plugin, final SelectorData selector) {
        final BaseDataCache cache = BaseDataCache.getInstance();
        assertNull(cache.obtainPluginData(plugin.getName()));
        assertNull(cache.obtainSelectorData(plugin.getName()));
        assertNull(cache.obtainRuleData(selector.getId()));
        assertFalse(MatchDataCache.getInstance().getSelectorMatchCache().containsKey(plugin.getName()));
        assertFalse(MatchDataCache.getInstance().getRuleMatchCache().containsKey(plugin.getName()));
    }

    private void removeDirectly(final PluginData plugin, final SelectorData selector) {
        final BaseDataCache cache = BaseDataCache.getInstance();
        cache.removePluginDataByPluginName(plugin.getName());
        cache.removeSelectDataByPluginName(plugin.getName());
        cache.removeRuleDataBySelectorId(selector.getId());
        MatchDataCache.getInstance().removeSelectorData(plugin.getName());
        MatchDataCache.getInstance().removeRuleData(plugin.getName());
    }
}
