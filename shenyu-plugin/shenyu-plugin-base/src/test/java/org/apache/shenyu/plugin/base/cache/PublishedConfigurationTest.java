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

package org.apache.shenyu.plugin.base.cache;

import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Regression for source references changing another request's published configuration.
 */
class PublishedConfigurationTest {

    private final BaseDataCache cache = BaseDataCache.getInstance();

    private final BaseDataCache.ReadOnly view = cache.readOnly();

    @BeforeEach
    @AfterEach
    void cleanup() {
        cache.cleanPluginData();
        cache.cleanSelectorData();
        cache.cleanRuleData();
        MatchDataCache.getInstance().cleanSelectorData();
        MatchDataCache.getInstance().cleanRuleDataData();
    }

    @Test
    void legacyControlDemonstratesWhyContainerImmutabilityAloneIsInsufficient() {
        SelectorData source = selector("one", 0);
        cache.cacheSelectData(source);
        List<SelectorData> legacy = cache.obtainSelectorData("divide");
        assertThrows(UnsupportedOperationException.class, legacy::clear);
        source.getConditionList().get(0).setParamValue("/polluted");
        assertEquals("/polluted", legacy.get(0).getConditionList().get(0).getParamValue());
        assertEquals("/original", view.obtainSelectorData("divide").get(0).getConditionList().get(0).getParamValue());
    }

    @Test
    void publicationDetachesNestedConditionsFromTheSynchronizationDto() {
        SelectorData source = selector("one", 0);
        cache.cacheSelectData(source);
        final SelectorData published = view.obtainSelectorData("divide").get(0);
        source.setHandle("changed");
        source.getConditionList().get(0).setParamValue("/polluted");
        source.getConditionList().clear();
        assertEquals("/original", published.getConditionList().get(0).getParamValue());
        assertEquals("original", published.getHandle());
        assertSame(source, cache.obtainSelectorData("divide").get(0));
        assertNotSame(source, published);
        assertThrows(UnsupportedOperationException.class, () -> published.setHandle("polluted"));
        assertThrows(UnsupportedOperationException.class, () -> published.getConditionList().get(0).setParamValue("polluted"));
    }

    @Test
    void pluginAndRuleCopiesDetachAndReuseUnchangedPublishedEntries() {
        PluginData plugin = PluginData.builder().name("divide").enabled(true).config("{}").build();
        cache.cachePluginData(plugin);
        RuleData first = rule("one", 1);
        cache.cacheRuleData(first);
        RuleData published = view.obtainRuleData("selector").get(0);
        cache.cacheRuleData(rule("two", 2));
        assertSame(published, view.obtainRuleData("selector").get(0));
        plugin.setEnabled(false);
        first.setHandle("changed");
        assertTrue(view.obtainPluginData("divide").getEnabled());
        assertEquals("original", published.getHandle());
        assertSame(view.getRuleMap(), view.getRuleMap());
        assertSame(view.obtainRuleData("selector"), view.obtainRuleData("selector"));
    }

    @Test
    void publishedMapsAndEveryExposedContainerRejectWrites() {
        cache.cacheSelectData(selector("one", 0));
        cache.cacheRuleData(rule("one", 0));
        cache.cachePluginData(PluginData.builder().name("divide").enabled(true).build());
        Map<String, List<SelectorData>> selectors = view.getSelectorMap();
        assertThrows(UnsupportedOperationException.class, selectors::clear);
        assertThrows(UnsupportedOperationException.class, () -> selectors.remove("divide"));
        assertThrows(UnsupportedOperationException.class, () -> selectors.compute("divide", (key, value) -> List.of()));
        assertThrows(UnsupportedOperationException.class, () -> selectors.entrySet().iterator().next().setValue(List.of()));
        assertThrows(UnsupportedOperationException.class, () -> selectors.values().remove(selectors.get("divide")));
        assertThrows(UnsupportedOperationException.class, () -> selectors.get("divide").clear());
        assertThrows(UnsupportedOperationException.class, () -> selectors.get("divide").get(0).getBeforeConditionList().clear());
        assertThrows(UnsupportedOperationException.class, () -> view.getPluginMap().get("divide").setEnabled(false));
        assertThrows(UnsupportedOperationException.class, () -> view.getRuleMap().get("selector").get(0).setEnabled(false));
        assertThrows(UnsupportedOperationException.class, () -> view.getRuleMap().put("injected", List.of(rule("injected", 0))));
        cache.getPluginMap().clear();
        cache.getSelectorMap().get("divide").get(0).setEnabled(false);
        cache.getRuleMap().clear();
        assertTrue(view.obtainPluginData("divide").getEnabled());
        assertTrue(view.obtainSelectorData("divide").get(0).getEnabled());
        assertEquals(1, view.obtainRuleData("selector").size());
    }

    @Test
    void allPublicationAndRemovalFormsReachTheReadOnlyContract() {
        cache.refreshPluginData(List.of(PluginData.builder().name("divide").enabled(true).build()));
        cache.refreshSelectorData(List.of(selector("a", 2), selector("b", 1)));
        cache.refreshRuleData(List.of(rule("a", 2), rule("b", 1)));
        assertEquals(List.of("b", "a"), view.obtainSelectorData("divide").stream().map(SelectorData::getId).toList());
        assertEquals(List.of("b", "a"), view.obtainRuleData("selector").stream().map(RuleData::getId).toList());
        SelectorData old = view.obtainSelectorData("divide").get(0);
        cache.refreshSelectorData(List.of(selector("b", 4)));
        assertEquals(1, old.getSort());
        assertEquals(List.of("a", "b"), view.obtainSelectorData("divide").stream().map(SelectorData::getId).toList());
        cache.removeSelectDataBatch(List.of(SelectorData.builder().id("a").build()));
        cache.removeRuleDataBatch(List.of(rule("a", 2)));
        assertEquals(1, view.obtainSelectorData("divide").size());
        assertEquals(1, view.obtainRuleData("selector").size());
        cache.removeSelectDataByPluginName("divide");
        cache.removeRuleDataBySelectorId("selector");
        cache.removePluginDataByPluginName("divide");
        assertNull(view.obtainPluginData("divide"));
        assertNull(view.obtainSelectorData("divide"));
        assertNull(view.obtainRuleData("selector"));
        cache.cacheSelectData(selector("c", 1));
        cache.cacheRuleData(rule("c", 1));
        cache.removeSelectData(SelectorData.builder().id("c").build());
        cache.removeRuleData(rule("c", 1));
        assertTrue(view.getSelectorMap().isEmpty());
        assertTrue(view.getRuleMap().isEmpty());
    }

    @Test
    void unsupportedSnapshotInputLeavesBothPublishedAndLegacyStateUnchanged() {
        cache.cacheSelectData(selector("one", 0));
        SelectorData oldLegacy = cache.obtainSelectorData("divide").get(0);
        final SelectorData oldPublished = view.obtainSelectorData("divide").get(0);
        SelectorData invalid = selector("one", 1);
        invalid.setConditionList(List.of(new ConditionData() { }));
        assertThrows(IllegalArgumentException.class, () -> cache.cacheSelectData(invalid));
        assertSame(oldLegacy, cache.obtainSelectorData("divide").get(0));
        assertSame(oldPublished, view.obtainSelectorData("divide").get(0));
        assertThrows(IllegalArgumentException.class, () -> cache.refreshSelectorData(List.of(selector("two", 2), invalid)));
        assertEquals(List.of(oldLegacy), cache.obtainSelectorData("divide"));
        assertEquals(List.of(oldPublished), view.obtainSelectorData("divide"));
    }

    @Test
    void isolatedMatchCacheRejectsMutableInjectionAndReceivesInvalidations() {
        MatchDataCache matches = MatchDataCache.getInstance();
        SelectorData source = selector("one", 0);
        RuleData sourceRule = rule("one", 0);
        cache.cacheSelectData(source);
        cache.cacheRuleData(sourceRule);
        matches.cacheSelectorData("/original", source, 8, 32, 60000);
        matches.cacheRuleData("/original", sourceRule, 8, 32, 60000);
        assertNull(matches.readOnly().obtainSelectorData("divide", "/original"));
        assertNull(matches.readOnly().obtainRuleData("divide", "/original"));
        assertThrows(IllegalArgumentException.class, () -> matches.readOnly().cacheSelectorData("/original", source, 8, 32, 60000));
        assertThrows(IllegalArgumentException.class, () -> matches.readOnly().cacheRuleData("/original", sourceRule, 8, 32, 60000));
        SelectorData frozen = view.obtainSelectorData("divide").get(0);
        RuleData frozenRule = view.obtainRuleData("selector").get(0);
        matches.readOnly().cacheSelectorData("/original", frozen, 8, 32, 60000);
        matches.readOnly().cacheRuleData("/original", frozenRule, 8, 32, 60000);
        assertSame(frozen, matches.readOnly().obtainSelectorData("divide", "/original"));
        assertSame(frozenRule, matches.readOnly().obtainRuleData("divide", "/original"));
        matches.removeSelectorData("divide", "one");
        matches.removeRuleDataBySelector("divide", "selector");
        assertNull(matches.readOnly().obtainSelectorData("divide", "/original"));
        assertNull(matches.readOnly().obtainRuleData("divide", "/original"));
    }

    @Test
    void readersRetainStableNestedValuesAcrossUpdateAndDelete() throws Exception {
        cache.cacheSelectData(selector("one", 0));
        SelectorData held = view.obtainSelectorData("divide").get(0);
        CountDownLatch start = new CountDownLatch(1);
        ExecutorService executor = Executors.newSingleThreadExecutor();
        try {
            Future<?> writer = executor.submit(() -> {
                try {
                    assertTrue(start.await(10, TimeUnit.SECONDS));
                    for (int index = 0; index < 100; index++) {
                        SelectorData next = selector("one", index);
                        next.getConditionList().get(0).setParamValue("/updated-" + index);
                        cache.cacheSelectData(next);
                    }
                    cache.removeSelectDataByPluginName("divide");
                } catch (InterruptedException error) {
                    Thread.currentThread().interrupt();
                    throw new IllegalStateException(error);
                }
            });
            start.countDown();
            for (int index = 0; index < 1000; index++) {
                assertEquals("/original", held.getConditionList().get(0).getParamValue());
                assertEquals(0, held.getSort());
            }
            writer.get(10, TimeUnit.SECONDS);
            assertNull(view.obtainSelectorData("divide"));
            assertFalse(held.getConditionList().isEmpty());
        } finally {
            executor.shutdownNow();
            assertTrue(executor.awaitTermination(10, TimeUnit.SECONDS));
        }
    }

    private SelectorData selector(final String id, final int sort) {
        ConditionData condition = new ConditionData();
        condition.setParamType("uri");
        condition.setOperator("=");
        condition.setParamValue("/original");
        return SelectorData.builder().id(id).pluginName("divide").sort(sort).enabled(true).handle("original")
                .conditionList(new ArrayList<>(List.of(condition))).beforeConditionList(new ArrayList<>(List.of(condition))).build();
    }

    private RuleData rule(final String id, final int sort) {
        return RuleData.builder().id(id).pluginName("divide").selectorId("selector").sort(sort).enabled(true).handle("original").build();
    }
}
