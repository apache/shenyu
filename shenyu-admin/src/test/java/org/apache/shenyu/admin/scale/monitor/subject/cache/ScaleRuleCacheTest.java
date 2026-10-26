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

package org.apache.shenyu.admin.scale.monitor.subject.cache;

import org.apache.shenyu.admin.model.entity.ScaleRuleDO;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Tests for the scaling rule cache.
 */
public final class ScaleRuleCacheTest {

    @Test
    public void testInitializeKeepsRulesWithSameMetric() {
        final ScaleRuleCache cache = new ScaleRuleCache();
        final ScaleRuleDO first = ScaleRuleDO.builder()
                .id("rule-a")
                .metricName("cpu_usage")
                .maximum("60")
                .build();
        final ScaleRuleDO second = ScaleRuleDO.builder()
                .id("rule-b")
                .metricName("cpu_usage")
                .maximum("80")
                .build();

        cache.initialize(List.of(first, second));

        assertEquals(2, cache.getAllRules().size());
    }

    @Test
    public void testAddingRulesWithSameMetricKeepsBothRules() {
        final ScaleRuleCache cache = new ScaleRuleCache();
        final ScaleRuleDO first = rule("a", "cpu_usage", 1);
        final ScaleRuleDO second = rule("b", "cpu_usage", 2);

        cache.addOrUpdateRuleToCache(first);
        cache.addOrUpdateRuleToCache(second);

        assertEquals(2, cache.getAllRules().size());
        assertTrue(cache.getAllRules().containsAll(List.of(first, second)));
    }

    @Test
    public void testUpdatingRuleReplacesOnlyItsOwnEntry() {
        final ScaleRuleCache cache = new ScaleRuleCache();
        final ScaleRuleDO first = rule("a", "cpu_usage", 1);
        final ScaleRuleDO second = rule("b", "cpu_usage", 2);
        cache.initialize(List.of(first, second));
        final ScaleRuleDO updated = rule("a", "cpu_usage", 3);

        cache.addOrUpdateRuleToCache(updated);

        assertEquals(2, cache.getAllRules().size());
        assertTrue(cache.getAllRules().containsAll(List.of(updated, second)));
    }

    @Test
    public void testRenamingRuleKeepsOtherRulesForOriginalMetric() {
        final ScaleRuleCache cache = new ScaleRuleCache();
        final ScaleRuleDO first = rule("a", "cpu_usage", 1);
        final ScaleRuleDO second = rule("b", "cpu_usage", 2);
        cache.initialize(List.of(first, second));
        final ScaleRuleDO renamed = rule("a", "memory_usage", 1);

        cache.addOrUpdateRuleToCache(renamed);

        assertEquals(2, cache.getAllRules().size());
        assertSame(second, cache.getRuleByMetricName("cpu_usage"));
        assertSame(renamed, cache.getRuleByMetricName("memory_usage"));
    }

    @Test
    public void testDeletingRuleByIdKeepsOtherRulesForSameMetric() {
        final ScaleRuleCache cache = new ScaleRuleCache();
        final ScaleRuleDO first = rule("a", "cpu_usage", 1);
        final ScaleRuleDO second = rule("b", "cpu_usage", 2);
        cache.initialize(List.of(first, second));

        cache.removeRulesByIdsFromCache(List.of("b", "missing"));

        assertEquals(List.of(first), cache.getAllRules());
    }

    @Test
    public void testDeletingByMetricRemovesAllMatchingRules() {
        final ScaleRuleCache cache = new ScaleRuleCache();
        final ScaleRuleDO first = rule("a", "cpu_usage", 1);
        final ScaleRuleDO second = rule("b", "cpu_usage", 2);
        final ScaleRuleDO other = rule("c", "memory_usage", 1);
        cache.initialize(List.of(first, second, other));

        cache.removeRulesFromCache(List.of("cpu_usage"));

        assertEquals(List.of(other), cache.getAllRules());
    }

    @Test
    public void testInitializeReplacesPreviousRules() {
        final ScaleRuleCache cache = new ScaleRuleCache();
        cache.initialize(List.of(rule("a", "cpu_usage", 1)));
        final ScaleRuleDO replacement = rule("b", "memory_usage", 1);

        cache.initialize(List.of(replacement));

        assertEquals(List.of(replacement), cache.getAllRules());
    }

    @Test
    public void testMetricLookupUsesPriorityAndStableIdOrder() {
        final ScaleRuleCache cache = new ScaleRuleCache();
        final ScaleRuleDO expected = rule("a", "cpu_usage", 1);
        cache.initialize(List.of(rule("c", "cpu_usage", 2), rule("b", "cpu_usage", 1),
                rule("d", "cpu_usage", null), expected));

        assertSame(expected, cache.getRuleByMetricName("cpu_usage"));
        assertNull(cache.getRuleByMetricName("missing"));
    }

    private ScaleRuleDO rule(final String id, final String metricName, final Integer sort) {
        return ScaleRuleDO.builder().id(id).metricName(metricName).sort(sort).build();
    }
}
