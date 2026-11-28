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

package org.apache.shenyu.loadbalancer.cache;

import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Exercises subset invalidation through the real discovery and health mutation paths.
 */
class UpstreamLabelSubsetsTest {

    private static final String SELECTOR = "label-subsets-selector";

    private static final Map<String, String> STABLE = Map.of("release", "stable");

    private static final Map<String, String> CANARY = Map.of("release", "canary");

    private final UpstreamCacheManager manager = UpstreamCacheManager.getInstance();

    @AfterEach
    void tearDown() {
        manager.removeLabelSubsets(SELECTOR);
        manager.removeByKey(SELECTOR);
    }

    @Test
    void testMetadataOnlyUpdateAndReadOnlySnapshot() {
        manager.registerLabelSubsets(SELECTOR, "r", List.of(STABLE, CANARY));
        Upstream original = upstream("same:8080", STABLE);
        manager.submit(SELECTOR, List.of(original));
        List<Upstream> request = manager.findUpstreamListByLabels(SELECTOR, STABLE);
        assertSame(original, request.get(0));
        assertSame(request, manager.findUpstreamListByLabels(SELECTOR, STABLE));
        assertThrows(UnsupportedOperationException.class, request::clear);
        assertEquals(1, manager.findUpstreamListByLabels(SELECTOR, STABLE).size());
        Upstream changed = upstream("same:8080", CANARY);
        changed.setWeight(70);
        manager.submit(SELECTOR, List.of(changed));
        assertTrue(manager.findUpstreamListByLabels(SELECTOR, STABLE).isEmpty());
        List<Upstream> canary = manager.findUpstreamListByLabels(SELECTOR, CANARY);
        assertEquals(1, canary.size());
        assertSame(original, canary.get(0));
        assertEquals(70, canary.get(0).getWeight());
    }

    @Test
    void testEmptyDiscoveryAndDrainingThenRecovery() {
        Upstream upstream = upstream("recover:8080", CANARY);
        manager.submit(SELECTOR, List.of(upstream));
        manager.registerLabelSubsets(SELECTOR, "r", List.of(CANARY));
        manager.submit(SELECTOR, List.of());
        assertTrue(manager.findUpstreamListByLabels(SELECTOR, CANARY).isEmpty());
        manager.submit(SELECTOR, List.of(upstream));
        assertEquals(List.of(upstream), manager.findUpstreamListByLabels(SELECTOR, CANARY));
        Upstream draining = upstream("recover:8080", CANARY);
        draining.setDraining(true);
        manager.submit(SELECTOR, List.of(draining));
        assertTrue(manager.findUpstreamListByLabels(SELECTOR, CANARY).isEmpty());
        manager.submit(SELECTOR, List.of(upstream("recover:8080", CANARY)));
        assertEquals(List.of(upstream), manager.findUpstreamListByLabels(SELECTOR, CANARY));
    }

    @Test
    void testReadinessUpdateRemovesCachedInstanceAndRecoveryRepopulates() {
        manager.registerLabelSubsets(SELECTOR, "r", List.of(CANARY));
        manager.submit(SELECTOR, List.of(upstream("ready:8080", CANARY)));
        final List<Upstream> before = manager.findUpstreamListByLabels(SELECTOR, CANARY);
        Upstream notReady = upstream("ready:8080", CANARY);
        notReady.setReady(false);
        manager.submit(SELECTOR, List.of(notReady));
        assertTrue(manager.findUpstreamListByLabels(SELECTOR, CANARY).isEmpty());
        assertTrue(manager.findAllUpstreamListBySelectorId(SELECTOR).isEmpty());
        assertEquals(1, before.size());
        manager.submit(SELECTOR, List.of(upstream("ready:8080", CANARY)));
        assertEquals(1, manager.findUpstreamListByLabels(SELECTOR, CANARY).size());
    }

    @Test
    void testHealthRemovalAndRecoveryRefreshSubsets() throws ReflectiveOperationException {
        Upstream upstream = upstream("health:8080", CANARY);
        manager.submit(SELECTOR, List.of(upstream));
        manager.registerLabelSubsets(SELECTOR, "r", List.of(CANARY));
        Field field = UpstreamCacheManager.class.getDeclaredField("task");
        field.setAccessible(true);
        UpstreamCheckTask task = (UpstreamCheckTask) field.get(manager);
        task.withBatch(() -> {
            task.removeFromMap(task.getHealthyUpstream(), SELECTOR, upstream);
            task.putToMap(task.getUnhealthyUpstream(), SELECTOR, upstream);
        });
        assertTrue(manager.findUpstreamListByLabels(SELECTOR, CANARY).isEmpty());
        task.withBatch(() -> {
            task.removeFromMap(task.getUnhealthyUpstream(), SELECTOR, upstream);
            task.putToMap(task.getHealthyUpstream(), SELECTOR, upstream);
        });
        assertEquals(List.of(upstream), manager.findUpstreamListByLabels(SELECTOR, CANARY));
    }

    @Test
    void testOldRulePredicateStillWorksWithoutRegistration() {
        Upstream upstream = upstream("old-rule:8080", CANARY);
        manager.submit(SELECTOR, List.of(upstream));
        manager.registerLabelSubsets(SELECTOR, "r", List.of(CANARY));
        manager.registerLabelSubsets(SELECTOR, "r", List.of(STABLE));
        List<Upstream> oldRule = manager.findUpstreamListByLabels(SELECTOR, CANARY);
        assertEquals(List.of(upstream), oldRule);
        assertThrows(UnsupportedOperationException.class, oldRule::clear);
        assertTrue(manager.findUpstreamListByLabels(SELECTOR, STABLE).isEmpty());
        assertTrue(manager.findUpstreamListByLabels(SELECTOR, Map.of()).isEmpty());
        manager.unregisterLabelSubsets(SELECTOR, "r");
        assertEquals(List.of(upstream), manager.findUpstreamListByLabels(SELECTOR, CANARY));
    }

    private Upstream upstream(final String url, final Map<String, String> labels) {
        Upstream upstream = Upstream.builder().url(url).status(true).healthCheckEnabled(false).build();
        upstream.setLabels(labels);
        return upstream;
    }
}
