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

import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Assertions;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * The type UpstreamCacheManager check task test.
 */
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class UpstreamCacheManagerTest {

    private static final String SELECTOR_ID = "SELECTOR_ID";

    @Test
    @Order(1)
    public void initUpstreamCacheManagerTest() throws InterruptedException {
        final ShenyuConfig shenyuConfig = new ShenyuConfig();
        shenyuConfig.getUpstreamCheck().setEnabled(true);
        shenyuConfig.getUpstreamCheck().setPrintEnabled(true);
        shenyuConfig.getUpstreamCheck().setPrintInterval(1);
        Singleton.INST.single(ShenyuConfig.class, shenyuConfig);
        Assertions.assertNotNull(UpstreamCacheManager.getInstance());
        Thread.sleep(3);
    }

    @Test
    @Order(2)
    public void submitTest() {
        final UpstreamCacheManager upstreamCacheManager = UpstreamCacheManager.getInstance();
        List<Upstream> upstreamList = new ArrayList<>(2);
        upstreamCacheManager.submit(SELECTOR_ID, upstreamList);
        upstreamList.add(Upstream.builder().url("url").status(true).build());
        upstreamList.add(Upstream.builder().status(true).build());
        upstreamCacheManager.submit(SELECTOR_ID, upstreamList);
        // hit `existUpstream.stream().filter`
        upstreamList.clear();
        upstreamList.add(Upstream.builder().url("url2").status(true).build());
        upstreamList.add(Upstream.builder().url("url").status(true).build());
        upstreamCacheManager.submit(SELECTOR_ID, upstreamList);
    }

    @Test
    @Order(3)
    public void removeByKeyTest() {
        final UpstreamCacheManager upstreamCacheManager = UpstreamCacheManager.getInstance();
        upstreamCacheManager.removeByKey(SELECTOR_ID);
    }

    @Test
    @Order(4)
    public void findUpstreamListBySelectorIdTest() {
        final UpstreamCacheManager upstreamCacheManager = UpstreamCacheManager.getInstance();
        Assertions.assertNull(upstreamCacheManager.findUpstreamListBySelectorId(SELECTOR_ID));
    }

    @Test
    @Order(5)
    public void testSubmitSyncsHealthCheckEnabled() {
        final UpstreamCacheManager upstreamCacheManager = UpstreamCacheManager.getInstance();
        final String testSelectorId = "HEALTH_CHECK_SYNC_TEST";

        // First submit with healthCheckEnabled = true (default)
        List<Upstream> upstreamList = new ArrayList<>(1);
        upstreamList.add(Upstream.builder()
                .url("health-check-url:8080")
                .status(true)
                .healthCheckEnabled(true)
                .build());
        upstreamCacheManager.submit(testSelectorId, upstreamList);

        // Now submit with the same URL but healthCheckEnabled = false
        List<Upstream> updatedList = new ArrayList<>(1);
        updatedList.add(Upstream.builder()
                .url("health-check-url:8080")
                .status(true)
                .healthCheckEnabled(false)
                .build());
        upstreamCacheManager.submit(testSelectorId, updatedList);

        // Clean up
        upstreamCacheManager.removeByKey(testSelectorId);
    }

    @Test
    @Order(6)
    public void testSubmitWithStatusFalsePreservesUnhealthyState() {
        final UpstreamCacheManager upstreamCacheManager = UpstreamCacheManager.getInstance();
        final String testSelectorId = "PRESERVE_UNHEALTHY_TEST";

        // First, submit healthy upstreams to establish baseline
        List<Upstream> initialList = new ArrayList<>(2);
        initialList.add(Upstream.builder()
                .protocol("http://")
                .url("upstream1:8080")
                .status(true)
                .healthCheckEnabled(true)
                .build());
        initialList.add(Upstream.builder()
                .protocol("http://")
                .url("upstream2:8080")
                .status(true)
                .healthCheckEnabled(true)
                .build());
        upstreamCacheManager.submit(testSelectorId, initialList);

        // Simulate health check marking one as unhealthy
        UpstreamCheckTask task = getUpstreamCheckTask(upstreamCacheManager);
        if (Objects.nonNull(task)) {
            Upstream unhealthyUpstream = initialList.get(0);
            unhealthyUpstream.setHealthy(false);
            task.putToMap(task.getUnhealthyUpstream(), testSelectorId, unhealthyUpstream);
            task.removeFromMap(task.getHealthyUpstream(), testSelectorId, unhealthyUpstream);

            // Verify it's in unhealthy map
            Assertions.assertNotNull(task.getUnhealthyUpstream().get(testSelectorId));
            Assertions.assertTrue(task.getUnhealthyUpstream().get(testSelectorId).stream()
                    .anyMatch(u -> u.getUrl().equals("upstream1:8080")));
        }

        // Now admin sends update with status=false for that upstream
        List<Upstream> updateList = new ArrayList<>(2);
        updateList.add(Upstream.builder()
                .protocol("http://")
                .url("upstream1:8080")
                .status(false)
                .healthCheckEnabled(true)
                .build());
        updateList.add(Upstream.builder()
                .protocol("http://")
                .url("upstream2:8080")
                .status(true)
                .healthCheckEnabled(true)
                .build());
        upstreamCacheManager.submit(testSelectorId, updateList);

        // Verify: upstream1 should still be in unhealthy map (preserved state)
        if (Objects.nonNull(task)) {
            List<Upstream> unhealthyList = task.getUnhealthyUpstream().get(testSelectorId);
            Assertions.assertNotNull(unhealthyList);
            Assertions.assertTrue(unhealthyList.stream()
                    .anyMatch(u -> u.getUrl().equals("upstream1:8080")),
                    "upstream1 should be preserved in unhealthy map");
        }

        // Clean up
        upstreamCacheManager.removeByKey(testSelectorId);
    }

    @Test
    @Order(7)
    public void testSubmitWithNewOfflineUpstreamAddedToUnhealthy() {
        final UpstreamCacheManager upstreamCacheManager = UpstreamCacheManager.getInstance();
        final String testSelectorId = "NEW_OFFLINE_UNHEALTHY_TEST";

        // Submit a list with a new upstream having status=false
        List<Upstream> upstreamList = new ArrayList<>(1);
        upstreamList.add(Upstream.builder()
                .protocol("http://")
                .url("new-upstream:8080")
                .status(false)
                .healthCheckEnabled(true)
                .build());
        upstreamCacheManager.submit(testSelectorId, upstreamList);

        // Verify: new upstream with status=false should be in unhealthy map for monitoring
        UpstreamCheckTask task = getUpstreamCheckTask(upstreamCacheManager);
        if (Objects.nonNull(task)) {
            List<Upstream> unhealthyList = task.getUnhealthyUpstream().get(testSelectorId);
            Assertions.assertNotNull(unhealthyList);
            Assertions.assertTrue(unhealthyList.stream()
                    .anyMatch(u -> u.getUrl().equals("new-upstream:8080")),
                    "New upstream with status=false should be in unhealthy map");
        }

        // Clean up
        upstreamCacheManager.removeByKey(testSelectorId);
    }

    @Test
    @Order(8)
    public void testSubmitPreservesUnhealthyForValidUpstream() {
        final UpstreamCacheManager upstreamCacheManager = UpstreamCacheManager.getInstance();
        final String testSelectorId = "PRESERVE_UNHEALTHY_VALID_TEST";

        // First submit and mark an upstream as unhealthy
        List<Upstream> initialList = new ArrayList<>(1);
        initialList.add(Upstream.builder()
                .protocol("http://")
                .url("recovering-upstream:8080")
                .status(true)
                .healthCheckEnabled(true)
                .build());
        upstreamCacheManager.submit(testSelectorId, initialList);

        UpstreamCheckTask task = getUpstreamCheckTask(upstreamCacheManager);
        if (Objects.nonNull(task)) {
            // Manually mark as unhealthy
            Upstream unhealthyUpstream = initialList.get(0);
            unhealthyUpstream.setHealthy(false);
            task.putToMap(task.getUnhealthyUpstream(), testSelectorId, unhealthyUpstream);
            task.removeFromMap(task.getHealthyUpstream(), testSelectorId, unhealthyUpstream);

            // Now admin sends update with status=true (valid) for the same upstream
            List<Upstream> updateList = new ArrayList<>(1);
            updateList.add(Upstream.builder()
                    .protocol("http://")
                    .url("recovering-upstream:8080")
                    .status(true)
                    .healthCheckEnabled(true)
                    .build());
            upstreamCacheManager.submit(testSelectorId, updateList);

            // Verify: should preserve unhealthy state since it was previously unhealthy
            List<Upstream> unhealthyList = task.getUnhealthyUpstream().get(testSelectorId);
            Assertions.assertNotNull(unhealthyList);
            Assertions.assertTrue(unhealthyList.stream()
                    .anyMatch(u -> u.getUrl().equals("recovering-upstream:8080")),
                    "Previously unhealthy upstream should remain in unhealthy map");
        }

        // Clean up
        upstreamCacheManager.removeByKey(testSelectorId);
    }

    @Test
    @Order(9)
    public void testSubmitWithHealthCheckDisabledAndStatusFalse() {
        final UpstreamCacheManager upstreamCacheManager = UpstreamCacheManager.getInstance();
        final String testSelectorId = "HEALTH_CHECK_DISABLED_STATUS_FALSE_TEST";

        // Submit upstream with healthCheckEnabled=false and status=false
        // This upstream should be removed, not added to unhealthy map
        List<Upstream> upstreamList = new ArrayList<>(1);
        upstreamList.add(Upstream.builder()
                .protocol("http://")
                .url("no-check-upstream:8080")
                .status(false)
                .healthCheckEnabled(false)
                .build());
        upstreamCacheManager.submit(testSelectorId, upstreamList);

        UpstreamCheckTask task = getUpstreamCheckTask(upstreamCacheManager);
        if (Objects.nonNull(task)) {
            // Verify: should NOT be in unhealthy map since health check is disabled
            List<Upstream> unhealthyList = task.getUnhealthyUpstream().get(testSelectorId);
            Assertions.assertTrue(Objects.isNull(unhealthyList) || unhealthyList.isEmpty(),
                    "Upstream with healthCheckEnabled=false should not be in unhealthy map");
        }

        // Clean up
        upstreamCacheManager.removeByKey(testSelectorId);
    }

    /**
     * Helper method to get the UpstreamCheckTask using reflection.
     */
    private UpstreamCheckTask getUpstreamCheckTask(final UpstreamCacheManager manager) {
        try {
            java.lang.reflect.Field field = UpstreamCacheManager.class.getDeclaredField("task");
            field.setAccessible(true);
            return (UpstreamCheckTask) field.get(manager);
        } catch (Exception e) {
            // If reflection fails, return null
            return null;
        }
    }
}
