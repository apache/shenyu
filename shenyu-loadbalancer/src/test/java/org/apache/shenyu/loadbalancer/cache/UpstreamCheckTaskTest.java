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

import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.awaitility.Awaitility;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;

import java.io.IOException;
import java.net.ServerSocket;
import java.util.concurrent.TimeUnit;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.Is.is;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * The type Upstream check task test.
 */
public class UpstreamCheckTaskTest {

    /**
     *  Here to set interval with 50s to avoid running the second time.
     */
    private final UpstreamCheckTask healthCheckTask = new UpstreamCheckTask(50000);

    /**
     * Test run.
     */
    @Test
    @Timeout(30000)
    public void testRun() throws IOException {
        // Mock selectorId1~selectorId4 to let it coverage 4 branch of `HealthCheckTask#check` method.
        final String selectorId1 = "s1";
        SelectorData selectorData1 = mock(SelectorData.class);
        final String selectorId2 = "s2";
        SelectorData selectorData2 = mock(SelectorData.class);
        final String selectorId3 = "s3";
        SelectorData selectorData3 = mock(SelectorData.class);
        final String selectorId4 = "s4";
        SelectorData selectorData4 = mock(SelectorData.class);
        Upstream upstream = mock(Upstream.class);
        when(selectorData1.getId()).thenReturn(selectorId1);
        when(selectorData2.getId()).thenReturn(selectorId2);
        when(selectorData3.getId()).thenReturn(selectorId3);
        when(selectorData4.getId()).thenReturn(selectorId4);

        /*
          Let it coverage line 165~175
          We should use powermock or mockito to mock static method of `UpstreamCheckUtils.checkUrl`,
          But mocked static method is not valid across thread. Because `UpstreamCheckUtils.checkUrl` is called in
          HealthCheckTask inner thread pool, but mocked in current thread. So we turn to do like below.
         */
        when(upstream.getUrl()).thenReturn("");
        when(upstream.isHealthy()).thenReturn(true).thenReturn(false);

        healthCheckTask.triggerAddOne(selectorData1.getId(), upstream);
        healthCheckTask.triggerAddOne(selectorData2.getId(), upstream);
        healthCheckTask.triggerAddOne(selectorData3.getId(), upstream);
        healthCheckTask.triggerAddOne(selectorData4.getId(), upstream);
        healthCheckTask.schedule();
        // Wait for the upstream-health-check thread to start.
        Awaitility.await().pollDelay(3500, TimeUnit.MILLISECONDS).untilAsserted(() -> assertFalse(healthCheckTask.getCheckStarted().get()));
        assertTrue(CollectionUtils.isNotEmpty(healthCheckTask.getUnhealthyUpstream().get(selectorId1)));
        // Let it coverage line 151~163.
        when(upstream.isHealthy()).thenReturn(false).thenReturn(true);
        try (ServerSocket serverSocket = new ServerSocket(0)) {
            when(upstream.getUrl()).thenReturn("http://127.0.0.1:" + serverSocket.getLocalPort());
            // Manually run one time against a local listening socket.
            healthCheckTask.run();
            Awaitility.await().pollDelay(1, TimeUnit.SECONDS).untilAsserted(() -> assertFalse(healthCheckTask.getCheckStarted().get()));
            assertFalse(healthCheckTask.getHealthyUpstream().get(selectorId1).isEmpty());
        }
        healthCheckTask.print();
    }
    
    /**
     * Test trigger remove one.
     */
    @Test
    public void testTriggerRemoveOne() {
        final String selectorId = "s1";
        Upstream upstream = mock(Upstream.class);
        healthCheckTask.triggerAddOne(selectorId, upstream);
        healthCheckTask.triggerRemoveOne(selectorId, upstream);
        assertThat(healthCheckTask.getHealthyUpstream().get(selectorId).size(), is(0));

        healthCheckTask.triggerAddOne(selectorId, upstream);
        healthCheckTask.triggerRemoveOne(selectorId, upstream);
        assertThat(healthCheckTask.getHealthyUpstream().get(selectorId).size(), is(0));
    }
    
    /**
     * Test trigger remove all.
     */
    @Test
    public void testTriggerRemoveAll() {
        final String selectorId = "s1";
        Upstream upstream = mock(Upstream.class);
        healthCheckTask.triggerAddOne(selectorId, upstream);
        healthCheckTask.triggerRemoveAll(selectorId);
        assertFalse(healthCheckTask.getHealthyUpstream().containsKey(selectorId));

        healthCheckTask.triggerAddOne(selectorId, upstream);
        healthCheckTask.triggerRemoveAll(selectorId);
        assertFalse(healthCheckTask.getHealthyUpstream().containsKey(selectorId));
    }

    /**
     * Test that upstream with healthCheckEnabled=false is always marked as healthy.
     */
    @Test
    @Timeout(10000)
    public void testHealthCheckDisabled() {
        final String selectorId = "healthCheckDisabledSelector";

        // Create upstream with healthCheckEnabled = false, and set healthy to false
        // manually
        Upstream upstream = Upstream.builder()
                .url("unreachable-url:8080")
                .healthCheckEnabled(false)
                .build();
        upstream.setHealthy(false);

        healthCheckTask.triggerAddOne(selectorId, upstream);
        healthCheckTask.setPoolSize(1);
        healthCheckTask.schedule();

        // Wait for health check to complete
        Awaitility.await().pollDelay(3500, TimeUnit.MILLISECONDS)
                .untilAsserted(() -> assertFalse(healthCheckTask.getCheckStarted().get()));

        // When healthCheckEnabled is false, upstream should be marked as healthy
        assertTrue(healthCheckTask.getHealthyUpstream().containsKey(selectorId));
        assertTrue(healthCheckTask.getHealthyUpstream().get(selectorId).get(0).isHealthy());
    }

    /**
     * Test that healthCheckEnabled defaults to true in Upstream.
     */
    @Test
    public void testHealthCheckEnabledDefaultsToTrue() {
        Upstream upstream = Upstream.builder()
                .url("test-url:8080")
                .build();

        assertTrue(upstream.isHealthCheckEnabled());
    }

    /**
     * Test public putToMap method.
     */
    @Test
    public void testPutToMap() {
        final String selectorId = "putToMapTest";
        Upstream upstream1 = Upstream.builder()
                .url("upstream1:8080")
                .build();
        Upstream upstream2 = Upstream.builder()
                .url("upstream2:8080")
                .build();

        // Test adding to healthy map
        healthCheckTask.putToMap(healthCheckTask.getHealthyUpstream(), selectorId, upstream1);
        assertThat(healthCheckTask.getHealthyUpstream().get(selectorId).size(), is(1));

        // Test adding another upstream
        healthCheckTask.putToMap(healthCheckTask.getHealthyUpstream(), selectorId, upstream2);
        assertThat(healthCheckTask.getHealthyUpstream().get(selectorId).size(), is(2));

        // Test adding duplicate (should not add again)
        healthCheckTask.putToMap(healthCheckTask.getHealthyUpstream(), selectorId, upstream1);
        assertThat(healthCheckTask.getHealthyUpstream().get(selectorId).size(), is(2));

        // Clean up
        healthCheckTask.triggerRemoveAll(selectorId);
    }

    /**
     * Test public putToMap method with unhealthy map.
     */
    @Test
    public void testPutToMapUnhealthy() {
        final String selectorId = "putToMapUnhealthyTest";
        Upstream upstream = Upstream.builder()
                .url("unhealthy-upstream:8080")
                .build();

        // Test adding to unhealthy map
        healthCheckTask.putToMap(healthCheckTask.getUnhealthyUpstream(), selectorId, upstream);
        assertThat(healthCheckTask.getUnhealthyUpstream().get(selectorId).size(), is(1));

        // Verify it's not in healthy map
        assertTrue(CollectionUtils.isEmpty(healthCheckTask.getHealthyUpstream().get(selectorId)));

        // Clean up
        healthCheckTask.triggerRemoveAll(selectorId);
    }

    /**
     * Test public removeFromMap method.
     */
    @Test
    public void testRemoveFromMap() {
        final String selectorId = "removeFromMapTest";
        Upstream upstream1 = Upstream.builder()
                .url("remove1:8080")
                .build();
        Upstream upstream2 = Upstream.builder()
                .url("remove2:8080")
                .build();

        // Add upstreams to healthy map
        healthCheckTask.putToMap(healthCheckTask.getHealthyUpstream(), selectorId, upstream1);
        healthCheckTask.putToMap(healthCheckTask.getHealthyUpstream(), selectorId, upstream2);
        assertThat(healthCheckTask.getHealthyUpstream().get(selectorId).size(), is(2));

        // Remove one upstream
        healthCheckTask.removeFromMap(healthCheckTask.getHealthyUpstream(), selectorId, upstream1);
        assertThat(healthCheckTask.getHealthyUpstream().get(selectorId).size(), is(1));

        // Verify correct upstream remains
        assertThat(healthCheckTask.getHealthyUpstream().get(selectorId).get(0).getUrl(), is("remove2:8080"));

        // Clean up
        healthCheckTask.triggerRemoveAll(selectorId);
    }

    /**
     * Test public removeFromMap method with unhealthy map.
     */
    @Test
    public void testRemoveFromMapUnhealthy() {
        final String selectorId = "removeFromMapUnhealthyTest";
        Upstream upstream = Upstream.builder()
                .url("unhealthy-to-remove:8080")
                .build();

        // Add to unhealthy map
        healthCheckTask.putToMap(healthCheckTask.getUnhealthyUpstream(), selectorId, upstream);
        assertThat(healthCheckTask.getUnhealthyUpstream().get(selectorId).size(), is(1));

        // Remove from unhealthy map
        healthCheckTask.removeFromMap(healthCheckTask.getUnhealthyUpstream(), selectorId, upstream);
        assertTrue(!healthCheckTask.getUnhealthyUpstream().containsKey(selectorId)
                || healthCheckTask.getUnhealthyUpstream().get(selectorId).isEmpty());
    }

    /**
     * Test moving upstream between healthy and unhealthy maps using public methods.
     */
    @Test
    public void testMoveUpstreamBetweenMaps() {
        final String selectorId = "moveBetweenMapsTest";
        Upstream upstream = Upstream.builder()
                .url("moving-upstream:8080")
                .build();
        upstream.setHealthy(true);

        // Start in healthy map
        healthCheckTask.putToMap(healthCheckTask.getHealthyUpstream(), selectorId, upstream);
        assertThat(healthCheckTask.getHealthyUpstream().get(selectorId).size(), is(1));

        // Move to unhealthy map
        healthCheckTask.removeFromMap(healthCheckTask.getHealthyUpstream(), selectorId, upstream);
        healthCheckTask.putToMap(healthCheckTask.getUnhealthyUpstream(), selectorId, upstream);

        // Verify moved
        assertTrue(!healthCheckTask.getHealthyUpstream().containsKey(selectorId)
                || healthCheckTask.getHealthyUpstream().get(selectorId).isEmpty());
        assertThat(healthCheckTask.getUnhealthyUpstream().get(selectorId).size(), is(1));

        // Move back to healthy
        healthCheckTask.removeFromMap(healthCheckTask.getUnhealthyUpstream(), selectorId, upstream);
        healthCheckTask.putToMap(healthCheckTask.getHealthyUpstream(), selectorId, upstream);

        // Verify moved back
        assertThat(healthCheckTask.getHealthyUpstream().get(selectorId).size(), is(1));
        assertTrue(!healthCheckTask.getUnhealthyUpstream().containsKey(selectorId)
                || healthCheckTask.getUnhealthyUpstream().get(selectorId).isEmpty());

        // Clean up
        healthCheckTask.triggerRemoveAll(selectorId);
    }

    /**
     * The observability counters added for #7647 must be populated after a synchronous round
     * completes, so operators can compare round duration, submitted probes and pending probes
     * against the configured checkInterval without adding a profiler to the runtime.
     */
    @Test
    @Timeout(30000)
    public void roundMetricsAreRecordedAfterRunCompletes() {
        final UpstreamCheckTask task = new UpstreamCheckTask(50000);
        task.setPoolSize(2);
        task.setCheckTimeout(500);
        task.schedule();

        final String selectorId = "metrics-round";
        for (int i = 0; i < 4; i++) {
            final Upstream upstream = Upstream.builder()
                    .url("127.0.0.1:" + (65000 + i))
                    .build();
            upstream.setHealthy(true);
            upstream.setLastHealthTimestamp(System.currentTimeMillis());
            task.triggerAddOne(selectorId, upstream);
        }

        final long before = task.getTotalRounds();
        task.run();

        assertTrue(task.getTotalRounds() > before, "totalRounds must advance after run()");
        assertTrue(task.getLastRoundDurationMillis() >= 0L, "duration must be non-negative");
        assertTrue(task.getLastRoundSubmittedChecks() >= 4,
                "submitted probes must cover every registered upstream, got " + task.getLastRoundSubmittedChecks());
        assertTrue(task.getLastRoundPendingChecks() >= 0, "pending probes must be non-negative");
        assertTrue(task.getLastRoundCompletedAtMillis() > 0L, "completion timestamp must be set");
        assertTrue(task.getTotalSubmittedChecks() >= task.getLastRoundSubmittedChecks(),
                "cumulative submitted must include the last round");
        assertTrue(task.getLastRoundOldestHealthStateMillis() >= 0L,
                "oldest health-state age must be non-negative");

        task.triggerRemoveAll(selectorId);
    }

    /**
     * An empty population still completes a round and updates the round counter, so operators
     * can distinguish "task not running" from "task running with nothing to check".
     */
    @Test
    @Timeout(10000)
    public void emptyPopulationStillRecordsRoundCompletion() {
        final UpstreamCheckTask task = new UpstreamCheckTask(50000);
        task.setPoolSize(1);
        task.schedule();

        final long before = task.getTotalRounds();
        task.run();

        assertTrue(task.getTotalRounds() > before, "round counter must advance even with no upstreams");
        assertTrue(task.getLastRoundDurationMillis() >= 0L, "duration must be non-negative");
        assertTrue(task.getLastRoundSubmittedChecks() == 0, "no probes should be submitted for an empty population");
        assertTrue(task.getLastRoundPendingChecks() == 0, "no probes should be pending for an empty population");
        assertTrue(task.getLastRoundMaxEndpointLatencyMillis() == 0L,
                "endpoint latency must stay zero when no probe ran");
    }

    /**
     * Upstreams registered with a stale {@code lastHealthTimestamp} must surface through
     * {@code lastRoundOldestHealthStateMillis}, so the metric actually reflects health-state age
     * instead of collapsing to zero.
     */
    @Test
    @Timeout(10000)
    public void oldestHealthStateAgeReflectsStaleTimestamp() {
        final UpstreamCheckTask task = new UpstreamCheckTask(50000);
        task.setPoolSize(1);
        task.schedule();

        final String selectorId = "metrics-stale";
        // Use a blank URL so checkUrl() returns false without any network I/O. A fixed port such as
        // 127.0.0.1:65100 would let an unrelated listener flip the probe to pass and refresh the
        // timestamp, making the stale-age assertion flaky.
        final Upstream stale = Upstream.builder().url("").build();
        stale.setHealthy(true);
        // Pretend the last successful probe happened 5 seconds ago.
        stale.setLastHealthTimestamp(System.currentTimeMillis() - 5_000L);
        task.triggerAddOne(selectorId, stale);

        task.run();

        assertTrue(task.getLastRoundOldestHealthStateMillis() >= 4_000L,
                "oldest health-state age must reflect the stale timestamp, got "
                        + task.getLastRoundOldestHealthStateMillis());

        task.triggerRemoveAll(selectorId);
    }

    /**
     * A health-check-disabled upstream that is already healthy never gets its {@code lastHealthTimestamp}
     * written, so it stays at the primitive default {@code 0}. Without a guard the oldest-health-state
     * metric would report {@code now - 0}, an epoch-sized (~56 years) age that hides every real
     * observation. The metric must skip such never-measured entries and report a small age instead.
     */
    @Test
    @Timeout(10000)
    public void oldestHealthStateAgeSkipsNeverMeasuredUpstream() {
        final UpstreamCheckTask task = new UpstreamCheckTask(50000);
        task.setPoolSize(1);
        task.schedule();

        final String selectorId = "metrics-disabled";
        final Upstream disabled = Upstream.builder().url("").healthCheckEnabled(false).build();
        // Healthy + disabled short-circuits the probe in check() without ever writing a timestamp.
        disabled.setHealthy(true);
        task.triggerAddOne(selectorId, disabled);

        task.run();

        final long age = task.getLastRoundOldestHealthStateMillis();
        assertTrue(age < TimeUnit.MINUTES.toMillis(1),
                "never-measured upstream must be skipped, not reported as an epoch-sized age, got " + age);

        task.triggerRemoveAll(selectorId);
    }

    /**
     * Rounds must be re-entrant safe: a second {@code run()} invocation resets the per-round
     * scratch counters so the exposed metrics never leak values from the previous round.
     */
    @Test
    @Timeout(30000)
    public void perRoundCountersAreResetBetweenRounds() {
        final UpstreamCheckTask task = new UpstreamCheckTask(50000);
        task.setPoolSize(2);
        task.setCheckTimeout(500);
        task.schedule();

        final String selectorId = "metrics-reset";
        final Upstream upstream = Upstream.builder().url("127.0.0.1:65200").build();
        upstream.setHealthy(true);
        upstream.setLastHealthTimestamp(System.currentTimeMillis());
        task.triggerAddOne(selectorId, upstream);

        task.run();
        final int firstSubmitted = task.getLastRoundSubmittedChecks();
        final long firstRounds = task.getTotalRounds();
        assertTrue(firstSubmitted >= 1, "first round must submit at least one probe");

        task.triggerRemoveAll(selectorId);
        task.run();

        assertTrue(task.getTotalRounds() > firstRounds, "round counter must advance across runs");
        assertTrue(task.getLastRoundSubmittedChecks() == 0,
                "second round must not carry over submitted count from the first round");
    }
}
