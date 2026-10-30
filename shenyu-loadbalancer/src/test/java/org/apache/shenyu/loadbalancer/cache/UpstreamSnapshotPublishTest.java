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
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;

import java.lang.management.ManagementFactory;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Tests for the read-only healthy-upstream snapshot publication that replaces
 * the per-request defensive copy in {@code findUpstreamListBySelectorId}.
 */
public class UpstreamSnapshotPublishTest {

    @Test
    public void testReturnedListIsReadOnlySnapshot() {
        final UpstreamCacheManager cacheManager = UpstreamCacheManager.getInstance();
        final String selectorId = "SNAPSHOT_READONLY_TEST";
        cacheManager.submit(selectorId, buildUpstreams(2));
        try {
            List<Upstream> snapshot = cacheManager.findUpstreamListBySelectorId(selectorId);
            Assertions.assertNotNull(snapshot);
            Assertions.assertEquals(2, snapshot.size());
            Assertions.assertThrows(UnsupportedOperationException.class,
                    () -> snapshot.add(Upstream.builder().url("intruder:8080").status(true).healthCheckEnabled(false).build()));
            Assertions.assertThrows(UnsupportedOperationException.class, () -> snapshot.remove(0));
        } finally {
            cacheManager.removeByKey(selectorId);
        }
    }

    @Test
    public void testRemovedTargetIsNotARoutingCandidateInNewSnapshot() {
        final UpstreamCacheManager cacheManager = UpstreamCacheManager.getInstance();
        final String selectorId = "SNAPSHOT_REMOVAL_TEST";
        List<Upstream> upstreams = buildUpstreams(3);
        cacheManager.submit(selectorId, upstreams);
        try {
            List<Upstream> before = cacheManager.findUpstreamListBySelectorId(selectorId);
            Assertions.assertEquals(3, Objects.requireNonNull(before).size());

            UpstreamCheckTask task = getUpstreamCheckTask(cacheManager);
            Assertions.assertNotNull(task);
            task.triggerRemoveOne(selectorId, upstreams.get(1));

            List<Upstream> after = cacheManager.findUpstreamListBySelectorId(selectorId);
            Assertions.assertEquals(2, Objects.requireNonNull(after).size());
            Assertions.assertTrue(after.stream().noneMatch(u -> u.getUrl().equals(upstreams.get(1).getUrl())),
                    "removed target must not remain a routing candidate");
            // an already handed-out snapshot stays coherent, it is not mutated in place
            Assertions.assertEquals(3, before.size());
        } finally {
            cacheManager.removeByKey(selectorId);
        }
        Assertions.assertNull(cacheManager.findUpstreamListBySelectorId(selectorId),
                "removeByKey must clear the published snapshot");
    }

    @Test
    public void testHealthTransitionRepublishesSnapshot() {
        final UpstreamCacheManager cacheManager = UpstreamCacheManager.getInstance();
        final String selectorId = "SNAPSHOT_HEALTH_TRANSITION_TEST";
        List<Upstream> upstreams = buildUpstreams(2);
        cacheManager.submit(selectorId, upstreams);
        try {
            UpstreamCheckTask task = getUpstreamCheckTask(cacheManager);
            Assertions.assertNotNull(task);
            // simulate a health transition: healthy -> unhealthy
            upstreams.get(0).setHealthy(false);
            task.putToMap(task.getUnhealthyUpstream(), selectorId, upstreams.get(0));
            task.removeFromMap(task.getHealthyUpstream(), selectorId, upstreams.get(0));
            List<Upstream> afterDown = cacheManager.findUpstreamListBySelectorId(selectorId);
            Assertions.assertEquals(1, Objects.requireNonNull(afterDown).size());
            // and back: unhealthy -> healthy
            upstreams.get(0).setHealthy(true);
            task.putToMap(task.getHealthyUpstream(), selectorId, upstreams.get(0));
            task.removeFromMap(task.getUnhealthyUpstream(), selectorId, upstreams.get(0));
            List<Upstream> afterUp = cacheManager.findUpstreamListBySelectorId(selectorId);
            Assertions.assertEquals(2, Objects.requireNonNull(afterUp).size());
        } finally {
            cacheManager.removeByKey(selectorId);
        }
    }

    @Test
    public void testConcurrentReadersAndWritersNeverSeeTornState() throws InterruptedException {
        final UpstreamCacheManager cacheManager = UpstreamCacheManager.getInstance();
        final String selectorId = "SNAPSHOT_CONCURRENCY_TEST";
        final int baseSize = 20;
        cacheManager.submit(selectorId, buildUpstreams(baseSize));
        UpstreamCheckTask task = getUpstreamCheckTask(cacheManager);
        Assertions.assertNotNull(task);
        final List<Upstream> churnUpstreams = buildUpstreams(5, "churn");
        final List<Throwable> failures = new CopyOnWriteArrayList<>();
        final AtomicBoolean stop = new AtomicBoolean(false);
        final CountDownLatch started = new CountDownLatch(6);
        ExecutorService pool = Executors.newFixedThreadPool(6);
        try {
            for (int r = 0; r < 4; r++) {
                pool.execute(newReaderLoop(cacheManager, selectorId, failures, stop, started));
            }
            pool.execute(newChurnWriterLoop(task, selectorId, churnUpstreams, failures, stop, started));
            pool.execute(newConfigChurnWriterLoop(cacheManager, selectorId, baseSize, failures, stop, started));
            Assertions.assertTrue(started.await(5, TimeUnit.SECONDS));
            TimeUnit.MILLISECONDS.sleep(1500);
            stop.set(true);
        } finally {
            pool.shutdown();
            Assertions.assertTrue(pool.awaitTermination(10, TimeUnit.SECONDS));
        }
        Assertions.assertTrue(failures.isEmpty(), "concurrent readers/writers must not fail: " + failures);
        // after churn ends, removed targets must not remain routing candidates
        for (Upstream churn : churnUpstreams) {
            task.triggerRemoveOne(selectorId, churn);
        }
        List<Upstream> finalSnapshot = cacheManager.findUpstreamListBySelectorId(selectorId);
        Assertions.assertNotNull(finalSnapshot);
        for (Upstream churn : churnUpstreams) {
            Assertions.assertTrue(finalSnapshot.stream().noneMatch(u -> u.getUrl().equals(churn.getUrl())),
                    "removed churn target must not remain a routing candidate");
        }
        cacheManager.removeByKey(selectorId);
    }

    @Test
    @SuppressWarnings("deprecation")
    public void testSnapshotLookupAllocatesFarLessThanPerRequestCopy() {
        final UpstreamCacheManager cacheManager = UpstreamCacheManager.getInstance();
        final String selectorId = "SNAPSHOT_ALLOCATION_TEST";
        final int upstreamCount = 100;
        final int iterations = 50_000;
        cacheManager.submit(selectorId, buildUpstreams(upstreamCount));
        try {
            UpstreamCheckTask task = getUpstreamCheckTask(cacheManager);
            Assertions.assertNotNull(task);
            java.lang.management.ThreadMXBean bean = ManagementFactory.getThreadMXBean();
            Assumptions.assumeTrue(bean instanceof com.sun.management.ThreadMXBean,
                    "thread allocation measurement not available on this JVM");
            com.sun.management.ThreadMXBean sunBean = (com.sun.management.ThreadMXBean) bean;
            Assumptions.assumeTrue(sunBean.isThreadAllocatedMemorySupported());
            sunBean.setThreadAllocatedMemoryEnabled(true);
            long threadId = Thread.currentThread().getId();

            long sink = 0;
            // warm up both paths
            for (int i = 0; i < 5_000; i++) {
                sink += cacheManager.findUpstreamListBySelectorId(selectorId).size();
                sink += new ArrayList<>(task.getHealthyUpstreamListBySelectorId(selectorId)).size();
            }

            long before = sunBean.getThreadAllocatedBytes(threadId);
            for (int i = 0; i < iterations; i++) {
                sink += cacheManager.findUpstreamListBySelectorId(selectorId).size();
            }
            final long snapshotAllocation = sunBean.getThreadAllocatedBytes(threadId) - before;

            before = sunBean.getThreadAllocatedBytes(threadId);
            for (int i = 0; i < iterations; i++) {
                sink += new ArrayList<>(task.getHealthyUpstreamListBySelectorId(selectorId)).size();
            }
            final long copyAllocation = sunBean.getThreadAllocatedBytes(threadId) - before;

            Assertions.assertTrue(sink > 0);
            Assertions.assertTrue(copyAllocation > 0, "the legacy per-request copy must allocate");
            Assertions.assertTrue(snapshotAllocation * 10 < copyAllocation,
                    "snapshot lookup should be allocation-free compared to the copy, snapshot="
                            + snapshotAllocation + " bytes, copy=" + copyAllocation + " bytes");
        } finally {
            cacheManager.removeByKey(selectorId);
        }
    }

    private Runnable newReaderLoop(final UpstreamCacheManager cacheManager, final String selectorId,
                                   final List<Throwable> failures, final AtomicBoolean stop, final CountDownLatch started) {
        return () -> {
            started.countDown();
            long sink = 0;
            while (!stop.get()) {
                try {
                    List<Upstream> snapshot = cacheManager.findUpstreamListBySelectorId(selectorId);
                    if (Objects.nonNull(snapshot)) {
                        for (Upstream upstream : snapshot) {
                            sink += upstream.getUrl().length();
                        }
                    }
                } catch (Throwable t) {
                    failures.add(t);
                    return;
                }
            }
            if (sink < 0) {
                failures.add(new IllegalStateException("unreachable"));
            }
        };
    }

    private Runnable newChurnWriterLoop(final UpstreamCheckTask task, final String selectorId,
                                        final List<Upstream> churnUpstreams, final List<Throwable> failures,
                                        final AtomicBoolean stop, final CountDownLatch started) {
        return () -> {
            started.countDown();
            while (!stop.get()) {
                try {
                    for (Upstream churn : churnUpstreams) {
                        task.triggerAddOne(selectorId, churn);
                        task.triggerRemoveOne(selectorId, churn);
                    }
                } catch (Throwable t) {
                    failures.add(t);
                    return;
                }
            }
        };
    }

    private Runnable newConfigChurnWriterLoop(final UpstreamCacheManager cacheManager, final String selectorId,
                                              final int baseSize, final List<Throwable> failures,
                                              final AtomicBoolean stop, final CountDownLatch started) {
        return () -> {
            started.countDown();
            while (!stop.get()) {
                try {
                    // configuration churn: full replacement and re-submission
                    cacheManager.submit(selectorId, buildUpstreams(baseSize));
                    TimeUnit.MILLISECONDS.sleep(5);
                } catch (Throwable t) {
                    failures.add(t);
                    return;
                }
            }
        };
    }

    private List<Upstream> buildUpstreams(final int count) {
        return buildUpstreams(count, "upstream");
    }

    private List<Upstream> buildUpstreams(final int count, final String prefix) {
        List<Upstream> upstreams = new ArrayList<>(count);
        for (int i = 0; i < count; i++) {
            upstreams.add(Upstream.builder()
                    .protocol("http://")
                    .url(prefix + i + ":8080")
                    .status(true)
                    .healthCheckEnabled(false)
                    .build());
        }
        return upstreams;
    }

    private UpstreamCheckTask getUpstreamCheckTask(final UpstreamCacheManager manager) {
        try {
            Field field = UpstreamCacheManager.class.getDeclaredField("task");
            field.setAccessible(true);
            return (UpstreamCheckTask) field.get(manager);
        } catch (Exception e) {
            return null;
        }
    }
}
