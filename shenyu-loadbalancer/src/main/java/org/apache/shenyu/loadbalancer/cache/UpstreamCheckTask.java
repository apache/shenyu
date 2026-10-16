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

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.MapUtils;
import org.apache.shenyu.common.utils.UpstreamCheckUtils;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Collectors;

/**
 * Health check manager for upstream servers.
 */
public final class UpstreamCheckTask implements Runnable {

    /**
     * logger.
     */
    private static final Logger LOG = LoggerFactory.getLogger(UpstreamCheckTask.class);

    private final Map<String, List<Upstream>> healthyUpstream = Maps.newConcurrentMap();

    private final Map<String, List<Upstream>> unhealthyUpstream = Maps.newConcurrentMap();

    private final Object lock = new Object();

    private final AtomicBoolean checkStarted = new AtomicBoolean(false);

    private final List<CompletableFuture<UpstreamWithSelectorId>> futures = Lists.newArrayList();

    private final int checkInterval;

    private ExecutorService executor;

    private int poolSize;

    private int checkTimeout = 3000;

    private int healthyThreshold = 1;

    private int unhealthyThreshold = 1;

    private volatile long lastRoundDurationMillis;

    private volatile int lastRoundSubmittedChecks;

    private volatile int lastRoundPendingChecks;

    private volatile long lastRoundMaxEndpointLatencyMillis;

    private volatile long lastRoundOldestHealthStateMillis;

    private volatile long lastRoundCompletedAtMillis;

    private final AtomicLong totalRounds = new AtomicLong();

    private final AtomicLong totalSubmittedChecks = new AtomicLong();

    private final AtomicLong totalEndpointLatencyMillis = new AtomicLong();
    
    /**
     * Instantiates a new Upstream check task.
     *
     * @param checkInterval the check interval
     */
    public UpstreamCheckTask(final int checkInterval) {
        this.checkInterval = checkInterval;
    }
    
    /**
     * get checkStarted.
     *
     * @return checkStarted check started
     */
    public AtomicBoolean getCheckStarted() {
        return checkStarted;
    }
    
    /**
     * Schedule health check task.
     */
    public void schedule() {
        // executor for health check
        ThreadFactory healthCheckFactory = ShenyuThreadFactory.create("upstream-health-check", true);
        new ScheduledThreadPoolExecutor(1, healthCheckFactory)
                .scheduleWithFixedDelay(this, 3000, checkInterval, TimeUnit.MILLISECONDS);

        // executor for async request, avoid request block health check thread
        ThreadFactory requestFactory = ShenyuThreadFactory.create("upstream-health-check-request", true);
        executor = new ScheduledThreadPoolExecutor(poolSize, requestFactory);
    }
    
    /**
     * Set check timeout.
     *
     * @param checkTimeout milliseconds
     */
    public void setCheckTimeout(final int checkTimeout) {
        this.checkTimeout = checkTimeout;
    }

    /**
     * get checkThreadPoolSize.
     *
     * @return checkThreadPoolSize
     */
    public int getPoolSize() {
        return poolSize;
    }

    /**
     * set checkThreadPoolSize.
     *
     * @param poolSize checkThreadPoolSize
     */
    public void setPoolSize(final int poolSize) {
        this.poolSize = poolSize;
    }

    /**
     * Set healthy threshold.
     *
     * @param healthyThreshold healthy threshold
     */
    public void setHealthyThreshold(final int healthyThreshold) {
        this.healthyThreshold = healthyThreshold;
    }
    
    /**
     * Set unhealthy threshold.
     *
     * @param unhealthyThreshold unhealthy threshold
     */
    public void setUnhealthyThreshold(final int unhealthyThreshold) {
        this.unhealthyThreshold = unhealthyThreshold;
    }

    @Override
    public void run() {
        healthCheck();
    }

    private void healthCheck() {
        final long startNanos = System.nanoTime();
        // Per-round scratch counters. maxEndpointLatencyMillis is mutated by the async probe
        // tasks running on the executor, so it stays an AtomicLong captured by the lambdas.
        final AtomicLong maxEndpointLatencyMillis = new AtomicLong();
        int submitted = 0;
        int pendingAtWait = 0;
        try {
            /*
             * If there is no synchronized. when check is done and all upstream check result is in the futures list.
             * In the same time, triggerRemoveAll() called before waitFinish(), there will be dirty data stay in map.
             */
            synchronized (lock) {
                if (tryStartHealthCheck()) {
                    submitted = doHealthCheck(maxEndpointLatencyMillis);
                    pendingAtWait = countPendingFutures();
                    waitFinish();
                }
            }
        } catch (Exception e) {
            LOG.error("[Health Check] Meet problem: ", e);
        } finally {
            finishHealthCheck();
            recordRoundMetrics(startNanos, submitted, pendingAtWait, maxEndpointLatencyMillis.get());
        }
    }

    private int countPendingFutures() {
        int pending = 0;
        for (CompletableFuture<UpstreamWithSelectorId> future : futures) {
            if (!future.isDone()) {
                pending++;
            }
        }
        return pending;
    }

    private void recordRoundMetrics(final long startNanos, final int submitted, final int pendingAtWait,
                                    final long maxEndpointLatencyMillis) {
        final long durationMillis = TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - startNanos);
        final long oldestHealthStateMillis = computeOldestHealthStateMillis();
        final long completedAtMillis = System.currentTimeMillis();
        this.lastRoundDurationMillis = durationMillis;
        this.lastRoundSubmittedChecks = submitted;
        this.lastRoundPendingChecks = pendingAtWait;
        this.lastRoundMaxEndpointLatencyMillis = maxEndpointLatencyMillis;
        this.lastRoundOldestHealthStateMillis = oldestHealthStateMillis;
        this.lastRoundCompletedAtMillis = completedAtMillis;
        totalRounds.incrementAndGet();
        totalSubmittedChecks.addAndGet(submitted);
        if (LOG.isDebugEnabled()) {
            LOG.debug("[Health Check] round summary: durationMs={}, submitted={}, pendingAtWait={}, "
                            + "maxEndpointLatencyMs={}, oldestHealthStateMs={}, healthySelectors={}, unhealthySelectors={}",
                    durationMillis, submitted, pendingAtWait, maxEndpointLatencyMillis, oldestHealthStateMillis,
                    healthyUpstream.size(), unhealthyUpstream.size());
        }
    }

    private long computeOldestHealthStateMillis() {
        final long now = System.currentTimeMillis();
        long oldest = 0L;
        for (List<Upstream> bucket : healthyUpstream.values()) {
            for (Upstream upstream : bucket) {
                final long age = now - upstream.getLastHealthTimestamp();
                if (age > oldest) {
                    oldest = age;
                }
            }
        }
        for (List<Upstream> bucket : unhealthyUpstream.values()) {
            for (Upstream upstream : bucket) {
                final long age = now - upstream.getLastUnhealthyTimestamp();
                if (age > oldest) {
                    oldest = age;
                }
            }
        }
        return oldest;
    }

    private int doHealthCheck(final AtomicLong maxEndpointLatencyMillis) {
        return check(healthyUpstream, maxEndpointLatencyMillis) + check(unhealthyUpstream, maxEndpointLatencyMillis);
    }

    private int check(final Map<String, List<Upstream>> map, final AtomicLong maxEndpointLatencyMillis) {
        int submitted = 0;
        for (Map.Entry<String, List<Upstream>> entry : map.entrySet()) {
            String key = entry.getKey();
            List<Upstream> value = entry.getValue();
            for (Upstream upstream : value) {
                CompletableFuture<UpstreamWithSelectorId> future =
                        CompletableFuture.supplyAsync(() -> check(key, upstream, maxEndpointLatencyMillis), executor);
                futures.add(future);
                submitted++;
            }
        }
        return submitted;
    }

    private UpstreamWithSelectorId check(final String selectorId, final Upstream upstream,
                                         final AtomicLong maxEndpointLatencyMillis) {
        if (!upstream.isHealthCheckEnabled()) {
            if (!upstream.isHealthy()) {
                upstream.setHealthy(true);
                upstream.setLastHealthTimestamp(System.currentTimeMillis());
            }
            return new UpstreamWithSelectorId(selectorId, upstream);
        }
        final long probeStart = System.nanoTime();
        boolean pass;
        try {
            pass = UpstreamCheckUtils.checkUrl(upstream.getUrl(), checkTimeout);
        } finally {
            final long latencyMillis = TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - probeStart);
            maxEndpointLatencyMillis.accumulateAndGet(latencyMillis, Math::max);
            totalEndpointLatencyMillis.addAndGet(latencyMillis);
        }
        if (pass) {
            if (upstream.isHealthy()) {
                upstream.setLastHealthTimestamp(System.currentTimeMillis());
            } else {
                long now = System.currentTimeMillis();
                long interval = now - upstream.getLastUnhealthyTimestamp();
                if (interval >= (long) checkInterval * healthyThreshold) {
                    upstream.setHealthy(true);
                    upstream.setLastHealthTimestamp(now);
                    LOG.info("[Health Check] Selector [{}] upstream {} health check passed, server is back online.",
                            selectorId, upstream.getUrl());
                }
            }
        } else {
            if (!upstream.isHealthy()) {
                upstream.setLastUnhealthyTimestamp(System.currentTimeMillis());
            } else {
                long now = System.currentTimeMillis();
                long interval = now - upstream.getLastHealthTimestamp();
                if (interval >= (long) checkInterval * unhealthyThreshold) {
                    upstream.setHealthy(false);
                    upstream.setLastUnhealthyTimestamp(now);
                    LOG.info("[Health Check] Selector [{}] upstream {} health check failed, server is offline.",
                            selectorId, upstream.getUrl());
                }
            }
        }
        return new UpstreamWithSelectorId(selectorId, upstream);
    }

    private boolean tryStartHealthCheck() {
        return checkStarted.compareAndSet(false, true);
    }

    private void waitFinish() throws ExecutionException, InterruptedException {
        for (CompletableFuture<UpstreamWithSelectorId> future : futures) {
            UpstreamWithSelectorId entity = future.get();
            putEntityToMap(entity);
        }

        futures.clear();
    }

    private void putEntityToMap(final UpstreamWithSelectorId entity) {
        Upstream upstream = entity.getUpstream();
        if (upstream.isHealthy()) {
            putToMap(healthyUpstream, entity.getSelectorId(), upstream);
            removeFromMap(unhealthyUpstream, entity.getSelectorId(), upstream);
        } else {
            putToMap(unhealthyUpstream, entity.getSelectorId(), upstream);
            removeFromMap(healthyUpstream, entity.getSelectorId(), upstream);
        }
    }

    private void finishHealthCheck() {
        checkStarted.set(false);
    }
    
    /**
     * Add one upstream via selectorData.
     *
     * @param selectorId selectorId
     * @param upstream upstream
     */
    public void triggerAddOne(final String selectorId, final Upstream upstream) {
        putToMap(healthyUpstream, selectorId, upstream);
    }

    /**
     * Remove a specific upstream via selectorId.
     *
     * @param selectorId selectorId
     * @param upstream upstream
     */
    public void triggerRemoveOne(final String selectorId, final Upstream upstream) {
        removeFromMap(healthyUpstream, selectorId, upstream);
        removeFromMap(unhealthyUpstream, selectorId, upstream);
    }

    /**
     * Put upstream to specified map (for preserving health status).
     *
     * @param map the map to put upstream
     * @param selectorId the selector id
     * @param upstream the upstream
     */
    public void putToMap(final Map<String, List<Upstream>> map, final String selectorId, final Upstream upstream) {
        synchronized (lock) {
            List<Upstream> list = MapUtils.computeIfAbsent(map, selectorId, k -> new CopyOnWriteArrayList<>());
            if (!list.contains(upstream)) {
                list.add(upstream);
            }
        }
    }

    /**
     * Remove upstream from specified map.
     *
     * @param map the map to remove upstream from
     * @param selectorId the selector id
     * @param upstream the upstream
     */
    public void removeFromMap(final Map<String, List<Upstream>> map, final String selectorId, final Upstream upstream) {
        synchronized (lock) {
            List<Upstream> list = map.get(selectorId);
            if (CollectionUtils.isNotEmpty(list)) {
                list.remove(upstream);
            }
        }
    }
    
    /**
     * Remove all upstream via selectorId.
     *
     * @param selectorId selectorId
     */
    public void triggerRemoveAll(final String selectorId) {
        synchronized (lock) {
            healthyUpstream.remove(selectorId);
            unhealthyUpstream.remove(selectorId);
        }
    }
    
    /**
     * Print healthy and unhealthy check log.
     */
    public void print() {
        printHealthyUpstream();
        printUnhealthyUpstream();
    }
    
    private void printHealthyUpstream() {
        healthyUpstream.forEach((k, v) -> {
            if (Objects.nonNull(v)) {
                List<String> list = v.stream().map(Upstream::getUrl).collect(Collectors.toList());
                LOG.info("[Health Check] currently healthy upstream: {}", GsonUtils.getInstance().toJson(list));
            }
        });
    }
    
    private void printUnhealthyUpstream() {
        unhealthyUpstream.forEach((k, v) -> {
            if (Objects.nonNull(v)) {
                List<String> list = v.stream().map(Upstream::getUrl).collect(Collectors.toList());
                LOG.info("[Health Check] currently unhealthy upstream: {}", GsonUtils.getInstance().toJson(list));
            }
        });
    }
    
    /**
     * Get healthy upstream map.
     *
     * @return healthy map.
     */
    public Map<String, List<Upstream>> getHealthyUpstream() {
        return healthyUpstream;
    }

    /**
     * Get healthy upstream list.
     * @param selectorId selectorId
     * @return healthy upstream list.
     */
    public List<Upstream> getHealthyUpstreamListBySelectorId(final String selectorId) {
        return getHealthyUpstream().get(selectorId);
    }
    
    /**
     * Get unhealthy upstream map.
     *
     * @return unhealthy map.
     */
    public Map<String, List<Upstream>> getUnhealthyUpstream() {
        return unhealthyUpstream;
    }

    /**
     * Duration of the most recent health-check round in milliseconds, measured with
     * {@link System#nanoTime()} around the whole synchronized section (submit + wait).
     * Zero until the first round completes.
     *
     * @return last round duration in millis
     */
    public long getLastRoundDurationMillis() {
        return lastRoundDurationMillis;
    }

    /**
     * Number of endpoint probes submitted to the async executor during the most recent round.
     *
     * @return submitted probe count
     */
    public int getLastRoundSubmittedChecks() {
        return lastRoundSubmittedChecks;
    }

    /**
     * Number of submitted probes that were still pending when the round entered {@code waitFinish()}.
     * A persistently high value indicates the executor is the bottleneck, not the endpoints.
     *
     * @return pending probe count at wait time
     */
    public int getLastRoundPendingChecks() {
        return lastRoundPendingChecks;
    }

    /**
     * Maximum single-endpoint probe latency observed during the most recent round, in milliseconds.
     *
     * @return max endpoint latency in millis
     */
    public long getLastRoundMaxEndpointLatencyMillis() {
        return lastRoundMaxEndpointLatencyMillis;
    }

    /**
     * Age of the oldest health-state timestamp across healthy and unhealthy buckets, in milliseconds.
     * Grows when rounds fall behind the configured interval.
     *
     * @return oldest health-state age in millis
     */
    public long getLastRoundOldestHealthStateMillis() {
        return lastRoundOldestHealthStateMillis;
    }

    /**
     * Wall-clock timestamp at which the most recent round finished, in milliseconds since epoch.
     *
     * @return last round completion timestamp
     */
    public long getLastRoundCompletedAtMillis() {
        return lastRoundCompletedAtMillis;
    }

    /**
     * Total number of health-check rounds completed since this task was created.
     *
     * @return total rounds
     */
    public long getTotalRounds() {
        return totalRounds.get();
    }

    /**
     * Total number of endpoint probes submitted since this task was created.
     *
     * @return total submitted probes
     */
    public long getTotalSubmittedChecks() {
        return totalSubmittedChecks.get();
    }

    /**
     * Cumulative probe latency in milliseconds since this task was created. Divide by
     * {@link #getTotalSubmittedChecks()} for an average; only probes with health-check
     * enabled contribute.
     *
     * @return cumulative endpoint latency in millis
     */
    public long getTotalEndpointLatencyMillis() {
        return totalEndpointLatencyMillis.get();
    }
}
