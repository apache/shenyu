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

package org.apache.shenyu.common.healthcheck;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import lombok.extern.slf4j.Slf4j;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.DivideUpstream;
import org.apache.shenyu.common.utils.CollectionUtils;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.UpstreamCheckUtils;

import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;

/**
 * Health check manager for upstream servers.
 */
@Slf4j
public final class HealthCheckTask implements Runnable {

    private final Map<String, List<DivideUpstream>> healthyUpstream = Maps.newConcurrentMap();

    private final Map<String, List<DivideUpstream>> unhealthyUpstream = Maps.newConcurrentMap();

    private final Map<String, SelectorData> selectorCache = Maps.newConcurrentMap();

    private final Object lock = new Object();

    private final AtomicBoolean checkStarted = new AtomicBoolean(false);

    private final List<CompletableFuture<UpstreamWithSelectorId>> futures = Lists.newArrayList();

    private final int checkInterval;

    private ExecutorService executor;

    private int checkTimeout = 3000;

    private int healthyThreshold = 1;

    private int unhealthyThreshold = 1;

    public HealthCheckTask(final int checkInterval) {
        this.checkInterval = checkInterval;
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
        executor = new ScheduledThreadPoolExecutor(10, requestFactory);
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
        try {
            // If there is no synchronized. when check is done and all upstream check result is in the futures list.
            // In the same time, triggerRemoveAll() called before waitFinish(), there will be dirty data stay in map.
            synchronized (lock) {
                if (tryStartHealthCheck()) {
                    doHealthCheck();
                    waitFinish();
                }
            }
        } catch (Exception e) {
            log.error("[Health Check] Meet problem: ", e);
        } finally {
            finishHealthCheck();
        }
    }

    private void doHealthCheck() {
        check(healthyUpstream);
        check(unhealthyUpstream);
    }

    private void check(final Map<String, List<DivideUpstream>> map) {
        for (Map.Entry<String, List<DivideUpstream>> entry : map.entrySet()) {
            String key = entry.getKey();
            List<DivideUpstream> value = entry.getValue();
            for (DivideUpstream upstream : value) {
                CompletableFuture<UpstreamWithSelectorId> future = CompletableFuture.supplyAsync(() -> check(key, upstream), executor);

                futures.add(future);
            }
        }
    }

    private UpstreamWithSelectorId check(final String selectorId, final DivideUpstream upstream) {
        SelectorData selectorData = selectorCache.get(selectorId);
        boolean pass = UpstreamCheckUtils.checkUrl(upstream.getUpstreamUrl(), checkTimeout);
        if (pass) {
            if (upstream.isHealthy()) {
                upstream.setLastHealthTimestamp(System.currentTimeMillis());
            } else {
                long now = System.currentTimeMillis();
                long interval = now - upstream.getLastUnhealthyTimestamp();
                if (interval >= (long) checkInterval * healthyThreshold) {
                    upstream.setHealthy(true);
                    upstream.setLastHealthTimestamp(now);
                    log.info("[Health Check] Selector [{}] upstream {} health check passed, server is back online.",
                            selectorData.getName(), upstream.getUpstreamUrl());
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
                    log.info("[Health Check] Selector [{}] upstream {} health check failed, server is offline.",
                            selectorData.getName(), upstream.getUpstreamUrl());
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
        DivideUpstream upstream = entity.getDivideUpstream();
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
     * @param selectorData selectorData
     * @param upstream     upstream
     */
    public void triggerAddOne(final SelectorData selectorData, final DivideUpstream upstream) {
        selectorCache.putIfAbsent(selectorData.getId(), selectorData);
        putToMap(healthyUpstream, selectorData.getId(), upstream);
    }

    /**
     * Remove a specific upstream via selectorData.
     *
     * @param selectorData selectorData
     * @param upstream     upstream
     */
    public void triggerRemoveOne(final SelectorData selectorData, final DivideUpstream upstream) {
        triggerRemoveOne(selectorData.getId(), upstream);
    }

    /**
     * Remove a specific upstream via selectorId.
     *
     * @param selectorId selectorId
     * @param upstream   upstream
     */
    public void triggerRemoveOne(final String selectorId, final DivideUpstream upstream) {
        removeFromMap(healthyUpstream, selectorId, upstream);
        removeFromMap(unhealthyUpstream, selectorId, upstream);

        SelectorData selectorData = selectorCache.get(selectorId);
        log.info("[Health Check] Selector [{}] upstream {} was removed.", selectorData.getName(), upstream.getUpstreamUrl());
    }

    private void putToMap(final Map<String, List<DivideUpstream>> map, final String selectorId, final DivideUpstream upstream) {
        synchronized (lock) {
            List<DivideUpstream> list = map.computeIfAbsent(selectorId, k -> Lists.newArrayList());
            if (!list.contains(upstream)) {
                list.add(upstream);
            }
        }
    }

    private void removeFromMap(final Map<String, List<DivideUpstream>> map, final String selectorId, final DivideUpstream upstream) {
        synchronized (lock) {
            List<DivideUpstream> list = map.get(selectorId);
            if (CollectionUtils.isNotEmpty(list)) {
                list.remove(upstream);
            }
        }
    }

    /**
     * Remove all upstream via selectorData.
     *
     * @param selectorData selectorData
     */
    public void triggerRemoveAll(final SelectorData selectorData) {
        triggerRemoveAll(selectorData.getId());
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

        SelectorData selectorData = selectorCache.get(selectorId);
        log.info("[Health Check] Selector [{}] all upstream as removed.", selectorData.getName());

        selectorCache.remove(selectorId);
    }

    /**
     * Print healthy upstream.
     */
    public void printHealthyUpstream() {
        healthyUpstream.forEach((k, v) -> {
            if (v != null) {
                SelectorData selectorData = selectorCache.get(k);
                List<String> list = v.stream().map(DivideUpstream::getUpstreamUrl).collect(Collectors.toList());
                log.info("[Health Check] Selector [{}] currently healthy upstream: {}",
                        selectorData.getName(), GsonUtils.getInstance().toJson(list));
            }
        });
    }

    /**
     * Print unhealthy upstream.
     */
    public void printUnhealthyUpstream() {
        unhealthyUpstream.forEach((k, v) -> {
            if (v != null) {
                SelectorData selectorData = selectorCache.get(k);
                List<String> list = v.stream().map(DivideUpstream::getUpstreamUrl).collect(Collectors.toList());
                log.info("[Health Check] Selector [{}] currently unhealthy upstream: {}",
                        selectorData.getName(), GsonUtils.getInstance().toJson(list));
            }
        });
    }

    /**
     * Get healthy upstream map.
     *
     * @return healthy map.
     */
    public Map<String, List<DivideUpstream>> getHealthyUpstream() {
        return healthyUpstream;
    }

    /**
     * Get unhealthy upstream map.
     *
     * @return unhealthy map.
     */
    public Map<String, List<DivideUpstream>> getUnhealthyUpstream() {
        return unhealthyUpstream;
    }
}
