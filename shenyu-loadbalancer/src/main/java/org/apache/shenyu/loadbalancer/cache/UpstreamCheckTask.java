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
import org.apache.shenyu.common.utils.UpstreamCheckUtils;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Map;
import java.util.Objects;
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

    private int checkTimeout = 3000;

    private int healthyThreshold = 1;

    private int unhealthyThreshold = 1;
    
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
            /*
             * If there is no synchronized. when check is done and all upstream check result is in the futures list.
             * In the same time, triggerRemoveAll() called before waitFinish(), there will be dirty data stay in map.
             */
            synchronized (lock) {
                if (tryStartHealthCheck()) {
                    doHealthCheck();
                    waitFinish();
                }
            }
        } catch (Exception e) {
            LOG.error("[Health Check] Meet problem: ", e);
        } finally {
            finishHealthCheck();
        }
    }

    private void doHealthCheck() {
        check(healthyUpstream);
        check(unhealthyUpstream);
    }

    private void check(final Map<String, List<Upstream>> map) {
        for (Map.Entry<String, List<Upstream>> entry : map.entrySet()) {
            String key = entry.getKey();
            List<Upstream> value = entry.getValue();
            for (Upstream upstream : value) {
                CompletableFuture<UpstreamWithSelectorId> future = CompletableFuture.supplyAsync(() -> check(key, upstream), executor);
                futures.add(future);
            }
        }
    }

    private UpstreamWithSelectorId check(final String selectorId, final Upstream upstream) {
        boolean pass = UpstreamCheckUtils.checkUrl(upstream.getUrl(), checkTimeout);
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

    private void putToMap(final Map<String, List<Upstream>> map, final String selectorId, final Upstream upstream) {
        synchronized (lock) {
            List<Upstream> list = map.computeIfAbsent(selectorId, k -> Lists.newArrayList());
            if (!list.contains(upstream)) {
                list.add(upstream);
            }
        }
    }

    private void removeFromMap(final Map<String, List<Upstream>> map, final String selectorId, final Upstream upstream) {
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
     * Get unhealthy upstream map.
     *
     * @return unhealthy map.
     */
    public Map<String, List<Upstream>> getUnhealthyUpstream() {
        return unhealthyUpstream;
    }
}
