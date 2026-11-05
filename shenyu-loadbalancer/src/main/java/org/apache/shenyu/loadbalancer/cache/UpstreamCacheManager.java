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
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.config.ShenyuConfig.UpstreamCheck;
import org.apache.shenyu.common.utils.MapUtils;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.loadbalancer.entity.UpstreamIdentity;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.function.Predicate;
import java.util.stream.Collectors;

/**
 * this is upstream .
 */
public final class UpstreamCacheManager {

    private static final UpstreamCacheManager INSTANCE = new UpstreamCacheManager();

    private static final Map<String, List<Upstream>> UPSTREAM_MAP = Maps.newConcurrentMap();

    // Include unhealthy configured gray nodes so legacy traffic never falls back to ordinary nodes.
    private static final Map<String, Boolean> LEGACY_GRAY_SELECTORS = Maps.newConcurrentMap();

    private UpstreamCheckTask task;

    /**
     * health check parameters.
     */
    private Boolean checkEnable;

    private int poolSize;

    private int checkTimeout;

    private int checkInterval;

    private int healthyThreshold;

    private int unhealthyThreshold;

    /**
     * healthy upstream print parameters.
     */
    private Boolean printEnable;

    private Integer printInterval;

    private UpstreamCacheManager() {
        initHealthCheck();
    }

    private void initHealthCheck() {
        ShenyuConfig shenyuConfig = Optional.ofNullable(Singleton.INST.get(ShenyuConfig.class)).orElse(new ShenyuConfig());
        UpstreamCheck upstreamCheck = shenyuConfig.getUpstreamCheck();
        checkEnable = upstreamCheck.getEnabled();
        poolSize = upstreamCheck.getPoolSize();
        checkTimeout = upstreamCheck.getTimeout();
        healthyThreshold = upstreamCheck.getHealthyThreshold();
        unhealthyThreshold = upstreamCheck.getUnhealthyThreshold();
        checkInterval = upstreamCheck.getInterval();
        printEnable = upstreamCheck.getPrintEnabled();
        printInterval = upstreamCheck.getPrintInterval();
        createTask();
        scheduleHealthCheck();
    }

    private void createTask() {
        task = new UpstreamCheckTask(checkInterval);
        task.setPoolSize(poolSize);
        task.setCheckTimeout(checkTimeout);
        task.setHealthyThreshold(healthyThreshold);
        task.setUnhealthyThreshold(unhealthyThreshold);
    }

    private void scheduleHealthCheck() {
        if (checkEnable) {
            task.schedule();
            // executor for log print
            if (printEnable) {
                ThreadFactory printFactory = ShenyuThreadFactory.create("upstream-health-print", true);
                new ScheduledThreadPoolExecutor(1, printFactory)
                        .scheduleWithFixedDelay(task::print, printInterval, printInterval, TimeUnit.MILLISECONDS);
            }
        }
    }

    /**
     * Gets instance.
     *
     * @return the instance
     */
    public static UpstreamCacheManager getInstance() {
        return INSTANCE;
    }

    /**
     * Find upstream list by selector id.
     *
     * <p>Returns the read-only snapshot published by the health-check task on
     * configuration and health-state updates, so the request path does not copy
     * the healthy list per lookup. Legacy gray-exclusive selectors instead get a
     * filtered view narrowed to their gray upstreams. Callers must not mutate the
     * returned list either way; elements keep the same shared-instance semantics
     * as before.</p>
     *
     * @param selectorId the selector id
     * @return the healthy upstreams for the selector, or null when it has no entry
     */
    public List<Upstream> findUpstreamListBySelectorId(final String selectorId) {
        List<Upstream> upstreamList = findAllUpstreamListBySelectorId(selectorId);
        if (Objects.isNull(upstreamList) || !Boolean.TRUE.equals(LEGACY_GRAY_SELECTORS.get(selectorId))) {
            return upstreamList;
        }
        return upstreamList.stream().filter(Upstream::isGray).collect(Collectors.toList());
    }

    /**
     * Find all healthy upstreams for rules that select their own label partition.
     * The regular lookup retains the legacy gray-exclusive policy, including for failover.
     *
     * <p>Returns the same read-only snapshot that backs
     * {@link #findUpstreamListBySelectorId} before gray narrowing, so the canary
     * path also avoids a per-request copy. Callers must not mutate the returned
     * list; elements keep the same shared-instance semantics as before.</p>
     *
     * @param selectorId selector identifier
     * @return a read-only snapshot of all healthy upstreams, or null when the selector has no healthy pool
     */
    public List<Upstream> findAllUpstreamListBySelectorId(final String selectorId) {
        return task.getHealthySnapshot(selectorId);
    }

    /**
     * Remove by key.
     *
     * @param key the key
     */
    public void removeByKey(final String key) {
        task.withLock(() -> {
            UPSTREAM_MAP.remove(key);
            LEGACY_GRAY_SELECTORS.remove(key);
            task.triggerRemoveAll(key);
        });
    }

    /**
     * Submit .
     *
     * @param selectorId   the selector id
     * @param upstreamList the upstream list
     */
    public void submit(final String selectorId, final List<Upstream> upstreamList) {
        task.withLock(() -> doSubmit(selectorId, upstreamList));
    }

    private void doSubmit(final String selectorId, final List<Upstream> upstreamList) {
        List<Upstream> actualUpstreamList = Objects.isNull(upstreamList) ? Lists.newArrayList() : upstreamList;

        // Check if the list is empty first to avoid unnecessary processing
        if (actualUpstreamList.isEmpty()) {
            removeByKey(selectorId);
            return;
        }

        LEGACY_GRAY_SELECTORS.put(selectorId, actualUpstreamList.stream().anyMatch(Upstream::isGray));
        initializeUpstreamHealthStatus(actualUpstreamList);

        Predicate<Upstream> admitted = upstream -> !upstream.isDraining() && upstream.isReady();
        Map<Boolean, List<Upstream>> partitionedUpstreams = actualUpstreamList.stream()
                .collect(Collectors.partitioningBy(upstream -> upstream.isStatus() && admitted.test(upstream)));
        final List<Upstream> validUpstreamList = partitionedUpstreams.get(true);
        final List<Upstream> excludedUpstreamList = actualUpstreamList.stream()
                .filter(admitted.negate()).collect(Collectors.toList());
        final List<Upstream> offlineUpstreamList = partitionedUpstreams.get(false).stream()
                .filter(admitted).collect(Collectors.toList());
        List<Upstream> existUpstreamList = MapUtils.computeIfAbsent(UPSTREAM_MAP, selectorId, k -> Lists.newArrayList());

        removeStaleUpstreams(selectorId, actualUpstreamList, existUpstreamList);
        updateUpstreamMetadata(selectorId, actualUpstreamList, existUpstreamList);
        processOfflineUpstreams(selectorId, offlineUpstreamList, existUpstreamList);
        processValidUpstreams(selectorId, validUpstreamList, existUpstreamList);
        processExcludedUpstreams(selectorId, excludedUpstreamList);

        List<Upstream> healthyUpstreamList = task.getHealthyUpstreamListBySelectorId(selectorId);
        UPSTREAM_MAP.put(selectorId, Objects.isNull(healthyUpstreamList) ? Lists.newArrayList() : healthyUpstreamList);
    }

    private void removeStaleUpstreams(final String selectorId, final List<Upstream> actualUpstreamList,
                                      final List<Upstream> existUpstreamList) {
        Set<UpstreamIdentity> actualIdentities = actualUpstreamList.stream()
                .map(Upstream::identity)
                .collect(Collectors.toSet());
        for (Upstream existUp : Lists.newArrayList(existUpstreamList)) {
            if (!actualIdentities.contains(existUp.identity())) {
                task.triggerRemoveOne(selectorId, existUp);
                existUpstreamList.remove(existUp);
            }
        }
        List<Upstream> healthyList = task.getHealthyUpstream().get(selectorId);
        if (Objects.nonNull(healthyList)) {
            for (Upstream healthyUp : Lists.newArrayList(healthyList)) {
                if (!actualIdentities.contains(healthyUp.identity())) {
                    task.removeFromMap(task.getHealthyUpstream(), selectorId, healthyUp);
                }
            }
        }
        List<Upstream> unhealthyList = task.getUnhealthyUpstream().get(selectorId);
        if (Objects.nonNull(unhealthyList)) {
            for (Upstream unhealthyUp : Lists.newArrayList(unhealthyList)) {
                if (!actualIdentities.contains(unhealthyUp.identity())) {
                    task.removeFromMap(task.getUnhealthyUpstream(), selectorId, unhealthyUp);
                }
            }
        }
    }

    private void updateUpstreamMetadata(final String selectorId, final List<Upstream> upstreams, final List<Upstream> existingHealthy) {
        Map<UpstreamIdentity, Upstream> existing = new HashMap<>(getCurrentUnhealthyMap(selectorId));
        existingHealthy.forEach(upstream -> existing.put(upstream.identity(), upstream));
        upstreams.forEach(upstream -> {
            Upstream cached = existing.get(upstream.identity());
            if (Objects.nonNull(cached)) {
                cached.setMetadata(Collections.unmodifiableMap(new HashMap<>(upstream.getMetadata())));
                cached.setLabels(upstream.getLabels());
                cached.setGray(upstream.isGray());
                cached.setDraining(upstream.isDraining());
                cached.setReady(upstream.isReady());
            }
        });
    }

    private void initializeUpstreamHealthStatus(final List<Upstream> upstreamList) {
        upstreamList.forEach(upstream -> {
            if (!upstream.isHealthCheckEnabled()) {
                upstream.setStatus(true);
                upstream.setHealthy(true);
            }
        });
    }

    private void processOfflineUpstreams(final String selectorId, final List<Upstream> offlineUpstreamList,
                                         final List<Upstream> existUpstreamList) {
        Map<UpstreamIdentity, Upstream> currentUnhealthyMap = getCurrentUnhealthyMap(selectorId);
        Set<UpstreamIdentity> existUpstreamSet = existUpstreamList.stream().map(Upstream::identity).collect(Collectors.toSet());

        offlineUpstreamList.forEach(offlineUp -> {
            if (existUpstreamSet.contains(offlineUp.identity())) {
                if (currentUnhealthyMap.containsKey(offlineUp.identity()) && offlineUp.isHealthCheckEnabled()) {
                    task.putToMap(task.getUnhealthyUpstream(), selectorId, offlineUp);
                    task.removeFromMap(task.getHealthyUpstream(), selectorId, offlineUp);
                } else {
                    task.triggerRemoveOne(selectorId, offlineUp);
                }
            } else if (offlineUp.isHealthCheckEnabled()) {
                task.putToMap(task.getUnhealthyUpstream(), selectorId, offlineUp);
            }
        });
    }

    private void processExcludedUpstreams(final String selectorId, final List<Upstream> excludedUpstreamList) {
        // A draining or not-ready upstream leaves both health pools, so no new selection
        // returns it and no probe can restore it; it stays in the configured set, and a
        // later sync without the marker re-admits it through the regular paths (#8042).
        excludedUpstreamList.forEach(excludedUp -> task.triggerRemoveOne(selectorId, excludedUp));
    }

    private void processValidUpstreams(final String selectorId, final List<Upstream> validUpstreamList,
                                       final List<Upstream> existUpstreamList) {
        if (validUpstreamList.isEmpty()) {
            return;
        }

        updateExistingUpstreams(validUpstreamList, existUpstreamList);
        addNewUpstreams(selectorId, validUpstreamList, existUpstreamList);
    }

    private void updateExistingUpstreams(final List<Upstream> validUpstreamList, final List<Upstream> existUpstreamList) {
        Map<UpstreamIdentity, Upstream> existUpstreamMap = existUpstreamList.stream()
            .collect(Collectors.toMap(Upstream::identity, existUp -> existUp, (existing, replacement) -> existing));

        validUpstreamList.forEach(validUp -> {
            Upstream matchedExistUp = existUpstreamMap.get(validUp.identity());
            if (Objects.nonNull(matchedExistUp)) {
                matchedExistUp.setWeight(validUp.getWeight());
                matchedExistUp.setHealthCheckEnabled(validUp.isHealthCheckEnabled());
                if (!matchedExistUp.isHealthCheckEnabled()) {
                    matchedExistUp.setHealthy(true);
                }
            }
        });
    }

    private void addNewUpstreams(final String selectorId, final List<Upstream> validUpstreamList,
                                 final List<Upstream> existUpstreamList) {
        Map<UpstreamIdentity, Upstream> currentUnhealthyMap = getCurrentUnhealthyMap(selectorId);

        validUpstreamList.stream()
            .filter(validUp -> !existUpstreamList.contains(validUp))
            .forEach(up -> {
                Upstream prevUnhealthy = currentUnhealthyMap.get(up.identity());
                if (Objects.nonNull(prevUnhealthy)) {
                    task.putToMap(task.getUnhealthyUpstream(), selectorId, up);
                } else {
                    task.triggerAddOne(selectorId, up);
                }
            });
    }

    private Map<UpstreamIdentity, Upstream> getCurrentUnhealthyMap(final String selectorId) {
        List<Upstream> currentUnhealthy = task.getUnhealthyUpstream().get(selectorId);
        return Objects.isNull(currentUnhealthy)
            ? Maps.newConcurrentMap()
            : currentUnhealthy.stream().collect(Collectors.toMap(Upstream::identity, u -> u, (a, b) -> a));
    }
}
