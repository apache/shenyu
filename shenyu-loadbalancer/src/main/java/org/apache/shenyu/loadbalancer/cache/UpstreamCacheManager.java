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

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * this is upstream .
 */
public final class UpstreamCacheManager {

    private static final UpstreamCacheManager INSTANCE = new UpstreamCacheManager();

    private static final Map<String, List<Upstream>> UPSTREAM_MAP = Maps.newConcurrentMap();

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
     * Find upstream list by selector id list.
     *
     * @param selectorId the selector id
     * @return the list
     */
    public List<Upstream> findUpstreamListBySelectorId(final String selectorId) {
        return task.getHealthyUpstream().get(selectorId);
    }

    /**
     * Remove by key.
     *
     * @param key the key
     */
    public void removeByKey(final String key) {
        UPSTREAM_MAP.remove(key);
        task.triggerRemoveAll(key);
    }

    /**
     * Submit .
     *
     * @param selectorId   the selector id
     * @param upstreamList the upstream list
     */
    public void submit(final String selectorId, final List<Upstream> upstreamList) {
        List<Upstream> actualUpstreamList = Objects.isNull(upstreamList) ? Lists.newArrayList() : upstreamList;

        // Check if the list is empty first to avoid unnecessary processing
        if (actualUpstreamList.isEmpty()) {
            List<Upstream> existUpstreamList = MapUtils.computeIfAbsent(UPSTREAM_MAP, selectorId, k -> Lists.newArrayList());
            removeAllUpstreams(selectorId, existUpstreamList);
            return;
        }

        initializeUpstreamHealthStatus(actualUpstreamList);

        Map<Boolean, List<Upstream>> partitionedUpstreams = actualUpstreamList.stream()
                .collect(Collectors.partitioningBy(Upstream::isStatus));
        List<Upstream> validUpstreamList = partitionedUpstreams.get(true);
        List<Upstream> offlineUpstreamList = partitionedUpstreams.get(false);
        List<Upstream> existUpstreamList = MapUtils.computeIfAbsent(UPSTREAM_MAP, selectorId, k -> Lists.newArrayList());

        processOfflineUpstreams(selectorId, offlineUpstreamList, existUpstreamList);
        processValidUpstreams(selectorId, validUpstreamList, existUpstreamList);

        List<Upstream> healthyUpstreamList = task.getHealthyUpstreamListBySelectorId(selectorId);
        UPSTREAM_MAP.put(selectorId, Objects.isNull(healthyUpstreamList) ? Lists.newArrayList() : healthyUpstreamList);
    }

    private void initializeUpstreamHealthStatus(final List<Upstream> upstreamList) {
        upstreamList.forEach(upstream -> {
            if (!upstream.isHealthCheckEnabled()) {
                upstream.setStatus(true);
                upstream.setHealthy(true);
            }
        });
    }

    private void removeAllUpstreams(final String selectorId, final List<Upstream> existUpstreamList) {
        List<Upstream> toRemove = new ArrayList<>(existUpstreamList);
        toRemove.forEach(up -> task.triggerRemoveOne(selectorId, up));
    }

    private void processOfflineUpstreams(final String selectorId, final List<Upstream> offlineUpstreamList,
                                         final List<Upstream> existUpstreamList) {
        Map<String, Upstream> currentUnhealthyMap = getCurrentUnhealthyMap(selectorId);
        Set<Upstream> existUpstreamSet = new HashSet<>(existUpstreamList);

        offlineUpstreamList.forEach(offlineUp -> {
            String key = upstreamMapKey(offlineUp);
            if (existUpstreamSet.contains(offlineUp)) {
                if (currentUnhealthyMap.containsKey(key) && offlineUp.isHealthCheckEnabled()) {
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

    private void processValidUpstreams(final String selectorId, final List<Upstream> validUpstreamList,
                                       final List<Upstream> existUpstreamList) {
        if (validUpstreamList.isEmpty()) {
            return;
        }

        updateExistingUpstreams(validUpstreamList, existUpstreamList);
        addNewUpstreams(selectorId, validUpstreamList, existUpstreamList);
    }

    private void updateExistingUpstreams(final List<Upstream> validUpstreamList, final List<Upstream> existUpstreamList) {
        Map<String, Upstream> existUpstreamMap = existUpstreamList.stream()
            .collect(Collectors.toMap(this::upstreamMapKey, existUp -> existUp, (existing, replacement) -> existing));

        validUpstreamList.forEach(validUp -> {
            Upstream matchedExistUp = existUpstreamMap.get(upstreamMapKey(validUp));
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
        Map<String, Upstream> currentUnhealthyMap = getCurrentUnhealthyMap(selectorId);

        validUpstreamList.stream()
            .filter(validUp -> !existUpstreamList.contains(validUp))
            .forEach(up -> {
                Upstream prevUnhealthy = currentUnhealthyMap.get(upstreamMapKey(up));
                if (Objects.nonNull(prevUnhealthy)) {
                    task.putToMap(task.getUnhealthyUpstream(), selectorId, up);
                } else {
                    task.triggerAddOne(selectorId, up);
                }
            });
    }

    private Map<String, Upstream> getCurrentUnhealthyMap(final String selectorId) {
        List<Upstream> currentUnhealthy = task.getUnhealthyUpstream().get(selectorId);
        return Objects.isNull(currentUnhealthy)
            ? Maps.newConcurrentMap()
            : currentUnhealthy.stream().collect(Collectors.toMap(this::upstreamMapKey, u -> u, (a, b) -> a));
    }

    private String upstreamMapKey(final Upstream upstream) {
        return String.join("_", upstream.getProtocol(), upstream.getUrl());
    }
}
