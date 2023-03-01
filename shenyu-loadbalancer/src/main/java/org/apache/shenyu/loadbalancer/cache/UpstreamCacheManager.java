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
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.config.ShenyuConfig.UpstreamCheck;
import org.apache.shenyu.common.utils.MapUtils;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.loadbalancer.entity.Upstream;

import java.util.List;
import java.util.Map;
import java.util.Optional;
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
     * @param selectorId the selector id
     * @param upstreamList the upstream list
     */
    public void submit(final String selectorId, final List<Upstream> upstreamList) {
        List<Upstream> validUpstreamList = upstreamList.stream().filter(Upstream::isStatus).collect(Collectors.toList());
        if (CollectionUtils.isNotEmpty(validUpstreamList)) {
            List<Upstream> existUpstream = MapUtils.computeIfAbsent(UPSTREAM_MAP, selectorId, k -> Lists.newArrayList());
            existUpstream.stream().filter(upstream -> !validUpstreamList.contains(upstream))
                    .forEach(upstream -> task.triggerRemoveOne(selectorId, upstream));
            validUpstreamList.stream().filter(upstream -> !existUpstream.contains(upstream))
                    .forEach(upstream -> task.triggerAddOne(selectorId, upstream));
            UPSTREAM_MAP.put(selectorId, validUpstreamList);
        } else {
            UPSTREAM_MAP.remove(selectorId);
            task.triggerRemoveAll(selectorId);
        }
    }
}
