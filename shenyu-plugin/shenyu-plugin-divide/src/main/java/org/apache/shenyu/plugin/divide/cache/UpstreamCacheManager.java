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

package org.apache.shenyu.plugin.divide.cache;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import lombok.extern.slf4j.Slf4j;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.DivideUpstream;
import org.apache.shenyu.common.dto.convert.rule.impl.DivideRuleHandle;
import org.apache.shenyu.common.healthcheck.HealthCheckTask;
import org.apache.shenyu.common.utils.CollectionUtils;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.base.cache.RuleHandleCache;

import java.util.List;
import java.util.Map;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;

/**
 * this is divide  http url upstream.
 */
@Slf4j
public final class UpstreamCacheManager extends RuleHandleCache<String, DivideRuleHandle> {

    private static final UpstreamCacheManager INSTANCE = new UpstreamCacheManager();

    private static final Map<String, List<DivideUpstream>> UPSTREAM_MAP = Maps.newConcurrentMap();

    private HealthCheckTask task;

    // health check parameters
    private Boolean checkEnable;

    private int checkTimeout;

    private int checkInterval;

    private int healthyThreshold;

    private int unhealthyThreshold;

    // healthy upstream print parameters
    private Boolean printEnable;

    private Integer printInterval;

    private UpstreamCacheManager() {
        initHealthCheck();
    }

    private void initHealthCheck() {
        checkEnable = Boolean.parseBoolean(System.getProperty("shenyu.upstream.check.enable", "false"));
        checkTimeout = Integer.parseInt(System.getProperty("shenyu.upstream.check.timeout", "3000"));
        healthyThreshold = Integer.parseInt(System.getProperty("shenyu.upstream.check.healthy-threshold", "1"));
        unhealthyThreshold = Integer.parseInt(System.getProperty("shenyu.upstream.check.unhealthy-threshold", "1"));
        checkInterval = Integer.parseInt(System.getProperty("shenyu.upstream.check.interval", "5000"));

        createTask();
        printEnable = Boolean.parseBoolean(System.getProperty("shenyu.upstream.check.print.enable", "true"));
        printInterval = Integer.parseInt(System.getProperty("shenyu.upstream.check.print.interval", "60000"));
        scheduleHealthCheck();
    }

    private void createTask() {
        task = new HealthCheckTask(checkInterval);
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
                        .scheduleWithFixedDelay(task::printHealthyUpstream, printInterval, printInterval, TimeUnit.MILLISECONDS);
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
    public List<DivideUpstream> findUpstreamListBySelectorId(final String selectorId) {
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
     * Submit.
     *
     * @param selectorData the selector data
     */
    public void submit(final SelectorData selectorData) {
        final List<DivideUpstream> upstreamList = GsonUtils.getInstance().fromList(selectorData.getHandle(), DivideUpstream.class);
        if (CollectionUtils.isNotEmpty(upstreamList)) {
            List<DivideUpstream> existUpstream = UPSTREAM_MAP.computeIfAbsent(selectorData.getId(), k -> Lists.newArrayList());

            // check upstream delete
            for (DivideUpstream upstream : existUpstream) {
                if (!upstreamList.contains(upstream)) {
                    task.triggerRemoveOne(selectorData, upstream);
                }
            }

            // check upstream add
            for (DivideUpstream upstream : upstreamList) {
                if (!existUpstream.contains(upstream)) {
                    task.triggerAddOne(selectorData, upstream);
                }
            }

            // replace upstream
            UPSTREAM_MAP.put(selectorData.getId(), upstreamList);
        } else {
            UPSTREAM_MAP.remove(selectorData.getId());
            task.triggerRemoveAll(selectorData);
        }
    }
}
