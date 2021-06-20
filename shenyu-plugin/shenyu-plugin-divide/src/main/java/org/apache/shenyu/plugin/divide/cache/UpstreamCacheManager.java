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
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.UpstreamCheckUtils;
import org.apache.shenyu.plugin.base.cache.BaseHandleCache;

import java.util.List;
import java.util.Map;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * this is divide  http url upstream.
 */
@Slf4j
public final class UpstreamCacheManager extends BaseHandleCache<String, DivideRuleHandle> {

    private static final UpstreamCacheManager INSTANCE = new UpstreamCacheManager();

    private static final Map<String, List<DivideUpstream>> UPSTREAM_MAP = Maps.newConcurrentMap();

    private static final Map<String, List<DivideUpstream>> HEALTHY_UPSTREAM = Maps.newConcurrentMap();

    private static final Map<String, List<DivideUpstream>> UNHEALTHY_UPSTREAM = Maps.newConcurrentMap();

    private final AtomicBoolean checkStarted = new AtomicBoolean(false);

    private Boolean checkEnable;
    private Integer checkTimeout;
    private Integer checkInterval;
    private Integer healthyThreshold;
    private Integer unhealthyThreshold;
    
    /**
     * suggest shenyu.upstream.scheduledTime set 1 SECONDS.
     */
    private UpstreamCacheManager() {
        initHealthCheck();
    }

    private void initHealthCheck() {
        checkEnable = Boolean.parseBoolean(System.getProperty("shenyu.upstream.check.enable", "false"));
        checkTimeout = Integer.parseInt(System.getProperty("shenyu.upstream.check.timeout", "3000"));
        checkInterval = Integer.parseInt(System.getProperty("shenyu.upstream.check.interval", "5000"));
        healthyThreshold = Integer.parseInt(System.getProperty("shenyu.upstream.check.healthy-threshold", "1"));
        unhealthyThreshold = Integer.parseInt(System.getProperty("shenyu.upstream.check.unhealthy-threshold", "1"));

        scheduleHealthCheck();
    }

    private void scheduleHealthCheck() {
        if (checkEnable) {
            ThreadFactory threadFactory = ShenyuThreadFactory.create("upstream-health-check", false);
            new ScheduledThreadPoolExecutor(1, threadFactory)
                    .scheduleWithFixedDelay(this::healthCheck, 3000, checkInterval, TimeUnit.MILLISECONDS);
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
        return HEALTHY_UPSTREAM.get(selectorId);
    }

    /**
     * Remove by key.
     *
     * @param key the key
     */
    public void removeByKey(final String key) {
        HEALTHY_UPSTREAM.remove(key);
    }

    /**
     * Submit.
     *
     * @param selectorData the selector data
     */
    public void submit(final SelectorData selectorData) {
        final List<DivideUpstream> upstreamList = GsonUtils.getInstance().fromList(selectorData.getHandle(), DivideUpstream.class);
        if (null != upstreamList && upstreamList.size() > 0) {
            UPSTREAM_MAP.put(selectorData.getId(), upstreamList);
            HEALTHY_UPSTREAM.put(selectorData.getId(), upstreamList);
        } else {
            UPSTREAM_MAP.remove(selectorData.getId());
            HEALTHY_UPSTREAM.remove(selectorData.getId());
        }
    }

    private void healthCheck() {
        if (tryStartHealthCheck()) {
            doHealthCheck();
            finishHealthCheck();
        }
    }

    private void doHealthCheck() {
        if (UPSTREAM_MAP.size() > 0) {
            UPSTREAM_MAP.forEach((k, v) -> {
                List<DivideUpstream> result = check(v);
                if (result.size() > 0) {
                    HEALTHY_UPSTREAM.put(k, result);
                } else {
                    HEALTHY_UPSTREAM.remove(k);
                }
            });
        }
    }

    private boolean tryStartHealthCheck() {
        return checkStarted.compareAndSet(false, true);
    }

    private void finishHealthCheck() {
        checkStarted.set(false);
    }

    private List<DivideUpstream> check(final List<DivideUpstream> upstreamList) {
        List<DivideUpstream> resultList = Lists.newArrayListWithCapacity(upstreamList.size());
        for (DivideUpstream divideUpstream : upstreamList) {
            final boolean pass = UpstreamCheckUtils.checkUrl(divideUpstream.getUpstreamUrl());
            if (pass) {
                if (!divideUpstream.isStatus()) {
                    divideUpstream.setTimestamp(System.currentTimeMillis());
                    divideUpstream.setStatus(true);
                    log.info("UpstreamCacheManager detect success the url: {}, host: {} ", divideUpstream.getUpstreamUrl(), divideUpstream.getUpstreamHost());
                }
                resultList.add(divideUpstream);
            } else {
                divideUpstream.setStatus(false);
                log.error("check the url={} is fail ", divideUpstream.getUpstreamUrl());
            }
        }
        return resultList;
    }
}
