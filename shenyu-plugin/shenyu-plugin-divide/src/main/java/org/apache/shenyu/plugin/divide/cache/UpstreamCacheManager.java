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
import org.apache.shenyu.plugin.base.cache.RuleHandleCache;

import java.util.List;
import java.util.Map;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * this is divide  http url upstream.
 */
@Slf4j
public final class UpstreamCacheManager extends RuleHandleCache<String, DivideRuleHandle> {

    private static final UpstreamCacheManager INSTANCE = new UpstreamCacheManager();

    private static final Map<String, List<DivideUpstream>> UPSTREAM_MAP = Maps.newConcurrentMap();

    private static final Map<String, List<DivideUpstream>> UPSTREAM_MAP_TEMP = Maps.newConcurrentMap();

    /**
     * suggest shenyu.upstream.scheduledTime set 1 SECONDS.
     */
    private UpstreamCacheManager() {
        boolean check = Boolean.parseBoolean(System.getProperty("shenyu.upstream.check", "false"));
        if (check) {
            new ScheduledThreadPoolExecutor(1, ShenyuThreadFactory.create("scheduled-upstream-task", false))
                    .scheduleWithFixedDelay(this::scheduled,
                            30, Integer.parseInt(System.getProperty("shenyu.upstream.scheduledTime", "30")), TimeUnit.SECONDS);
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
        return UPSTREAM_MAP_TEMP.get(selectorId);
    }

    /**
     * Remove by key.
     *
     * @param key the key
     */
    public void removeByKey(final String key) {
        UPSTREAM_MAP_TEMP.remove(key);
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
            UPSTREAM_MAP_TEMP.put(selectorData.getId(), upstreamList);
        } else {
            UPSTREAM_MAP.remove(selectorData.getId());
            UPSTREAM_MAP_TEMP.remove(selectorData.getId());
        }
    }

    private void scheduled() {
        if (UPSTREAM_MAP.size() > 0) {
            UPSTREAM_MAP.forEach((k, v) -> {
                List<DivideUpstream> result = check(v);
                if (result.size() > 0) {
                    UPSTREAM_MAP_TEMP.put(k, result);
                } else {
                    UPSTREAM_MAP_TEMP.remove(k);
                }
            });
        }
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
