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
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.DivideUpstream;
import org.apache.shenyu.common.dto.convert.rule.impl.DivideRuleHandle;
import org.apache.shenyu.common.utils.CollectionUtils;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.healthcheck.HealthCheckManager;
import org.apache.shenyu.plugin.base.cache.RuleHandleCache;

import java.util.List;
import java.util.Map;

/**
 * this is divide  http url upstream.
 */
@Slf4j
public final class UpstreamCacheManager extends RuleHandleCache<String, DivideRuleHandle> {

    private static final UpstreamCacheManager INSTANCE = new UpstreamCacheManager();

    private static final Map<String, List<DivideUpstream>> UPSTREAM_MAP = Maps.newConcurrentMap();

    private UpstreamCacheManager() {

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
        return HealthCheckManager.getInstance().getHealthyUpstream().get(selectorId);
    }

    /**
     * Remove by key.
     *
     * @param key the key
     */
    public void removeByKey(final String key) {
        UPSTREAM_MAP.remove(key);
        HealthCheckManager.getInstance().triggerRemoveAll(key);
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
                    HealthCheckManager.getInstance().triggerRemoveOne(selectorData, upstream);
                }
            }

            // check upstream add
            for (DivideUpstream upstream : upstreamList) {
                if (!existUpstream.contains(upstream)) {
                    HealthCheckManager.getInstance().triggerAddOne(selectorData, upstream);
                }
            }

            // replace upstream
            UPSTREAM_MAP.put(selectorData.getId(), upstreamList);
        } else {
            UPSTREAM_MAP.remove(selectorData.getId());
            HealthCheckManager.getInstance().triggerRemoveAll(selectorData);
        }
    }
}
