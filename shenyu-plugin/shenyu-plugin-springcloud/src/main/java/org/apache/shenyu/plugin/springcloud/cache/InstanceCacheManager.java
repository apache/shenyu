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

package org.apache.shenyu.plugin.springcloud.cache;

import com.google.common.collect.Maps;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.DivideUpstream;
import org.apache.shenyu.common.utils.CollectionUtils;
import org.apache.shenyu.common.utils.GsonUtils;

import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * this is spring cloud url upstream.
 */
public final class InstanceCacheManager {

    private static final InstanceCacheManager INSTANCE = new InstanceCacheManager();

    private static final Map<String, List<DivideUpstream>> UPSTREAM_MAP = Maps.newConcurrentMap();

    private InstanceCacheManager() {
    }

    /**
     * Gets instance.
     *
     * @return the instance
     */
    public static InstanceCacheManager getInstance() {
        return INSTANCE;
    }

    /**
     * Find upstream list by selector id list.
     *
     * @param selectorId the selector id
     * @return the list
     */
    public List<DivideUpstream> findUpstreamListBySelectorId(final String selectorId) {
        return UPSTREAM_MAP.get(selectorId);
    }

    /**
     * Remove by key.
     *
     * @param key the key
     */
    public void removeByKey(final String key) {
        UPSTREAM_MAP.remove(key);
    }

    /**
     * Submit.
     *
     * @param selectorData the selector data
     */
    public void submit(final SelectorData selectorData) {
        final List<DivideUpstream> upstreamList = GsonUtils.getInstance().fromList(selectorData.getHandle(), DivideUpstream.class);
        if (CollectionUtils.isNotEmpty(upstreamList)) {
            // replace upstream
            UPSTREAM_MAP.put(selectorData.getId(), upstreamList);
        } else {
            UPSTREAM_MAP.remove(selectorData.getId());
        }
    }

    /**
     * get serviceId.
     *
     * @param key key
     * @return String string
     */
    public Optional<DivideUpstream> getServiceId(final String key) {
        return Optional.ofNullable(findUpstreamListBySelectorId(key)).map(divideUpstreams ->
                divideUpstreams.stream().findFirst()).orElse(Optional.empty());
    }
}
