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

package org.apache.shenyu.protocol.tcp;

import org.apache.shenyu.common.dto.DiscoveryUpstreamData;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * UpstreamProvider.
 */
public final class UpstreamProvider {

    private static final UpstreamProvider SINGLETON = new UpstreamProvider();

    private final Map<String, List<DiscoveryUpstreamData>> cache = new ConcurrentHashMap<>();

    private final Map<String, String> selectorNames = new ConcurrentHashMap<>();

    private UpstreamProvider() {
    }

    /**
     * getSingleton.
     *
     * @return UpstreamProvider
     */
    public static UpstreamProvider getSingleton() {
        return SINGLETON;
    }

    /**
     * provide.
     *
     * @param pluginSelectorName pluginSelectorName
     * @return UpstreamList
     */
    public List<DiscoveryUpstreamData> provide(final String pluginSelectorName) {
        return cache.getOrDefault(pluginSelectorName, new ArrayList<>());
    }

    /**
     * Whether the selector has an upstream cache entry.
     *
     * @param pluginSelectorName pluginSelectorName
     * @return true if the selector has an upstream cache entry
     */
    public boolean inCache(final String pluginSelectorName) {
        return cache.containsKey(pluginSelectorName);
    }

    /**
     * Register selector name by selector id.
     *
     * @param selectorId selectorId
     * @param selectorName selectorName
     */
    public void registerSelector(final String selectorId, final String selectorName) {
        if (Objects.nonNull(selectorId) && Objects.nonNull(selectorName)) {
            selectorNames.put(selectorId, selectorName);
        }
    }

    /**
     * Get selector name by selector id.
     *
     * @param selectorId selectorId
     * @return selectorName
     */
    public String getSelectorName(final String selectorId) {
        return Objects.isNull(selectorId) ? null : selectorNames.get(selectorId);
    }

    /**
     * createUpstreams.
     *
     * @param pluginSelectorName pluginSelectorName
     * @param upstreams          upstreams
     */
    public void createUpstreams(final String pluginSelectorName, final List<DiscoveryUpstreamData> upstreams) {
        List<DiscoveryUpstreamData> discoveryUpstreamDataList = Optional.ofNullable(upstreams).orElseGet(ArrayList::new);
        cache.put(pluginSelectorName, discoveryUpstreamDataList);
    }

    /**
     * refreshCache.
     *
     * @param pluginSelectorName pluginSelectorName
     * @param upstreams          upstreams
     * @return removeList
     */
    public List<DiscoveryUpstreamData> refreshCache(final String pluginSelectorName, final List<DiscoveryUpstreamData> upstreams) {
        List<DiscoveryUpstreamData> remove = cache.remove(pluginSelectorName);
        if (Objects.isNull(remove)) {
            return Collections.emptyList();
        }
        List<DiscoveryUpstreamData> discoveryUpstreamDataList = Optional.ofNullable(upstreams).orElse(new ArrayList<>());
        cache.put(pluginSelectorName, discoveryUpstreamDataList);
        Set<String> urlSet = discoveryUpstreamDataList.stream().map(DiscoveryUpstreamData::getUrl).collect(Collectors.toSet());
        return remove.stream().filter(r -> !urlSet.contains(r.getUrl())).collect(Collectors.toList());
    }

    /**
     * Remove upstreams.
     *
     * @param pluginSelectorName pluginSelectorName
     * @return removed upstreams
     */
    public List<DiscoveryUpstreamData> removeUpstreams(final String pluginSelectorName) {
        if (Objects.isNull(pluginSelectorName)) {
            return Collections.emptyList();
        }
        selectorNames.entrySet().removeIf(entry -> pluginSelectorName.equals(entry.getValue()));
        return Optional.ofNullable(cache.remove(pluginSelectorName)).orElseGet(Collections::emptyList);
    }

    /**
     * Clear upstreams and selector names.
     */
    public void clear() {
        cache.clear();
        selectorNames.clear();
    }
}
