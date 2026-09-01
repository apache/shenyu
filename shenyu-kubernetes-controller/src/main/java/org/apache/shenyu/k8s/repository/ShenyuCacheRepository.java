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

package org.apache.shenyu.k8s.repository;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.selector.DivideUpstream;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.base.cache.CommonDiscoveryUpstreamDataSubscriber;
import org.apache.shenyu.plugin.base.cache.CommonPluginDataSubscriber;
import org.apache.shenyu.plugin.base.cache.MetaDataCache;
import org.apache.shenyu.plugin.global.subsciber.MetaDataCacheSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Properties;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * The repository to config shenyu.
 *
 * <p>Should try to avoid directly operating memory configuration through PluginDataSubscriber
 * in ingress-controller, but use ShenyuCacheRepository.
 * This will make it easier for us if we have architectural changes to the ingress-controller.
 * </p>
 */
public class ShenyuCacheRepository {

    private static final Logger LOG = LoggerFactory.getLogger(ShenyuCacheRepository.class);

    private final CommonPluginDataSubscriber subscriber;

    private final CommonDiscoveryUpstreamDataSubscriber discoveryUpstreamDataSubscriber;

    private final MetaDataSubscriber metaDataSubscriber;

    private final MetaDataCacheSubscriber metaDataCacheSubscriber;

    /**
     * Shenyu Cache Repository Constructor.
     *
     * @param subscriber PluginDataSubscriber
     */
    public ShenyuCacheRepository(final CommonPluginDataSubscriber subscriber, final CommonDiscoveryUpstreamDataSubscriber discoveryUpstreamDataSubscriber,
                                 final MetaDataSubscriber metaDataSubscriber, final MetaDataCacheSubscriber metaDataCacheSubscriber) {
        this.subscriber = subscriber;
        this.discoveryUpstreamDataSubscriber = discoveryUpstreamDataSubscriber;
        this.metaDataSubscriber = metaDataSubscriber;
        this.metaDataCacheSubscriber = metaDataCacheSubscriber;
    }

    /**
     * Find PluginData by plugin name.
     *
     * @param pluginName  plugin name
     * @return PluginData
     */
    public PluginData findPluginData(final String pluginName) {
        return BaseDataCache.getInstance().obtainPluginData(pluginName);
    }

    /**
     * Save or update PluginData by PluginData.
     *
     * @param pluginData PluginData
     */
    public void saveOrUpdatePluginData(final PluginData pluginData) {
        subscriber.onSubscribe(pluginData);
    }

    /**
     * Delete PluginData by plugin name.
     *
     * @param pluginName plugin name
     */
    public void deletePluginData(final String pluginName) {
        subscriber.unSubscribe(PluginData.builder().name(pluginName).build());
    }

    /**
     * Find SelectorData list by pluginName.
     *
     * @param pluginName plugin name
     * @return SelectorData list
     */
    public List<SelectorData> findSelectorDataList(final String pluginName) {
        return BaseDataCache.getInstance().obtainSelectorData(pluginName);
    }

    /**
     * Save or update SelectorData by SelectorData. Idempotent: a selector whose cached
     * content is already equal (including the upstream list in its handle) is skipped, so
     * the periodic informer resync does not churn the data plane with no-op updates.
     *
     * @param selectorData SelectorData
     */
    public void saveOrUpdateSelectorData(final SelectorData selectorData) {
        List<DiscoveryUpstreamData> upstreamDataList = new ArrayList<>(convert(selectorData.getPluginName(), selectorData.getHandle()));
        Set<String> newUrls = upstreamDataList.stream().map(DiscoveryUpstreamData::getUrl).collect(Collectors.toSet());
        List<Upstream> cachedUpstreams = UpstreamCacheManager.getInstance().findUpstreamListBySelectorId(selectorData.getId());
        Set<String> cachedUrls = CollectionUtils.isEmpty(cachedUpstreams) ? Collections.emptySet()
                : cachedUpstreams.stream().map(Upstream::getUrl).collect(Collectors.toSet());
        // {@link UpstreamCacheManager#submit} merges by URL and only removes entries
        // explicitly marked offline, because the admin control plane pushes incremental
        // upstream events. The Kubernetes reconciler submits an authoritative full snapshot,
        // so cache-only entries are appended as offline in the SAME submit: the snapshot is
        // replaced atomically, without the remove-then-readd window that would briefly leave
        // the selector with zero upstreams. Upstream identity is url+protocol, so both are
        // taken from the cached entry.
        if (CollectionUtils.isNotEmpty(cachedUpstreams)) {
            for (Upstream cached : cachedUpstreams) {
                if (!newUrls.contains(cached.getUrl())) {
                    upstreamDataList.add(buildOfflineUpstreamData(cached.getUrl(), cached.getProtocol()));
                }
            }
        }
        boolean upstreamsChanged = !cachedUrls.equals(newUrls);
        SelectorData cachedSelector = findSelectorData(selectorData.getPluginName(), selectorData.getId());
        boolean selectorChanged = !selectorData.equals(cachedSelector);
        if (!selectorChanged && !upstreamsChanged) {
            return;
        }
        DiscoverySyncData discoverySyncData = new DiscoverySyncData();
        discoverySyncData.setSelectorName(selectorData.getName());
        discoverySyncData.setSelectorId(selectorData.getId());
        discoverySyncData.setPluginName(selectorData.getPluginName());
        discoverySyncData.setUpstreamDataList(upstreamDataList);
        saveOrUpdateDiscoveryUpstreamData(discoverySyncData);
        if (upstreamsChanged) {
            LOG.info("Resolved {} upstream(s) for selector {}", newUrls.size(), selectorData.getId());
        }
        if (selectorChanged) {
            subscriber.onSelectorSubscribe(selectorData);
            LOG.info("Published divide selector {} for HTTPRoute coordinates {}", selectorData.getId(), selectorData.getName());
        }
    }

    /**
     * Find a cached SelectorData by plugin name and selector id.
     *
     * @param pluginName plugin name
     * @param selectorId selector id
     * @return the cached selector, or null
     */
    public SelectorData findSelectorData(final String pluginName, final String selectorId) {
        List<SelectorData> selectors = BaseDataCache.getInstance().obtainSelectorData(pluginName);
        if (CollectionUtils.isEmpty(selectors)) {
            return null;
        }
        return selectors.stream()
                .filter(selector -> selector.getId().equals(selectorId))
                .findFirst().orElse(null);
    }

    private DiscoveryUpstreamData buildOfflineUpstreamData(final String url, final String protocol) {
        DiscoveryUpstreamData upstreamData = new DiscoveryUpstreamData();
        upstreamData.setUrl(url);
        upstreamData.setProtocol(protocol);
        // status 1 marks the entry offline, which is how submit evicts it from the cache
        upstreamData.setStatus(1);
        upstreamData.setDateUpdated(new Timestamp(System.currentTimeMillis()));
        return upstreamData;
    }

    private List<DiscoveryUpstreamData> convert(final String pluginName, final String handle) {
        List<DivideUpstream> divideUpstreams = GsonUtils.getInstance().fromList(handle, DivideUpstream.class);
        if (CollectionUtils.isEmpty(divideUpstreams)) {
            return Collections.emptyList();
        }
        return divideUpstreams.stream().map(up -> {
            DiscoveryUpstreamData upstreamData = new DiscoveryUpstreamData();
            upstreamData.setUrl(up.getUpstreamUrl());
            upstreamData.setProtocol(up.getProtocol());
            upstreamData.setWeight(up.getWeight());
            upstreamData.setStatus(up.isStatus() ? 0 : 1);
            Properties properties = new Properties();
            properties.setProperty("warmup", String.valueOf(up.getWarmup()));
            properties.setProperty("upstreamHost", String.valueOf(up.getUpstreamHost()));
            upstreamData.setDateUpdated(Optional.of(up.getTimestamp()).map(Timestamp::new).orElse(new Timestamp(System.currentTimeMillis())));
            upstreamData.setProps(GsonUtils.getInstance().toJson(properties));
            upstreamData.setDateCreated(Optional.of(up.getTimestamp()).map(Timestamp::new).orElse(new Timestamp(System.currentTimeMillis())));
            return upstreamData;
        }).collect(Collectors.toList());
    }

    /**
     * Save or update DiscoveryUpstreamData.
     *
     * @param data data
     */
    public void saveOrUpdateDiscoveryUpstreamData(final DiscoverySyncData data) {
        discoveryUpstreamDataSubscriber.onSubscribe(data);
    }

    /**
     * Delete SelectorData by plugin name and selector id.
     *
     * @param pluginName plugin name
     * @param selectorId selector id
     */
    public void deleteSelectorData(final String pluginName, final String selectorId) {
        subscriber.unSelectorSubscribe(SelectorData.builder().pluginName(pluginName).id(selectorId).build());
    }

    /**
     * Delete a selector together with all rules still attached to it. findRuleDataList
     * exposes the cache's mutable internal list, so iterate over a copy to avoid
     * ConcurrentModificationException.
     *
     * @param pluginName plugin name
     * @param selectorId selector id
     */
    public void deleteSelectorWithRules(final String pluginName, final String selectorId) {
        List<RuleData> rules = findRuleDataList(selectorId);
        if (CollectionUtils.isNotEmpty(rules)) {
            for (RuleData rule : new ArrayList<>(rules)) {
                deleteRuleData(pluginName, selectorId, rule.getId());
            }
        }
        deleteSelectorData(pluginName, selectorId);
    }

    /**
     * Find RuleData list by selector id.
     *
     * @param selectorId selector id
     * @return RuleData list, never null; empty when the selector has no cached rules yet
     */
    public List<RuleData> findRuleDataList(final String selectorId) {
        return Optional.ofNullable(BaseDataCache.getInstance().obtainRuleData(selectorId)).orElse(Collections.emptyList());
    }

    /**
     * Save or update RuleData by RuleData. Idempotent like the selector path: an unchanged
     * rule is skipped so the periodic resync does not churn the data plane.
     *
     * @param ruleData RuleData
     */
    public void saveOrUpdateRuleData(final RuleData ruleData) {
        boolean unchanged = findRuleDataList(ruleData.getSelectorId()).stream()
                .anyMatch(cached -> cached.getId().equals(ruleData.getId()) && cached.equals(ruleData));
        if (unchanged) {
            return;
        }
        subscriber.onRuleSubscribe(ruleData);
    }

    /**
     * Delete RuleData by plugin name, selector id and rule id.
     *
     * @param pluginName plugin name
     * @param selectorId selector id
     * @param ruleId rule id
     */
    public void deleteRuleData(final String pluginName, final String selectorId, final String ruleId) {
        subscriber.unRuleSubscribe(RuleData.builder().pluginName(pluginName).selectorId(selectorId).id(ruleId).build());
    }

    /**
     * Find MetaData by path.
     * @param path path
     * @return MetaData
     */
    public MetaData findMetaData(final String path) {
        return MetaDataCache.getInstance().obtain(path);
    }

    /**
     * Save or update MetaData by MetaData.
     * @param metaData MetaData
     */
    public void saveOrUpdateMetaData(final MetaData metaData) {
        metaDataSubscriber.onSubscribe(metaData);
        metaDataCacheSubscriber.onSubscribe(metaData);
    }

    /**
     * Delete MetaData by MetaData.
     * @param metaData MetaData
     */
    public void deleteMetaData(final MetaData metaData) {
        metaDataSubscriber.unSubscribe(metaData);
        metaDataCacheSubscriber.unSubscribe(metaData);
    }
}
