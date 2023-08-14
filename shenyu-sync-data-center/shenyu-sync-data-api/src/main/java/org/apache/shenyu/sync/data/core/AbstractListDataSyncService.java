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

package org.apache.shenyu.sync.data.core;

import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.NacosPathConstants;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.DiscoveryUpstreamDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.api.ProxySelectorDataSubscriber;

import java.util.List;
import java.util.Optional;
import java.util.function.Consumer;

/**
 * AbstractNodeDataSyncService.
 * Abstract method to monitor child node changes.
 */
public abstract class AbstractListDataSyncService {

    private final PluginDataSubscriber pluginDataSubscriber;

    private final List<MetaDataSubscriber> metaDataSubscribers;

    private final List<AuthDataSubscriber> authDataSubscribers;

    private final List<ProxySelectorDataSubscriber> proxySelectorDataSubscribers;

    private final List<DiscoveryUpstreamDataSubscriber> discoveryUpstreamDataSubscribers;

    public AbstractListDataSyncService(final PluginDataSubscriber pluginDataSubscriber,
                                       final List<MetaDataSubscriber> metaDataSubscribers,
                                       final List<AuthDataSubscriber> authDataSubscribers,
                                       final List<ProxySelectorDataSubscriber> proxySelectorDataSubscribers,
                                       final List<DiscoveryUpstreamDataSubscriber> discoveryUpstreamDataSubscribers) {
        this.pluginDataSubscriber = pluginDataSubscriber;
        this.metaDataSubscribers = metaDataSubscribers;
        this.authDataSubscribers = authDataSubscribers;
        this.proxySelectorDataSubscribers = proxySelectorDataSubscribers;
        this.discoveryUpstreamDataSubscribers = discoveryUpstreamDataSubscribers;
    }

    protected void startWatch() {
        try {
            final List<String> pluginNames = getConfigListOnWatch("plugin.list", updateData -> {
                List<String> changedPluginNames = GsonUtils.getInstance().fromList(updateData, String.class);
                watcherPlugin(changedPluginNames);
            });

            watcherPlugin(pluginNames);

            watchCommonList("auth.", this::cacheAuthData, this::unCacheAuthData, this::removeListener);

            watchCommonList("meta.", this::cacheMetaData, this::unCacheMetaData, this::removeListener);

        } catch (Exception e) {
            throw new ShenyuException(e);
        }
    }

    protected abstract List<String> getConfigListOnWatch(String key, Consumer<String> updateHandler);

    protected abstract String getServiceConfig(String key, Consumer<String> updateHandler, Consumer<String> deleteHandler);

    protected abstract String getConfigOnWatch(String key, Consumer<String> updateHandler, Consumer<String> deleteHandler);

    protected abstract void removeListener(String removeKey);

    private void watcherPlugin(final List<String> pluginNames) {
        if (ObjectUtils.isEmpty(pluginNames)) {
            return;
        }
        pluginNames.forEach(pluginName -> {
            final String pluginData = this.getConfigOnWatch("plugin." + pluginName, this::cachePluginData, this::unCachePluginData);
            cachePluginData(pluginData);

            final List<String> selectorIds = getConfigListOnWatch("selector." + pluginName + ".list", updateData -> {
                List<String> changedSelectorIds = GsonUtils.getInstance().fromList(updateData, String.class);
                watcherSelector(pluginName, changedSelectorIds);
            });

            watcherSelector(pluginName, selectorIds);

            watchCommonList(NacosPathConstants.PROXY_SELECTOR_DATA_ID + "." + pluginName + ".", this::cacheProxySelectorData, this::unCacheProxySelectorData, this::removeListener);
            watchCommonList(NacosPathConstants.DISCOVERY_DATA_ID + "." + pluginName + ".", this::cacheProxySelectorData, this::unCacheProxySelectorData, this::removeListener);
        });
    }

    /**
     * watchCommonList.
     * examples data:
     *  meta.list
     *   -> meta.id
     *   -> meta.id
     *   -> meta.id
     * @param keyPrefix keyPrefix
     * @param updateHandler updateHandler
     * @param deleteHandler deleteHandler
     * @param removeListener removeListener
     */
    private void watchCommonList(final String keyPrefix, final Consumer<String> updateHandler,
                                 final Consumer<String> deleteHandler, final Consumer<String> removeListener) {
        final List<String> keyIds = getConfigListOnWatch(keyPrefix + "list", updateData -> {
            List<String> changedIds = GsonUtils.getInstance().fromList(updateData, String.class);
            watcherCommonData(changedIds, keyPrefix, updateHandler, deleteHandler);
        });

        watcherCommonData(keyIds, keyPrefix, updateHandler, deleteHandler);
    }

    private void watcherCommonData(final List<String> keys, final String keyPrefix,
                                   final Consumer<String> updateHandler, final Consumer<String> deleteHandler) {
        if (ObjectUtils.isEmpty(keys)) {
            return;
        }
        keys.forEach(key -> {
            final String keyData = this.getConfigOnWatch(keyPrefix + key, updateHandler, deleteHandler);
            updateHandler.accept(keyData);
        });
    }

    private void watcherSelector(final String pluginName, final List<String> selectorIds) {
        if (ObjectUtils.isEmpty(selectorIds)) {
            return;
        }
        selectorIds.forEach(selectorId -> {
            final String selectorData = this.getConfigOnWatch("selector." + pluginName + "." + selectorId,
                    this::cacheSelectorData, this::unCacheSelectorData);

            cacheSelectorData(selectorData);

            final List<String> ruleIds = getConfigListOnWatch("rule." + pluginName + "." + selectorId + ".list", updateData -> {
                List<String> upSelectorIds = GsonUtils.getInstance().fromList(updateData, String.class);
                watcherRule(selectorId, upSelectorIds, pluginName);
            });

            watcherRule(selectorId, ruleIds, pluginName);
        });
    }

    private void watcherRule(final String selectorId, final List<String> ruleIds, final String pluginName) {
        if (ObjectUtils.isEmpty(ruleIds)) {
            return;
        }
        ruleIds.forEach(ruleId -> {
            final String ruleDataStr = this.getConfigOnWatch("rule." + pluginName + "." + selectorId + "." + ruleId,
                    this::cacheRuleData, this::unCacheRuleData);
            cacheRuleData(ruleDataStr);
        });
    }

    protected void cachePluginData(final String dataString) {
        final PluginData pluginData = GsonUtils.getInstance().fromJson(dataString, PluginData.class);
        Optional.ofNullable(pluginData)
                .flatMap(data -> Optional.ofNullable(pluginDataSubscriber)).ifPresent(e -> e.onSubscribe(pluginData));
    }

    protected void unCachePluginData(final String pluginName) {
        final PluginData data = new PluginData();
        data.setName(pluginName);
        Optional.ofNullable(pluginDataSubscriber).ifPresent(e -> e.unSubscribe(data));
    }

    protected void cacheSelectorData(final String dataString) {
        final SelectorData selectorData = GsonUtils.getInstance().fromJson(dataString, SelectorData.class);
        Optional.ofNullable(selectorData)
                .ifPresent(data -> Optional.ofNullable(pluginDataSubscriber).ifPresent(e -> e.onSelectorSubscribe(data)));
    }

    protected void unCacheSelectorData(final String removeKey) {
        final SelectorData selectorData = new SelectorData();
        final String[] ruleKeys = StringUtils.split(removeKey, ".");
        selectorData.setPluginName(ruleKeys[1]);
        selectorData.setId(ruleKeys[2]);
        Optional.ofNullable(pluginDataSubscriber).ifPresent(e -> e.unSelectorSubscribe(selectorData));
        removeListener(removeKey);
    }

    protected void cacheRuleData(final String dataString) {
        final RuleData ruleData = GsonUtils.getInstance().fromJson(dataString, RuleData.class);
        Optional.ofNullable(ruleData)
                .ifPresent(data -> Optional.ofNullable(pluginDataSubscriber).ifPresent(e -> e.onRuleSubscribe(data)));
    }

    protected void unCacheRuleData(final String removeKey) {
        final RuleData ruleData = new RuleData();
        final String[] ruleKeys = StringUtils.split(removeKey, ".");
        ruleData.setPluginName(ruleKeys[1]);
        ruleData.setSelectorId(ruleKeys[2]);
        ruleData.setId(ruleKeys[3]);
        Optional.ofNullable(pluginDataSubscriber).ifPresent(e -> e.unRuleSubscribe(ruleData));
        removeListener(removeKey);
    }

    protected void cacheAuthData(final String dataString) {
        final AppAuthData appAuthData = GsonUtils.getInstance().fromJson(dataString, AppAuthData.class);
        Optional.ofNullable(appAuthData)
                .ifPresent(data -> authDataSubscribers.forEach(e -> e.onSubscribe(data)));
    }

    protected void unCacheAuthData(final String removeKey) {
        final AppAuthData appAuthData = new AppAuthData();
        final String[] ruleKeys = StringUtils.split(removeKey, ".");
        appAuthData.setAppKey(ruleKeys[1]);
        authDataSubscribers.forEach(e -> e.unSubscribe(appAuthData));
        removeListener(removeKey);
    }

    protected void cacheMetaData(final String dataString) {
        final MetaData metaData = GsonUtils.getInstance().fromJson(dataString, MetaData.class);
        Optional.ofNullable(metaData)
                .ifPresent(data -> metaDataSubscribers.forEach(e -> e.onSubscribe(metaData)));
    }

    protected void unCacheMetaData(final String removeKey) {
        final MetaData metaData = new MetaData();
        final String[] ruleKeys = StringUtils.split(removeKey, ".");
        metaData.setId(ruleKeys[1]);
        metaDataSubscribers.forEach(e -> e.unSubscribe(metaData));
        removeListener(removeKey);
    }

    protected void cacheProxySelectorData(final String dataString) {
        final ProxySelectorData proxySelectorData = GsonUtils.getInstance().fromJson(dataString, ProxySelectorData.class);
        Optional.ofNullable(proxySelectorData)
                .ifPresent(data -> proxySelectorDataSubscribers.forEach(e -> e.onSubscribe(data)));
    }

    protected void unCacheProxySelectorData(final String removeKey) {
        ProxySelectorData proxySelectorData = new ProxySelectorData();
        final String[] proxySelectorKeys = StringUtils.split(removeKey, ".");
        proxySelectorData.setPluginName(proxySelectorKeys[2]);
        proxySelectorData.setName(proxySelectorKeys[3]);
        proxySelectorDataSubscribers.forEach(e -> e.unSubscribe(proxySelectorData));
        removeListener(removeKey);
    }

    protected void cacheDiscoveryUpstreamData(final String dataString) {
        final DiscoverySyncData discoverySyncData = GsonUtils.getInstance().fromJson(dataString, DiscoverySyncData.class);
        Optional.ofNullable(discoverySyncData)
                .ifPresent(data -> discoveryUpstreamDataSubscribers.forEach(e -> e.onSubscribe(data)));
    }

    protected void unCacheDiscoveryUpstreamData(final String removeKey) {
        DiscoverySyncData proxySelectorData = new DiscoverySyncData();
        final String[] proxySelectorKeys = StringUtils.split(removeKey, ".");
        proxySelectorData.setPluginName(proxySelectorKeys[2]);
        proxySelectorData.setSelectorId(proxySelectorKeys[3]);
        discoveryUpstreamDataSubscribers.forEach(e -> e.unSubscribe(proxySelectorData));
        removeListener(removeKey);
    }
}
