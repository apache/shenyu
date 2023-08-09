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

package org.apache.shenyu.sync.data.nacos;

import com.alibaba.nacos.api.config.ConfigService;
import com.alibaba.nacos.api.config.listener.Listener;
import com.alibaba.nacos.api.exception.NacosException;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.NacosPathConstants;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.api.ProxySelectorDataSubscriber;
import org.apache.shenyu.sync.data.api.SyncDataService;
import org.apache.shenyu.sync.data.nacos.handler.NacosCacheHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executor;
import java.util.function.Consumer;

/**
 * The type Nacos sync data service.
 */
public class NacosSyncDataService extends NacosCacheHandler implements SyncDataService {

    private static final Logger LOG = LoggerFactory.getLogger(NacosSyncDataService.class);

    private final PluginDataSubscriber pluginDataSubscriber;

    private final List<MetaDataSubscriber> metaDataSubscribers;

    private final List<AuthDataSubscriber> authDataSubscribers;

    private final List<ProxySelectorDataSubscriber> proxySelectorDataSubscribers;

    private final ConfigService configService;

    private final ConcurrentHashMap<String, Listener> watchCache = new ConcurrentHashMap<>();

    /**
     * Instantiates a new Nacos sync data service.
     *
     * @param configService         the config service
     * @param pluginDataSubscriber the plugin data subscriber
     * @param metaDataSubscribers   the meta data subscribers
     * @param authDataSubscribers   the auth data subscribers
     */
    public NacosSyncDataService(final ConfigService configService, final PluginDataSubscriber pluginDataSubscriber,
                                final List<MetaDataSubscriber> metaDataSubscribers,
                                final List<AuthDataSubscriber> authDataSubscribers,
                                final List<ProxySelectorDataSubscriber> proxySelectorDataSubscribers) {

        super(configService, pluginDataSubscriber, metaDataSubscribers, authDataSubscribers, proxySelectorDataSubscribers);
        this.pluginDataSubscriber = pluginDataSubscriber;
        this.metaDataSubscribers = metaDataSubscribers;
        this.authDataSubscribers = authDataSubscribers;
        this.proxySelectorDataSubscribers = proxySelectorDataSubscribers;
        this.configService = configService;
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

            final List<String> ruleIds = getConfigListOnWatch("rule." + selectorId + ".list", updateData -> {
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
            final String ruleDataStr = this.getConfigOnWatch("rule." + selectorId + "." + ruleId,
                    this::cacheRuleData, removeKey -> unCacheRuleData(removeKey, pluginName));
            cacheRuleData(ruleDataStr);
        });
    }

    private void removeListener(final String key) {
        final Listener listener = watchCache.get(key);
        if (!ObjectUtils.isEmpty(listener)) {
            configService.removeListener(key, NacosPathConstants.GROUP, listener);
            watchCache.remove(key);
            LOG.info("nacos sync remove listener key:{}", key);
        }
    }

    private List<String> getConfigListOnWatch(final String key, final Consumer<String> updateHandler) {
        try {
            if (watchCache.containsKey(key)) {
                return GsonUtils.getInstance().fromList(configService.getConfig(key, NacosPathConstants.GROUP, 3000), String.class);
            }
            final String serviceConfig = getServiceConfig(key, updateHandler, null);
            return GsonUtils.getInstance().fromList(serviceConfig, String.class);
        } catch (Exception e) {
            throw new ShenyuException(e);
        }
    }

    private String getServiceConfig(final String key, final Consumer<String> updateHandler, final Consumer<String> deleteHandler) throws NacosException {
        final Listener listener = new Listener() {
            @Override
            public Executor getExecutor() {
                return null;
            }

            @Override
            public void receiveConfigInfo(final String configInfo) {
                try {
                    if (StringUtils.isBlank(configInfo) && deleteHandler != null) {
                        deleteHandler.accept(key);
                    } else {
                        updateHandler.accept(configInfo);
                    }
                } catch (Exception e) {
                    LOG.error("nacos sync listener receiveConfigInfo error", e);
                }
            }
        };
        final String serviceConfig = configService.getConfigAndSignListener(key, NacosPathConstants.GROUP, 3000, listener);
        watchCache.put(key, listener);
        return serviceConfig;
    }

    private String getConfigOnWatch(final String key, final Consumer<String> updateHandler, final Consumer<String> deleteHandler) {
        try {
            if (watchCache.containsKey(key)) {
                return null;
            }
            return getServiceConfig(key, updateHandler, deleteHandler);
        } catch (Exception e) {
            throw new ShenyuException(e);
        }
    }

    private void cachePluginData(final String dataString) {
        final PluginData pluginData = GsonUtils.getInstance().fromJson(dataString, PluginData.class);
        Optional.ofNullable(pluginData)
                .flatMap(data -> Optional.ofNullable(pluginDataSubscriber)).ifPresent(e -> e.onSubscribe(pluginData));
    }

    private void unCachePluginData(final String pluginName) {
        final PluginData data = new PluginData();
        data.setName(pluginName);
        Optional.ofNullable(pluginDataSubscriber).ifPresent(e -> e.unSubscribe(data));
    }

    private void cacheSelectorData(final String dataString) {
        final SelectorData selectorData = GsonUtils.getInstance().fromJson(dataString, SelectorData.class);
        Optional.ofNullable(selectorData)
                .ifPresent(data -> Optional.ofNullable(pluginDataSubscriber).ifPresent(e -> e.onSelectorSubscribe(data)));
    }

    private void unCacheSelectorData(final String removeKey) {
        final SelectorData selectorData = new SelectorData();
        final String[] ruleKeys = StringUtils.split(removeKey, ".");
        selectorData.setPluginName(ruleKeys[1]);
        selectorData.setId(ruleKeys[2]);
        Optional.ofNullable(pluginDataSubscriber).ifPresent(e -> e.unSelectorSubscribe(selectorData));
        removeListener(removeKey);
    }

    private void cacheRuleData(final String dataString) {
        final RuleData ruleData = GsonUtils.getInstance().fromJson(dataString, RuleData.class);
        Optional.ofNullable(ruleData)
                .ifPresent(data -> Optional.ofNullable(pluginDataSubscriber).ifPresent(e -> e.onRuleSubscribe(data)));
    }

    private void unCacheRuleData(final String removeKey, final String pluginName) {
        final RuleData ruleData = new RuleData();
        final String[] ruleKeys = StringUtils.split(removeKey, ".");
        ruleData.setSelectorId(ruleKeys[1]);
        ruleData.setId(ruleKeys[2]);
        ruleData.setPluginName(pluginName);
        Optional.ofNullable(pluginDataSubscriber).ifPresent(e -> e.unRuleSubscribe(ruleData));
        removeListener(removeKey);
    }

    private void cacheAuthData(final String dataString) {
        final AppAuthData appAuthData = GsonUtils.getInstance().fromJson(dataString, AppAuthData.class);
        Optional.ofNullable(appAuthData)
                .ifPresent(data -> authDataSubscribers.forEach(e -> e.onSubscribe(data)));
    }

    private void unCacheAuthData(final String removeKey) {
        final AppAuthData appAuthData = new AppAuthData();
        final String[] ruleKeys = StringUtils.split(removeKey, ".");
        appAuthData.setAppKey(ruleKeys[1]);
        authDataSubscribers.forEach(e -> e.unSubscribe(appAuthData));
        removeListener(removeKey);
    }

    private void cacheMetaData(final String dataString) {
        final MetaData metaData = GsonUtils.getInstance().fromJson(dataString, MetaData.class);
        Optional.ofNullable(metaData)
                .ifPresent(data -> metaDataSubscribers.forEach(e -> e.onSubscribe(metaData)));
    }

    private void unCacheMetaData(final String removeKey) {
        final MetaData metaData = new MetaData();
        final String[] ruleKeys = StringUtils.split(removeKey, ".");
        metaData.setId(ruleKeys[1]);
        metaDataSubscribers.forEach(e -> e.unSubscribe(metaData));
        removeListener(removeKey);
    }

    private void cacheProxySelectorData(final String dataString) {
        final ProxySelectorData proxySelectorData = GsonUtils.getInstance().fromJson(dataString, ProxySelectorData.class);
        Optional.ofNullable(proxySelectorData)
                .ifPresent(data -> proxySelectorDataSubscribers.forEach(e -> e.onSubscribe(data)));
    }

    private void unCacheProxySelectorData(final String removeKey) {
        ProxySelectorData proxySelectorData = new ProxySelectorData();
        final String[] proxySelectorKeys = StringUtils.split(removeKey, ".");
        proxySelectorData.setPluginName(proxySelectorKeys[2]);
        proxySelectorData.setName(proxySelectorKeys[3]);
        proxySelectorDataSubscribers.forEach(e -> e.unSubscribe(proxySelectorData));
        removeListener(removeKey);
    }

    @Override
    public void close() {
        LISTENERS.forEach((dataId, lss) -> {
            lss.forEach(listener -> getConfigService().removeListener(dataId, NacosPathConstants.GROUP, listener));
            lss.clear();
        });
        LISTENERS.clear();
    }
}
