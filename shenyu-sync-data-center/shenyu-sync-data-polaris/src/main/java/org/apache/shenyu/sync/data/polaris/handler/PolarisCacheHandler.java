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

package org.apache.shenyu.sync.data.polaris.handler;

import com.google.common.collect.Maps;
import com.google.gson.JsonParseException;
import com.tencent.polaris.api.exception.PolarisException;
import com.tencent.polaris.configuration.api.core.ConfigFile;
import com.tencent.polaris.configuration.api.core.ConfigFileChangeListener;
import com.tencent.polaris.configuration.api.core.ConfigFileService;
import org.apache.shenyu.common.constant.PolarisPathConstants;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.MapUtils;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.api.ProxySelectorDataSubscriber;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Polaris cache handler.
 */
public class PolarisCacheHandler {

    protected static final Map<String, List<ConfigFileChangeListener>> LISTENERS = Maps.newConcurrentMap();

    private static final Logger LOG = LoggerFactory.getLogger(PolarisCacheHandler.class);

    private final ConfigFileService configFileService;

    private final PluginDataSubscriber pluginDataSubscriber;

    private final List<MetaDataSubscriber> metaDataSubscribers;

    private final List<AuthDataSubscriber> authDataSubscribers;

    private final List<ProxySelectorDataSubscriber> proxySelectorDataSubscribers;

    public PolarisCacheHandler(final ConfigFileService configFileService, final PluginDataSubscriber pluginDataSubscriber,
                               final List<MetaDataSubscriber> metaDataSubscribers,
                               final List<AuthDataSubscriber> authDataSubscribers,
                               final List<ProxySelectorDataSubscriber> proxySelectorDataSubscribers) {
        this.configFileService = configFileService;
        this.pluginDataSubscriber = pluginDataSubscriber;
        this.metaDataSubscribers = metaDataSubscribers;
        this.authDataSubscribers = authDataSubscribers;
        this.proxySelectorDataSubscribers = proxySelectorDataSubscribers;
    }

    /**
     * get configFileService.
     *
     * @return configFileService
     */
    public ConfigFileService getConfigFileService() {
        return this.configFileService;
    }

    protected void updatePluginMap(final String configInfo) {
        try {
            List<PluginData> pluginDataList = new ArrayList<>(GsonUtils.getInstance().toObjectMap(configInfo, PluginData.class).values());
            pluginDataList.forEach(pluginData -> Optional.ofNullable(pluginDataSubscriber).ifPresent(subscriber -> {
                subscriber.unSubscribe(pluginData);
                subscriber.onSubscribe(pluginData);
            }));
        } catch (JsonParseException e) {
            LOG.error("sync plugin data have error:", e);
        }
    }

    protected void updateSelectorMap(final String configInfo) {
        try {
            List<SelectorData> selectorDataList = GsonUtils.getInstance().toObjectMapList(configInfo, SelectorData.class).values().stream().flatMap(Collection::stream).collect(Collectors.toList());
            selectorDataList.forEach(selectorData -> Optional.ofNullable(pluginDataSubscriber).ifPresent(subscriber -> {
                subscriber.unSelectorSubscribe(selectorData);
                subscriber.onSelectorSubscribe(selectorData);
            }));
        } catch (JsonParseException e) {
            LOG.error("sync selector data have error:", e);
        }
    }

    protected void updateRuleMap(final String configInfo) {
        try {
            List<RuleData> ruleDataList = GsonUtils.getInstance().toObjectMapList(configInfo, RuleData.class).values()
                    .stream().flatMap(Collection::stream)
                    .collect(Collectors.toList());
            ruleDataList.forEach(ruleData -> Optional.ofNullable(pluginDataSubscriber).ifPresent(subscriber -> {
                subscriber.unRuleSubscribe(ruleData);
                subscriber.onRuleSubscribe(ruleData);
            }));
        } catch (JsonParseException e) {
            LOG.error("sync rule data have error:", e);
        }
    }

    protected void updateMetaDataMap(final String configInfo) {
        try {
            List<MetaData> metaDataList = new ArrayList<>(GsonUtils.getInstance().toObjectMap(configInfo, MetaData.class).values());
            metaDataList.forEach(metaData -> metaDataSubscribers.forEach(subscriber -> {
                subscriber.unSubscribe(metaData);
                subscriber.onSubscribe(metaData);
            }));
        } catch (JsonParseException e) {
            LOG.error("sync meta data have error:", e);
        }
    }

    protected void updateAuthMap(final String configInfo) {
        try {
            List<AppAuthData> appAuthDataList = new ArrayList<>(GsonUtils.getInstance().toObjectMap(configInfo, AppAuthData.class).values());
            appAuthDataList.forEach(appAuthData -> authDataSubscribers.forEach(subscriber -> {
                subscriber.unSubscribe(appAuthData);
                subscriber.onSubscribe(appAuthData);
            }));
        } catch (JsonParseException e) {
            LOG.error("sync auth data have error:", e);
        }
    }

    protected void updateProxySelectorMap(final String configInfo) {
        try {
            List<ProxySelectorData> proxySelectorDataList = new ArrayList<>(GsonUtils.getInstance().toObjectMap(configInfo, ProxySelectorData.class).values());
            proxySelectorDataList.forEach(proxySelectorData -> proxySelectorDataSubscribers.forEach(subscriber -> {
                subscriber.unSubscribe(proxySelectorData);
            }));
        } catch (JsonParseException e) {
            LOG.error("sync proxy selector data have error", e);
        }
    }

    private String getConfigAndSignListener(final String dataId, final ConfigFileChangeListener listener) {
        String config = null;
        try {
            ConfigFile configFile = configFileService.getConfigFile(PolarisPathConstants.NAMESPACE, PolarisPathConstants.FILE_GROUP, dataId);
            configFile.addChangeListener(listener);
            config = configFile.getContent();

        } catch (PolarisException e) {
            LOG.error(e.getMessage(), e);
        }
        if (Objects.isNull(config)) {
            config = PolarisPathConstants.EMPTY_CONFIG_DEFAULT_VALUE;
        }
        return config;
    }

    protected void watcherData(final String dataId, final OnChange oc) {

        ConfigFileChangeListener listener = configFileChangeEvent -> {
            oc.change(configFileChangeEvent.getNewValue());
        };

        oc.change(getConfigAndSignListener(dataId, listener));
        MapUtils.computeIfAbsent(LISTENERS, dataId, key -> new ArrayList<>()).add(listener);
    }

    protected interface OnChange {
        void change(String changeData);
    }

}
