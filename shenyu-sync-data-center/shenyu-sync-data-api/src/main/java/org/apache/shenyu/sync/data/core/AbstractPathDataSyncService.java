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

import com.google.common.base.Splitter;
import com.google.common.collect.Lists;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.DefaultPathConstants;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.DiscoveryUpstreamDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.api.ProxySelectorDataSubscriber;
import org.apache.shenyu.sync.data.api.SyncDataService;

import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Optional;

/**
 * AbstractPathDataSyncService.
 * Abstract method to monitor child node changes.
 */
public abstract class AbstractPathDataSyncService implements SyncDataService {

    private final PluginDataSubscriber pluginDataSubscriber;

    private final List<MetaDataSubscriber> metaDataSubscribers;

    private final List<AuthDataSubscriber> authDataSubscribers;

    private final List<ProxySelectorDataSubscriber> proxySelectorDataSubscribers;

    private final List<DiscoveryUpstreamDataSubscriber> discoveryUpstreamDataSubscribers;

    public AbstractPathDataSyncService(final PluginDataSubscriber pluginDataSubscriber,
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

    /**
     * event.
     *
     * @param namespaceId namespaceId
     * @param updatePath updatePath
     * @param updateData updateData
     * @param registerPath registerPath
     * @param eventType eventType
     */
    public void event(final String namespaceId, final String updatePath, final String updateData, final String registerPath, final EventType eventType) {
        String realUpdatePath = StringUtils.substringAfter(updatePath, namespaceId);
        String realRegisterPath = StringUtils.substringAfter(registerPath, namespaceId);
        switch (realRegisterPath) {
            case DefaultPathConstants.PLUGIN_PARENT:
                pluginHandlerEvent(realUpdatePath, updateData, eventType);
                break;
            case DefaultPathConstants.SELECTOR_PARENT:
                selectorHandlerEvent(realUpdatePath, updateData, eventType);
                break;
            case DefaultPathConstants.META_DATA:
                metaDataHandlerEvent(realUpdatePath, updateData, eventType);
                break;
            case DefaultPathConstants.APP_AUTH_PARENT:
                appAuthHandlerEvent(realUpdatePath, updateData, eventType);
                break;
            case DefaultPathConstants.RULE_PARENT:
                ruleHandlerEvent(realUpdatePath, updateData, eventType);
                break;
            case DefaultPathConstants.DISCOVERY_UPSTREAM:
                discoveryUpstreamHandlerEvent(realUpdatePath, updateData, eventType);
                break;
            case DefaultPathConstants.PROXY_SELECTOR:
                proxyHandlerEvent(realUpdatePath, updateData, eventType);
                break;
            default:
                break;
        }
    }

    private void proxyHandlerEvent(final String updatePath, final String updateData, final EventType eventType) {
        String[] pathInfoArray = updatePath.split("/");
        if (pathInfoArray.length != 5) {
            return;
        }
        String pluginName = pathInfoArray[pathInfoArray.length - 2];
        String proxySelectorName = pathInfoArray[pathInfoArray.length - 1];
        if (EventType.DELETE.equals(eventType)) {
            ProxySelectorData proxySelectorData = new ProxySelectorData();
            proxySelectorData.setPluginName(pluginName);
            proxySelectorData.setName(proxySelectorName);
            unCacheProxySelectorData(proxySelectorData);
            return;
        }
        ProxySelectorData proxySelectorData = GsonUtils.getInstance().fromJson(updateData, ProxySelectorData.class);
        proxySelectorData.setName(proxySelectorName);
        proxySelectorData.setPluginName(pluginName);
        Optional.ofNullable(updateData)
                .ifPresent(e -> cacheProxySelectorData(proxySelectorData));
    }

    private void discoveryUpstreamHandlerEvent(final String updatePath, final String updateData, final EventType eventType) {
        String[] pathInfoArray2 = updatePath.split("/");
        if (pathInfoArray2.length != 5) {
            return;
        }
        if (!EventType.DELETE.equals(eventType)) {
            Optional.ofNullable(updateData)
                    .ifPresent(e -> cacheDiscoveryUpstreamData(GsonUtils.getInstance().fromJson(updateData, DiscoverySyncData.class)));
        }
    }

    private void ruleHandlerEvent(final String updatePath, final String updateData, final EventType eventType) {
        if (EventType.DELETE.equals(eventType)) {
            unCacheRuleData(updatePath);
            return;
        }
        Optional.ofNullable(updateData)
                .ifPresent(e -> cacheRuleData(GsonUtils.getInstance().fromJson(updateData, RuleData.class)));
    }

    private void appAuthHandlerEvent(final String updatePath, final String updateData, final EventType eventType) {
        if (EventType.DELETE.equals(eventType)) {
            unCacheAuthData(updatePath);
            return;
        }
        Optional.ofNullable(updateData)
                .ifPresent(e -> cacheAuthData(GsonUtils.getInstance().fromJson(updateData, AppAuthData.class)));
    }

    private void metaDataHandlerEvent(final String updatePath, final String updateData, final EventType eventType) {
        if (EventType.DELETE.equals(eventType)) {
            final String realPath = updatePath.substring(DefaultPathConstants.META_DATA.length() + 1);
            MetaData metaData = new MetaData();
            metaData.setPath(URLDecoder.decode(realPath, StandardCharsets.UTF_8));
            unCacheMetaData(metaData);
            return;
        }
        Optional.ofNullable(updateData)
                .ifPresent(e -> cacheMetaData(GsonUtils.getInstance().fromJson(updateData, MetaData.class)));
    }

    private void selectorHandlerEvent(final String updatePath, final String updateData, final EventType eventType) {
        if (EventType.DELETE.equals(eventType)) {
            unCacheSelectorData(updatePath);
            return;
        }
        Optional.ofNullable(updateData)
                .ifPresent(e -> cacheSelectorData(GsonUtils.getInstance().fromJson(updateData, SelectorData.class)));
    }

    private void pluginHandlerEvent(final String updatePath, final String updateData, final EventType eventType) {
        if (EventType.DELETE.equals(eventType)) {
            String pluginName = updatePath.substring(updatePath.lastIndexOf("/") + 1);
            unCachePluginName(pluginName);
            return;
        }
        Optional.ofNullable(updateData)
                .ifPresent(e -> cachePluginData(GsonUtils.getInstance().fromJson(updateData, PluginData.class)));
    }

    public enum EventType {
        PUT,
        DELETE,
    }

    protected void cachePluginData(final PluginData pluginData) {
        Optional.ofNullable(pluginData)
                .flatMap(data -> Optional.ofNullable(pluginDataSubscriber))
                .ifPresent(e -> e.onSubscribe(pluginData));
    }

    protected void cacheSelectorData(final SelectorData selectorData) {
        Optional.ofNullable(selectorData)
                .ifPresent(data -> Optional.ofNullable(pluginDataSubscriber)
                        .ifPresent(e -> e.onSelectorSubscribe(data)));
    }

    protected void unCacheSelectorData(final String dataPath) {
        SelectorData selectorData = new SelectorData();
        final String selectorId = dataPath.substring(dataPath.lastIndexOf("/") + 1);
        final String str = dataPath.substring(DefaultPathConstants.SELECTOR_PARENT.length());
        final int pluginNameIndex = str.length() - selectorId.length() - 1;
        if (pluginNameIndex <= 0) {
            return;
        }
        final String pluginName = str.substring(1, pluginNameIndex);
        selectorData.setPluginName(pluginName);
        selectorData.setId(selectorId);

        Optional.ofNullable(pluginDataSubscriber)
                .ifPresent(e -> e.unSelectorSubscribe(selectorData));
    }

    protected void unCachePluginName(final String pluginName) {
        final PluginData pluginData = new PluginData();
        pluginData.setName(pluginName);
        Optional.ofNullable(pluginDataSubscriber).ifPresent(e -> e.unSubscribe(pluginData));
    }

    protected void cacheRuleData(final RuleData ruleData) {
        Optional.ofNullable(ruleData)
                .ifPresent(data -> Optional.ofNullable(pluginDataSubscriber)
                        .ifPresent(e -> e.onRuleSubscribe(data)));
    }

    protected void unCacheRuleData(final String dataPath) {
        String ruleDataId = dataPath.substring(dataPath.lastIndexOf("/") + 1);
        final String str = dataPath.substring(DefaultPathConstants.RULE_PARENT.length());
        final int pluginNameIndex = str.length() - ruleDataId.length() - 1;
        if (pluginNameIndex <= 0) {
            return;
        }
        final String pluginName = str.substring(1, pluginNameIndex);
        final List<String> list = Lists.newArrayList(Splitter.on(DefaultPathConstants.SELECTOR_JOIN_RULE).split(ruleDataId));

        RuleData ruleData = new RuleData();
        ruleData.setPluginName(pluginName);
        ruleData.setSelectorId(list.get(0));
        ruleData.setId(list.get(1));

        Optional.ofNullable(pluginDataSubscriber)
                .ifPresent(e -> e.unRuleSubscribe(ruleData));
    }

    protected void cacheAuthData(final AppAuthData appAuthData) {
        Optional.ofNullable(appAuthData)
                .ifPresent(data -> authDataSubscribers.forEach(e -> e.onSubscribe(data)));
    }

    protected void unCacheAuthData(final String dataPath) {
        final String key = dataPath.substring(DefaultPathConstants.APP_AUTH_PARENT.length() + 1);
        AppAuthData appAuthData = new AppAuthData();
        appAuthData.setAppKey(key);
        authDataSubscribers.forEach(e -> e.unSubscribe(appAuthData));
    }

    protected void cacheMetaData(final MetaData metaData) {
        Optional.ofNullable(metaData)
                .ifPresent(data -> metaDataSubscribers.forEach(e -> e.onSubscribe(metaData)));
    }

    protected void cacheProxySelectorData(final ProxySelectorData proxySelectorData) {
        Optional.ofNullable(proxySelectorData)
                .ifPresent(data -> proxySelectorDataSubscribers.forEach(e -> e.onSubscribe(proxySelectorData)));
    }

    protected void cacheDiscoveryUpstreamData(final DiscoverySyncData upstreamDataList) {
        Optional.ofNullable(discoveryUpstreamDataSubscribers)
                .ifPresent(data -> discoveryUpstreamDataSubscribers.forEach(e -> e.onSubscribe(upstreamDataList)));
    }

    protected void unCacheMetaData(final MetaData metaData) {
        Optional.ofNullable(metaData)
                .ifPresent(data -> metaDataSubscribers.forEach(e -> e.unSubscribe(metaData)));
    }

    protected void unCacheProxySelectorData(final ProxySelectorData proxySelectorData) {
        Optional.ofNullable(proxySelectorData)
                .ifPresent(data -> proxySelectorDataSubscribers.forEach(e -> e.unSubscribe(proxySelectorData)));
    }
}
