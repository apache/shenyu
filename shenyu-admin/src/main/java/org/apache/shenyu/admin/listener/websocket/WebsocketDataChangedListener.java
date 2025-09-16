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

package org.apache.shenyu.admin.listener.websocket;

import java.util.List;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.listener.DataChangedListener;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.WebsocketData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;

/**
 * The type Websocket data changed listener.
 *
 * @since 2.0.0
 */
public class WebsocketDataChangedListener implements DataChangedListener {
    
    @Override
    public void onPluginChanged(final List<PluginData> pluginDataList, final DataEventTypeEnum eventType) {
        WebsocketData<PluginData> websocketData =
                new WebsocketData<>(ConfigGroupEnum.PLUGIN.name(), eventType.name(), pluginDataList);
        if (CollectionUtils.isNotEmpty(pluginDataList)) {
            String namespaceId = pluginDataList.stream().map(value -> StringUtils.defaultString(value.getNamespaceId(), SYS_DEFAULT_NAMESPACE_ID)).findFirst().get();
            WebsocketCollector.send(namespaceId, GsonUtils.getInstance().toJson(websocketData), eventType);
        }
    }
    
    @Override
    public void onSelectorChanged(final List<SelectorData> selectorDataList, final DataEventTypeEnum eventType) {
        WebsocketData<SelectorData> websocketData =
                new WebsocketData<>(ConfigGroupEnum.SELECTOR.name(), eventType.name(), selectorDataList);
        if (CollectionUtils.isNotEmpty(selectorDataList)) {
            String namespaceId = selectorDataList.stream().map(value -> StringUtils.defaultString(value.getNamespaceId(), SYS_DEFAULT_NAMESPACE_ID)).findFirst().get();
            WebsocketCollector.send(namespaceId, GsonUtils.getInstance().toJson(websocketData), eventType);
        }
    }
    
    @Override
    public void onRuleChanged(final List<RuleData> ruleDataList, final DataEventTypeEnum eventType) {
        WebsocketData<RuleData> configData =
                new WebsocketData<>(ConfigGroupEnum.RULE.name(), eventType.name(), ruleDataList);
        if (CollectionUtils.isNotEmpty(ruleDataList)) {
            String namespaceId = ruleDataList.stream().map(value -> StringUtils.defaultString(value.getNamespaceId(), SYS_DEFAULT_NAMESPACE_ID)).findFirst().get();
            WebsocketCollector.send(namespaceId, GsonUtils.getInstance().toJson(configData), eventType);
        }
    }
    
    @Override
    public void onAppAuthChanged(final List<AppAuthData> appAuthDataList, final DataEventTypeEnum eventType) {
        WebsocketData<AppAuthData> configData =
                new WebsocketData<>(ConfigGroupEnum.APP_AUTH.name(), eventType.name(), appAuthDataList);
        if (CollectionUtils.isNotEmpty(appAuthDataList)) {
            String namespaceId = appAuthDataList.stream().map(value -> StringUtils.defaultString(value.getNamespaceId(), SYS_DEFAULT_NAMESPACE_ID)).findFirst().get();
            WebsocketCollector.send(namespaceId, GsonUtils.getInstance().toJson(configData), eventType);
        }
    }
    
    @Override
    public void onMetaDataChanged(final List<MetaData> metaDataList, final DataEventTypeEnum eventType) {
        WebsocketData<MetaData> configData =
                new WebsocketData<>(ConfigGroupEnum.META_DATA.name(), eventType.name(), metaDataList);
        if (CollectionUtils.isNotEmpty(metaDataList)) {
            String namespaceId = metaDataList.stream().map(value -> StringUtils.defaultString(value.getNamespaceId(), SYS_DEFAULT_NAMESPACE_ID)).findFirst().get();
            WebsocketCollector.send(namespaceId, GsonUtils.getInstance().toJson(configData), eventType);
        }
    }
    
    @Override
    public void onProxySelectorChanged(final List<ProxySelectorData> proxySelectorDataList, final DataEventTypeEnum eventType) {
        WebsocketData<ProxySelectorData> configData =
                new WebsocketData<>(ConfigGroupEnum.PROXY_SELECTOR.name(), eventType.name(), proxySelectorDataList);
        if (CollectionUtils.isNotEmpty(proxySelectorDataList)) {
            String namespaceId = proxySelectorDataList.stream().map(value -> StringUtils.defaultString(value.getNamespaceId(), SYS_DEFAULT_NAMESPACE_ID)).findFirst().get();
            WebsocketCollector.send(namespaceId, GsonUtils.getInstance().toJson(configData), eventType);
        }
    }
    
    @Override
    public void onDiscoveryUpstreamChanged(final List<DiscoverySyncData> discoveryUpstreamList, final DataEventTypeEnum eventType) {
        WebsocketData<DiscoverySyncData> configData =
                new WebsocketData<>(ConfigGroupEnum.DISCOVER_UPSTREAM.name(), eventType.name(), discoveryUpstreamList);
        if (CollectionUtils.isNotEmpty(discoveryUpstreamList)) {
            String namespaceId = discoveryUpstreamList.stream().map(value -> StringUtils.defaultString(value.getNamespaceId(), SYS_DEFAULT_NAMESPACE_ID)).findFirst().get();
            WebsocketCollector.send(namespaceId, GsonUtils.getInstance().toJson(configData), eventType);
        }
    }
    
}
