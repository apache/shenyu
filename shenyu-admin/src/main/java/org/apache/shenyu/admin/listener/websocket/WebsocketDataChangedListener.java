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

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.listener.DataChangedListener;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.ProxyApiKeyData;
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.WebsocketData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;

/**
 * The type Websocket data changed listener.
 *
 * @since 2.0.0
 */
public class WebsocketDataChangedListener implements DataChangedListener {

    private static final Logger LOG = LoggerFactory.getLogger(WebsocketDataChangedListener.class);

    @Override
    public void onPluginChanged(final List<PluginData> pluginDataList, final DataEventTypeEnum eventType) {
        sendByNamespace(pluginDataList, eventType, ConfigGroupEnum.PLUGIN, PluginData::getNamespaceId);
    }

    @Override
    public void onPluginChanged(final List<PluginData> changed, final DataEventTypeEnum eventType,
                                final String namespaceId) {
        if (CollectionUtils.isEmpty(changed)) {
            sendEmptySnapshot(ConfigGroupEnum.PLUGIN, eventType, namespaceId);
            return;
        }
        onPluginChanged(changed, eventType);
    }

    @Override
    public void onSelectorChanged(final List<SelectorData> selectorDataList, final DataEventTypeEnum eventType) {
        sendByNamespace(selectorDataList, eventType, ConfigGroupEnum.SELECTOR, SelectorData::getNamespaceId);
    }

    @Override
    public void onSelectorChanged(final List<SelectorData> changed, final DataEventTypeEnum eventType,
                                  final String namespaceId) {
        if (CollectionUtils.isEmpty(changed)) {
            sendEmptySnapshot(ConfigGroupEnum.SELECTOR, eventType, namespaceId);
            return;
        }
        onSelectorChanged(changed, eventType);
    }

    @Override
    public void onRuleChanged(final List<RuleData> ruleDataList, final DataEventTypeEnum eventType) {
        sendByNamespace(ruleDataList, eventType, ConfigGroupEnum.RULE, RuleData::getNamespaceId);
    }

    @Override
    public void onRuleChanged(final List<RuleData> changed, final DataEventTypeEnum eventType,
                              final String namespaceId) {
        if (CollectionUtils.isEmpty(changed)) {
            sendEmptySnapshot(ConfigGroupEnum.RULE, eventType, namespaceId);
            return;
        }
        onRuleChanged(changed, eventType);
    }

    @Override
    public void onAppAuthChanged(final List<AppAuthData> appAuthDataList, final DataEventTypeEnum eventType) {
        sendByNamespace(appAuthDataList, eventType, ConfigGroupEnum.APP_AUTH, AppAuthData::getNamespaceId);
    }

    @Override
    public void onAppAuthChanged(final List<AppAuthData> changed, final DataEventTypeEnum eventType,
                                 final String namespaceId) {
        if (CollectionUtils.isEmpty(changed)) {
            sendEmptySnapshot(ConfigGroupEnum.APP_AUTH, eventType, namespaceId);
            return;
        }
        onAppAuthChanged(changed, eventType);
    }

    private void sendEmptySnapshot(final ConfigGroupEnum group, final DataEventTypeEnum eventType,
                                   final String namespaceId) {
        if (StringUtils.isBlank(namespaceId)
                || (eventType != DataEventTypeEnum.REFRESH && eventType != DataEventTypeEnum.MYSELF)) {
            return;
        }
        WebsocketData<Object> websocketData =
                new WebsocketData<>(group.name(), eventType.name(), Collections.emptyList());
        WebsocketCollector.send(namespaceId, GsonUtils.getInstance().toJson(websocketData), eventType);
    }

    @Override
    public void onMetaDataChanged(final List<MetaData> metaDataList, final DataEventTypeEnum eventType) {
        sendByNamespace(metaDataList, eventType, ConfigGroupEnum.META_DATA, MetaData::getNamespaceId);
    }

    @Override
    public void onProxySelectorChanged(final List<ProxySelectorData> proxySelectorDataList, final DataEventTypeEnum eventType) {
        sendByNamespace(proxySelectorDataList, eventType, ConfigGroupEnum.PROXY_SELECTOR, ProxySelectorData::getNamespaceId);
    }

    @Override
    public void onAiProxyApiKeyChanged(final List<ProxyApiKeyData> changed, final DataEventTypeEnum eventType) {
        sendByNamespace(changed, eventType, ConfigGroupEnum.AI_PROXY_API_KEY, ProxyApiKeyData::getNamespaceId);
    }

    @Override
    public void onDiscoveryUpstreamChanged(final List<DiscoverySyncData> discoveryUpstreamList, final DataEventTypeEnum eventType) {
        sendByNamespace(discoveryUpstreamList, eventType, ConfigGroupEnum.DISCOVER_UPSTREAM, DiscoverySyncData::getNamespaceId);
    }

    private <T> void sendByNamespace(final List<T> changed, final DataEventTypeEnum eventType,
                                     final ConfigGroupEnum group, final Function<T, String> namespaceOf) {
        if (CollectionUtils.isEmpty(changed)) {
            return;
        }
        Map<String, List<T>> byNamespace = new LinkedHashMap<>();
        for (T item : changed) {
            if (Objects.isNull(item)) {
                continue;
            }
            String namespaceId = StringUtils.defaultString(namespaceOf.apply(item), SYS_DEFAULT_NAMESPACE_ID);
            byNamespace.computeIfAbsent(namespaceId, key -> new ArrayList<>()).add(item);
        }
        for (Map.Entry<String, List<T>> entry : byNamespace.entrySet()) {
            List<T> groupData = entry.getValue();
            WebsocketData<T> websocketData = new WebsocketData<>(group.name(), eventType.name(), groupData);
            WebsocketCollector.send(entry.getKey(), GsonUtils.getInstance().toJson(websocketData), eventType);
            LOG.info("websocket config delivered, group={}, eventType={}, namespaceId={}, count={}",
                    group.name(), eventType.name(), entry.getKey(), groupData.size());
        }
    }
}
