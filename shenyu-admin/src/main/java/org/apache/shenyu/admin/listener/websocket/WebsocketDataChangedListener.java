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
import org.apache.shenyu.admin.model.dto.DiscoverySyncPayload;
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

    private final boolean compactDiscoveryPayload;

    /**
     * Create a listener using the legacy payload.
     */
    public WebsocketDataChangedListener() {
        this(false);
    }

    /**
     * Create a listener with optional Discovery payload projection.
     *
     * @param compactDiscoveryPayload whether to omit Discovery management fields
     */
    public WebsocketDataChangedListener(final boolean compactDiscoveryPayload) {
        this.compactDiscoveryPayload = compactDiscoveryPayload;
    }

    @Override
    public void onPluginChanged(final List<PluginData> pluginDataList, final DataEventTypeEnum eventType) {
        sendByNamespace(pluginDataList, eventType, ConfigGroupEnum.PLUGIN, PluginData::getNamespaceId);
    }

    @Override
    public void onPluginChanged(final List<PluginData> changed, final DataEventTypeEnum eventType,
                                final String namespaceId) {
        if (eventType == DataEventTypeEnum.REFRESH && StringUtils.isNotBlank(namespaceId)) {
            sendNamespaceSnapshot(ConfigGroupEnum.PLUGIN, namespaceId, changed);
            return;
        }
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
        if (eventType == DataEventTypeEnum.REFRESH && StringUtils.isNotBlank(namespaceId)) {
            sendNamespaceSnapshot(ConfigGroupEnum.SELECTOR, namespaceId, changed);
            return;
        }
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
        if (eventType == DataEventTypeEnum.REFRESH && StringUtils.isNotBlank(namespaceId)) {
            sendNamespaceSnapshot(ConfigGroupEnum.RULE, namespaceId, changed);
            return;
        }
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

    /**
     * Send a complete-group snapshot for one namespace. A namespace-addressed REFRESH publishes the whole
     * group of that namespace (see {@code SyncDataService#syncAllByNamespaceId}), so it must be applied as
     * a full snapshot: a payload-derived plugin scope could not remove a plugin bucket whose last row was
     * deleted, because that plugin no longer appears in the payload.
     *
     * @param group config group to replace
     * @param namespaceId namespace to replace
     * @param dataList complete group of the namespace, possibly empty
     */
    private <T> void sendNamespaceSnapshot(final ConfigGroupEnum group, final String namespaceId, final List<T> dataList) {
        WebsocketData<T> websocketData = new WebsocketData<>(group.name(), DataEventTypeEnum.REFRESH.name(), dataList);
        websocketData.setNamespaceId(namespaceId);
        websocketData.setFullSnapshot(true);
        WebsocketCollector.send(namespaceId, GsonUtils.getInstance().toJson(websocketData), DataEventTypeEnum.REFRESH);
        LOG.info("websocket namespace snapshot delivered, group={}, namespaceId={}, count={}",
                group.name(), namespaceId, dataList.size());
    }

    private void sendEmptySnapshot(final ConfigGroupEnum group, final DataEventTypeEnum eventType,
                                   final String namespaceId) {
        if (StringUtils.isBlank(namespaceId)
                || (eventType != DataEventTypeEnum.REFRESH && eventType != DataEventTypeEnum.MYSELF)) {
            return;
        }
        WebsocketData<Object> websocketData =
                new WebsocketData<>(group.name(), eventType.name(), Collections.emptyList());
        WebsocketCollector.sendConfiguration(namespaceId, websocketData, eventType);
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
        if (compactDiscoveryPayload && CollectionUtils.isNotEmpty(discoveryUpstreamList)) {
            for (Map.Entry<String, List<DiscoverySyncData>> entry : groupByNamespace(discoveryUpstreamList, DiscoverySyncData::getNamespaceId).entrySet()) {
                WebsocketCollector.sendDiscovery(entry.getKey(),
                        () -> GsonUtils.getInstance().toJson(new WebsocketData<>(ConfigGroupEnum.DISCOVER_UPSTREAM.name(), eventType.name(), entry.getValue())),
                        () -> {
                            List<DiscoverySyncPayload> payloads = new ArrayList<>(entry.getValue().size());
                            entry.getValue().forEach(data -> payloads.add(DiscoverySyncPayload.from(data)));
                            return GsonUtils.getInstance().toJson(new WebsocketData<>(ConfigGroupEnum.DISCOVER_UPSTREAM.name(), eventType.name(), payloads));
                        }, eventType);
                LOG.info("websocket config delivered, group={}, eventType={}, namespaceId={}, count={}",
                        ConfigGroupEnum.DISCOVER_UPSTREAM, eventType, entry.getKey(), entry.getValue().size());
            }
            return;
        }
        sendByNamespace(discoveryUpstreamList, eventType, ConfigGroupEnum.DISCOVER_UPSTREAM, DiscoverySyncData::getNamespaceId);
    }

    private <T> void sendByNamespace(final List<T> changed, final DataEventTypeEnum eventType,
                                     final ConfigGroupEnum group, final Function<T, String> namespaceOf) {
        if (CollectionUtils.isEmpty(changed)) {
            return;
        }
        for (Map.Entry<String, List<T>> entry : groupByNamespace(changed, namespaceOf).entrySet()) {
            List<T> groupData = entry.getValue();
            WebsocketData<T> websocketData = new WebsocketData<>(group.name(), eventType.name(), groupData);
            WebsocketCollector.sendConfiguration(entry.getKey(), websocketData, eventType);
            LOG.info("websocket config delivered, group={}, eventType={}, namespaceId={}, count={}",
                    group.name(), eventType.name(), entry.getKey(), groupData.size());
        }
    }

    private <T> Map<String, List<T>> groupByNamespace(final List<T> changed, final Function<T, String> namespaceOf) {
        Map<String, List<T>> byNamespace = new LinkedHashMap<>();
        for (T item : changed) {
            if (Objects.isNull(item)) {
                continue;
            }
            String namespaceId = StringUtils.defaultString(namespaceOf.apply(item), SYS_DEFAULT_NAMESPACE_ID);
            byNamespace.computeIfAbsent(namespaceId, key -> new ArrayList<>()).add(item);
        }
        return byNamespace;
    }
}
