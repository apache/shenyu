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

package org.apache.shenyu.admin.listener;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.ProxyApiKeyData;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.DataEventTypeEnum;

import java.util.List;
import java.util.function.Function;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;

/**
 * Event listener, used to send notification of event changes,
 * used to support HTTP, websocket, zookeeper and other event notifications.
 */
public interface DataChangedListener {

    /**
     * invoke this method when AppAuth was received.
     *
     * @param changed   the changed
     * @param eventType the event type
     */
    default void onAppAuthChanged(List<AppAuthData> changed, DataEventTypeEnum eventType) {
    }

    /**
     * Invoke this method when AppAuth was received for a known namespace.
     *
     * @param changed changed data
     * @param eventType event type
     * @param namespaceId namespace id
     */
    default void onAppAuthChanged(List<AppAuthData> changed, DataEventTypeEnum eventType, String namespaceId) {
        onAppAuthChanged(changed, eventType);
    }

    /**
     * invoke this method when Plugin was received.
     *
     * @param changed   the changed
     * @param eventType the event type
     */
    default void onPluginChanged(List<PluginData> changed, DataEventTypeEnum eventType) {
    }

    /**
     * Invoke this method when Plugin was received for a known namespace.
     *
     * @param changed changed data
     * @param eventType event type
     * @param namespaceId namespace id
     */
    default void onPluginChanged(List<PluginData> changed, DataEventTypeEnum eventType, String namespaceId) {
        onPluginChanged(changed, eventType);
    }

    /**
     * invoke this method when Selector was received.
     *
     * @param changed   the changed
     * @param eventType the event type
     */
    default void onSelectorChanged(List<SelectorData> changed, DataEventTypeEnum eventType) {
    }

    /**
     * Invoke this method when Selector was received for a known namespace.
     *
     * @param changed changed data
     * @param eventType event type
     * @param namespaceId namespace id
     */
    default void onSelectorChanged(List<SelectorData> changed, DataEventTypeEnum eventType, String namespaceId) {
        onSelectorChanged(changed, eventType);
    }

    /**
     * On meta data changed.
     *
     * @param changed   the changed
     * @param eventType the event type
     */
    default void onMetaDataChanged(List<MetaData> changed, DataEventTypeEnum eventType) {

    }

    /**
     * invoke this method when Rule was received.
     *
     * @param changed   the changed
     * @param eventType the event type
     */
    default void onRuleChanged(List<RuleData> changed, DataEventTypeEnum eventType) {
    }

    /**
     * Invoke this method when Rule was received for a known namespace.
     *
     * @param changed changed data
     * @param eventType event type
     * @param namespaceId namespace id
     */
    default void onRuleChanged(List<RuleData> changed, DataEventTypeEnum eventType, String namespaceId) {
        onRuleChanged(changed, eventType);
    }

    /**
     * invoke this method when ProxySelector was changed.
     *
     * @param changed   the changed
     * @param eventType the event type
     */
    default void onProxySelectorChanged(List<ProxySelectorData> changed, DataEventTypeEnum eventType) {
    }

    /**
     * invoke this method when DiscoveryUpstream was changed.
     *
     * @param changed   the changed
     * @param eventType the event type
     */
    default void onDiscoveryUpstreamChanged(List<DiscoverySyncData> changed, DataEventTypeEnum eventType) {
    }

    /**
     * invoke this method when AiProxyApiKey was changed.
     *
     * @param changed   the changed
     * @param eventType the event type
     */
    default void onAiProxyApiKeyChanged(List<ProxyApiKeyData> changed, DataEventTypeEnum eventType) {
    }

    /**
     * Resolve the namespace of an event carrying a snapshot.
     *
     * <p>An explicit namespace wins over the payload, so an empty snapshot can still be reconciled.
     *
     * @param namespaceId the namespace of the event, may be blank
     * @param changed the snapshot
     * @param namespaceGetter the namespace accessor of the snapshot type
     * @param <T> the snapshot type
     * @return the namespace, or null when the event names neither a namespace nor a snapshot
     */
    static <T> String snapshotNamespace(final String namespaceId, final List<T> changed,
                                        final Function<? super T, ? extends String> namespaceGetter) {
        if (StringUtils.isNotBlank(namespaceId)) {
            return namespaceId;
        }
        if (CollectionUtils.isEmpty(changed)) {
            return null;
        }
        return StringUtils.defaultString(namespaceGetter.apply(changed.get(0)), SYS_DEFAULT_NAMESPACE_ID);
    }

    /**
     * Whether the event is a full snapshot of one namespace.
     *
     * @param eventType the event type
     * @param namespaceId the namespace of the event
     * @return true when the event is a namespace-scoped REFRESH
     */
    static boolean isNamespaceRefresh(final DataEventTypeEnum eventType, final String namespaceId) {
        return DataEventTypeEnum.REFRESH == eventType && StringUtils.isNotBlank(namespaceId);
    }

}
