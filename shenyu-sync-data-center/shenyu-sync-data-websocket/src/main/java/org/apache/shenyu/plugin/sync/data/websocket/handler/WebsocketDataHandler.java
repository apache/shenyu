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

package org.apache.shenyu.plugin.sync.data.websocket.handler;

import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.utils.DigestUtils;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.DiscoveryUpstreamDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.api.ProxySelectorDataSubscriber;
import org.apache.shenyu.sync.data.api.AiProxyApiKeyDataSubscriber;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The type Websocket cache handler.
 */
public class WebsocketDataHandler {

    private static final Logger LOG = LoggerFactory.getLogger(WebsocketDataHandler.class);

    private final EnumMap<ConfigGroupEnum, DataHandler> handlers = new EnumMap<>(ConfigGroupEnum.class);

    /**
     * Fingerprint of the payload that was applied last for each group. Admin re-sends the whole
     * group payload on every change of that group, so an unchanged payload means the gateway has
     * already applied exactly this state and re-caching it only burns CPU on the sync thread.
     */
    private final Map<ConfigGroupEnum, String> lastAppliedFingerprints = new ConcurrentHashMap<>();

    /**
     * Instantiates a new Websocket data handler.
     *
     * @param pluginDataSubscriber the plugin data subscriber
     * @param metaDataSubscribers  the meta data subscribers
     * @param authDataSubscribers  the auth data subscribers
     */
    public WebsocketDataHandler(final PluginDataSubscriber pluginDataSubscriber,
                                final List<MetaDataSubscriber> metaDataSubscribers,
                                final List<AuthDataSubscriber> authDataSubscribers,
                                final List<ProxySelectorDataSubscriber> proxySelectorDataSubscribers,
                                final List<DiscoveryUpstreamDataSubscriber> discoveryUpstreamDataSubscribers,
                                final List<AiProxyApiKeyDataSubscriber> aiProxyApiKeyDataSubscribers) {
        handlers.put(ConfigGroupEnum.PLUGIN, new PluginDataHandler(pluginDataSubscriber));
        handlers.put(ConfigGroupEnum.SELECTOR, new SelectorDataHandler(pluginDataSubscriber));
        handlers.put(ConfigGroupEnum.RULE, new RuleDataHandler(pluginDataSubscriber));
        handlers.put(ConfigGroupEnum.APP_AUTH, new AuthDataHandler(authDataSubscribers));
        handlers.put(ConfigGroupEnum.META_DATA, new MetaDataHandler(metaDataSubscribers));
        handlers.put(ConfigGroupEnum.PROXY_SELECTOR, new ProxySelectorDataHandler(proxySelectorDataSubscribers));
        handlers.put(ConfigGroupEnum.DISCOVER_UPSTREAM, new DiscoveryUpstreamDataHandler(discoveryUpstreamDataSubscribers));
        handlers.put(ConfigGroupEnum.AI_PROXY_API_KEY, new AiProxyApiKeyDataHandler(aiProxyApiKeyDataSubscribers));
    }

    /**
     * Executor.
     *
     * @param type      the type
     * @param json      the json
     * @param eventType the event type
     */
    public void executor(final ConfigGroupEnum type, final String json, final String eventType) {
        final String fingerprint = fingerprintOf(json, eventType);
        if (Objects.equals(fingerprint, lastAppliedFingerprints.get(type))) {
            LOG.info("ignore duplicated {} event of group {}, this payload has already been applied", eventType, type);
            return;
        }
        handlers.get(type).handle(json, eventType);
        lastAppliedFingerprints.put(type, fingerprint);
    }

    private static String fingerprintOf(final String json, final String eventType) {
        return DigestUtils.md5Hex(String.join("|", String.valueOf(eventType), String.valueOf(json)));
    }

    /**
     * Apply a complete group snapshot after verifying the connection namespace.
     * @param type configuration group
     * @param json snapshot array
     * @param snapshotNamespace namespace supplied by Admin
     * @param connectionNamespace namespace configured on this connection
     */
    public void snapshot(final ConfigGroupEnum type, final String json,
                         final String snapshotNamespace, final String connectionNamespace) {
        if (java.util.Objects.isNull(snapshotNamespace) || snapshotNamespace.isEmpty()
                || !snapshotNamespace.equals(connectionNamespace)) {
            throw new IllegalArgumentException("Snapshot namespace does not match the connection");
        }
        ((AbstractDataHandler<?>) handlers.get(type)).handleSnapshot(json, snapshotNamespace);
        // a snapshot replaces the whole group, so the next payload must be applied even if it
        // happens to be identical to the payload applied before the snapshot
        lastAppliedFingerprints.remove(type);
    }

}
