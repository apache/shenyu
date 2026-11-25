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
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.utils.DigestUtils;
import org.apache.shenyu.common.utils.InitialSyncApplication;
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

    private static final CompletableFuture<Boolean> APPLIED = CompletableFuture.completedFuture(true);

    private final EnumMap<ConfigGroupEnum, DataHandler> handlers = new EnumMap<>(ConfigGroupEnum.class);

    /**
     * Latest payload per group for the current connection.
     * Admin can resend the entire group payload, so an identical delivery can share a pending application
     * or be skipped after success. A failed rule update can be applied again on the next delivery.
     */
    private final Map<ConfigGroupEnum, PayloadApplication> lastApplications = new ConcurrentHashMap<>();

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
     * Discard payload application results when a new connection opens.
     */
    public void resetApplications() {
        lastApplications.clear();
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
        PayloadApplication previous = lastApplications.get(type);
        if (Objects.nonNull(previous) && previous.matchesAppliedOrPending(fingerprint)) {
            LOG.info("skip {} event of group {} because the same payload is applied or pending", eventType, type);
            return;
        }
        try {
            lastApplications.put(type, new PayloadApplication(fingerprint, apply(type, json, eventType)));
        } catch (RuntimeException ex) {
            // The failed payload may have modified the cache before throwing.
            if (Objects.nonNull(previous)) {
                lastApplications.remove(type, previous);
            }
            throw ex;
        }
    }

    private CompletableFuture<Boolean> apply(final ConfigGroupEnum type, final String json, final String eventType) {
        DataHandler handler = handlers.get(type);
        if (ConfigGroupEnum.RULE == type && DataEventTypeEnum.UPDATE.name().equals(eventType)) {
            CompletableFuture<Void> completion = InitialSyncApplication.observe(() -> handler.handle(json, eventType));
            if (InitialSyncApplication.isActive() && completion.isCompletedExceptionally()) {
                // Preserve immediate failure handling during the initial sync attempt.
                completion.join();
            }
            // A rejected ordinary rule update stays replayable on the same connection.
            return completion.handle((ignored, failure) -> Objects.isNull(failure));
        }
        handler.handle(json, eventType);
        return APPLIED;
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
        if (Objects.isNull(snapshotNamespace) || snapshotNamespace.isEmpty()
                || !snapshotNamespace.equals(connectionNamespace)) {
            throw new IllegalArgumentException("Snapshot namespace does not match the connection");
        }
        // The snapshot may replace previously applied rows, even if its application fails partway through.
        lastApplications.remove(type);
        ((AbstractDataHandler<?>) handlers.get(type)).handleSnapshot(json, snapshotNamespace);
    }

    private record PayloadApplication(String fingerprint, CompletableFuture<Boolean> applied) {

        private boolean matchesAppliedOrPending(final String candidate) {
            // A pending application is shared until its result is known.
            return fingerprint.equals(candidate) && applied.getNow(true);
        }
    }
}
