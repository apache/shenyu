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

import org.apache.commons.codec.digest.DigestUtils;
import org.apache.shenyu.admin.config.properties.ClusterProperties;
import org.apache.shenyu.admin.config.properties.WebsocketSyncProperties;
import org.apache.shenyu.admin.mode.cluster.service.ClusterSelectMasterService;
import org.apache.shenyu.admin.service.AiProxyApiKeyService;
import org.apache.shenyu.admin.service.AppAuthService;
import org.apache.shenyu.admin.service.DiscoveryUpstreamService;
import org.apache.shenyu.admin.service.MetaDataService;
import org.apache.shenyu.admin.service.NamespacePluginService;
import org.apache.shenyu.admin.service.ProxySelectorService;
import org.apache.shenyu.admin.service.RuleService;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.dto.WebsocketData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.ObjectProvider;

import java.util.Collections;
import java.util.Comparator;
import java.util.stream.Collectors;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.TimeUnit;

/**
 * Reconciles the configuration pushed through WebSocket data sync with the state
 * stored in the database.
 *
 * <p>{@code DataChangedEvent} is a local Spring event and {@link WebsocketCollector}
 * sessions are local JVM state, so when several standalone admin nodes (cluster mode
 * disabled) share one database, a change written through one admin node never reaches
 * the gateway sessions connected to the other admin nodes. This task periodically
 * compares a digest of every configuration group per namespace with the database state
 * and pushes a full {@link DataEventTypeEnum#REFRESH} of the changed groups to the
 * gateway sessions connected to this admin node, so all gateways converge within the
 * configured interval without manual synchronization.</p>
 *
 * <p>Namespaces without connected gateway sessions are skipped, because there is
 * nothing to converge on this node. When cluster mode is enabled, non-master nodes
 * skip every cycle and reconciliation is owned by the master node. The digest cursor
 * is only advanced after a successful load and push, so failures are retried in the
 * next cycle. A full refresh does not modify the database, so a pushed cycle can
 * never trigger itself again.</p>
 */
public class WebsocketDataReconciler implements InitializingBean, DisposableBean {

    private static final Logger LOG = LoggerFactory.getLogger(WebsocketDataReconciler.class);

    private final ScheduledThreadPoolExecutor executor;

    private final WebsocketSyncProperties websocketSyncProperties;

    private final ClusterProperties clusterProperties;

    private final ObjectProvider<ClusterSelectMasterService> masterServiceProvider;

    private final AppAuthService appAuthService;

    private final NamespacePluginService namespacePluginService;

    private final SelectorService selectorService;

    private final RuleService ruleService;

    private final MetaDataService metaDataService;

    private final ProxySelectorService proxySelectorService;

    private final DiscoveryUpstreamService discoveryUpstreamService;

    private final AiProxyApiKeyService aiProxyApiKeyService;

    /**
     * Digest of the last pushed configuration group, keyed by {@code namespaceId:group}.
     * The cursor is in-memory only: after an admin restart the first cycle pushes a
     * full refresh for every connected namespace, which converges any drift that
     * accumulated while this admin node was down.
     */
    private final ConcurrentMap<String, String> digestCursor = new ConcurrentHashMap<>();

    public WebsocketDataReconciler(final WebsocketSyncProperties websocketSyncProperties,
                                   final ClusterProperties clusterProperties,
                                   final ObjectProvider<ClusterSelectMasterService> masterServiceProvider,
                                   final AppAuthService appAuthService,
                                   final NamespacePluginService namespacePluginService,
                                   final SelectorService selectorService,
                                   final RuleService ruleService,
                                   final MetaDataService metaDataService,
                                   final ProxySelectorService proxySelectorService,
                                   final DiscoveryUpstreamService discoveryUpstreamService,
                                   final AiProxyApiKeyService aiProxyApiKeyService) {
        this.executor = new ScheduledThreadPoolExecutor(1,
                ShenyuThreadFactory.create("websocket-reconciliation", true));
        this.websocketSyncProperties = websocketSyncProperties;
        this.clusterProperties = clusterProperties;
        this.masterServiceProvider = masterServiceProvider;
        this.appAuthService = appAuthService;
        this.namespacePluginService = namespacePluginService;
        this.selectorService = selectorService;
        this.ruleService = ruleService;
        this.metaDataService = metaDataService;
        this.proxySelectorService = proxySelectorService;
        this.discoveryUpstreamService = discoveryUpstreamService;
        this.aiProxyApiKeyService = aiProxyApiKeyService;
    }

    @Override
    public void afterPropertiesSet() {
        WebsocketSyncProperties.Reconciliation reconciliation = websocketSyncProperties.getReconciliation();
        if (!reconciliation.isEnabled()) {
            LOG.info("websocket data reconciliation is disabled");
            return;
        }
        long intervalMillis = reconciliation.getInterval().toMillis();
        // jitter the initial delay so several admin nodes sharing one database do not
        // poll it simultaneously; scheduleWithFixedDelay also prevents overlapping runs
        long jitterMillis = ThreadLocalRandom.current().nextLong(Math.max(1L, intervalMillis));
        executor.scheduleWithFixedDelay(this::reconcileSafely, intervalMillis + jitterMillis,
                intervalMillis, TimeUnit.MILLISECONDS);
        LOG.info("websocket data reconciliation started, interval: {}ms, initial jitter: {}ms",
                intervalMillis, jitterMillis);
    }

    @Override
    public void destroy() {
        executor.shutdownNow();
    }

    /**
     * Run one reconciliation cycle synchronously, visible for tests.
     */
    void reconcileSafely() {
        try {
            reconcile();
        } catch (Exception e) {
            LOG.error("websocket data reconciliation cycle failed", e);
        }
    }

    private void reconcile() {
        if (skipForClusterNonMaster()) {
            LOG.debug("websocket data reconciliation skipped: cluster mode enabled and this node is not the master");
            return;
        }
        Set<String> namespaceIds = activeNamespaceIds();
        for (String namespaceId : namespaceIds) {
            for (ConfigGroupEnum group : ConfigGroupEnum.values()) {
                reconcileGroup(namespaceId, group);
            }
        }
    }

    private boolean skipForClusterNonMaster() {
        if (!clusterProperties.isEnabled()) {
            return false;
        }
        ClusterSelectMasterService masterService = masterServiceProvider.getIfAvailable();
        return Objects.nonNull(masterService) && !masterService.isMaster();
    }

    /**
     * Snapshot the namespaces this node currently serves, visible for tests.
     *
     * @return the namespace ids with active websocket sessions
     */
    Set<String> activeNamespaceIds() {
        return WebsocketCollector.getActiveNamespaceIds();
    }

    private void reconcileGroup(final String namespaceId, final ConfigGroupEnum group) {
        String cursorKey = namespaceId + ":" + group.name();
        try {
            List<?> dataList = load(namespaceId, group);
            String digest = DigestUtils.md5Hex(dataList.stream()
                    .map(GsonUtils.getInstance()::toJson)
                    .sorted()
                    .collect(Collectors.joining("\n")));
            if (digest.equals(digestCursor.get(cursorKey))) {
                LOG.debug("websocket reconciliation group {} in namespace {} is unchanged, skip push",
                        group, namespaceId);
                return;
            }
            WebsocketData<?> websocketData =
                    new WebsocketData<>(group.name(), DataEventTypeEnum.REFRESH.name(), dataList);
            push(namespaceId, GsonUtils.getInstance().toJson(websocketData));
            digestCursor.put(cursorKey, digest);
            LOG.info("websocket reconciliation pushed group {} for namespace {}, size: {}",
                    group, namespaceId, dataList.size());
        } catch (Exception e) {
            // the cursor is not advanced, so the next cycle retries this group
            LOG.error("websocket reconciliation failed for group {} in namespace {}, cursor not advanced",
                    group, namespaceId, e);
        }
    }

    /**
     * Push one refresh message to the sessions of a namespace, visible for tests.
     *
     * @param namespaceId the namespace id
     * @param message the message
     */
    void push(final String namespaceId, final String message) {
        WebsocketCollector.send(namespaceId, message, DataEventTypeEnum.REFRESH);
    }

    private List<?> load(final String namespaceId, final ConfigGroupEnum group) {
        switch (group) {
            case APP_AUTH:
                return appAuthService.listAllByNamespaceId(namespaceId);
            case PLUGIN:
                return namespacePluginService.listAll(namespaceId);
            case RULE:
                return ruleService.listAllByNamespaceId(namespaceId);
            case SELECTOR:
                return selectorService.listAllByNamespaceId(namespaceId);
            case META_DATA:
                return metaDataService.listAllByNamespaceId(namespaceId);
            case PROXY_SELECTOR:
                return proxySelectorService.listAllByNamespaceId(namespaceId);
            case DISCOVER_UPSTREAM:
                return discoveryUpstreamService.listAllByNamespaceId(namespaceId);
            case AI_PROXY_API_KEY:
                return aiProxyApiKeyService.listAllByNamespaceId(namespaceId);
            default:
                return Collections.emptyList();
        }
    }

    /**
     * Expose the digest cursor for tests.
     *
     * @return the digest cursor
     */
    Map<String, String> getDigestCursor() {
        return digestCursor;
    }
}
