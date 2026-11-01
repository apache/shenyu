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

package org.apache.shenyu.plugin.sync.data.websocket;

import com.google.common.collect.Lists;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.utils.BoundServerPort;
import org.apache.shenyu.common.enums.RunningModeEnum;
import org.apache.shenyu.common.timer.AbstractRoundTask;
import org.apache.shenyu.common.timer.TaskEntity;
import org.apache.shenyu.common.timer.Timer;
import org.apache.shenyu.common.timer.TimerTask;
import org.apache.shenyu.common.timer.WheelTimerFactory;
import org.apache.shenyu.plugin.sync.data.websocket.client.ShenyuWebsocketClient;
import org.apache.shenyu.plugin.sync.data.websocket.config.WebsocketConfig;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.DiscoveryUpstreamDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.api.ProxySelectorDataSubscriber;
import org.apache.shenyu.sync.data.api.SyncDataService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.web.context.WebServerInitializedEvent;
import org.springframework.context.event.EventListener;

import java.net.URI;
import java.util.OptionalInt;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.concurrent.atomic.AtomicBoolean;

/** Websocket sync data service. */
public class WebsocketSyncDataService implements SyncDataService {
    
    /** logger. */
    private static final Logger LOG = LoggerFactory.getLogger(WebsocketSyncDataService.class);
    
    /**
     * see
     * https://github.com/apache/tomcat/blob/main/java/org/apache/tomcat/websocket/Constants.java#L99.
     */
    private static final String ORIGIN_HEADER_NAME = "Origin";
    
    private final WebsocketConfig websocketConfig;

    private final AtomicBoolean initialSyncReady = new AtomicBoolean();

    private final AtomicBoolean rediscoveryScheduled = new AtomicBoolean(false);

    
    private final PluginDataSubscriber pluginDataSubscriber;
    
    private final List<MetaDataSubscriber> metaDataSubscribers;
    
    private final List<AuthDataSubscriber> authDataSubscribers;
    
    private final List<ProxySelectorDataSubscriber> proxySelectorDataSubscribers;
    
    private final List<DiscoveryUpstreamDataSubscriber> discoveryUpstreamDataSubscribers;
    
    private final List<org.apache.shenyu.sync.data.api.AiProxyApiKeyDataSubscriber> aiProxyApiKeyDataSubscribers;

    private final List<ShenyuWebsocketClient> clients = Lists.newArrayList();
    
    private final String namespaceId;
    
    private final Timer timer;
    
    private TimerTask timerTask;

    private boolean closed;

    /**
     * Port of the application listener. Zero until the server initialization event.
     */
    private volatile int boundPort;

    /**
     * Instantiates a new Websocket sync cache.
     *
     * @param websocketConfig the websocket config
     * @param shenyuConfig the shenyu config
     * @param pluginDataSubscriber the plugin data subscriber
     * @param metaDataSubscribers the meta data subscribers
     * @param authDataSubscribers the auth data subscribers
     * @param proxySelectorDataSubscribers the proxy selector data subscribers
     * @param discoveryUpstreamDataSubscribers the discovery upstream data subscribers
     * @param aiProxyApiKeyDataSubscribers the ai proxy api key subscribers
     */
    public WebsocketSyncDataService(
            final WebsocketConfig websocketConfig,
                                    final ShenyuConfig shenyuConfig,
                                    final PluginDataSubscriber pluginDataSubscriber,
                                    final List<MetaDataSubscriber> metaDataSubscribers,
                                    final List<AuthDataSubscriber> authDataSubscribers,
                                    final List<ProxySelectorDataSubscriber> proxySelectorDataSubscribers,
            final List<DiscoveryUpstreamDataSubscriber> discoveryUpstreamDataSubscribers,
            final List<org.apache.shenyu.sync.data.api.AiProxyApiKeyDataSubscriber>
                    aiProxyApiKeyDataSubscribers) {
        this.timer = WheelTimerFactory.newWheelTimer();
        this.websocketConfig = websocketConfig;
        this.pluginDataSubscriber = pluginDataSubscriber;
        this.metaDataSubscribers = metaDataSubscribers;
        this.authDataSubscribers = authDataSubscribers;
        this.proxySelectorDataSubscribers = proxySelectorDataSubscribers;
        this.discoveryUpstreamDataSubscribers = discoveryUpstreamDataSubscribers;
        this.aiProxyApiKeyDataSubscribers = aiProxyApiKeyDataSubscribers;
        this.namespaceId = shenyuConfig.getNamespace();
        LOG.info("start check task...");
        long intervalSeconds = websocketConfig.getMasterCheckInterval() <= 0 ? 60 : websocketConfig.getMasterCheckInterval();
        this.timer.add(timerTask = new AbstractRoundTask(null, TimeUnit.SECONDS.toMillis(intervalSeconds)) {
            @Override
            public void doRun(final String key, final TimerTask timerTask) {
                masterCheck();
            }
        });
    }

    private synchronized void masterCheck() {
        if (closed) {
            return;
        }
        if (LOG.isDebugEnabled()) {
            LOG.debug("master checking task start...");
        }
        if (CollectionUtils.isEmpty(clients)) {
            addSeedClients();
        }
        Set<URI> masterUrls = new LinkedHashSet<>();
        Iterator<ShenyuWebsocketClient> iterator = clients.iterator();
        while (iterator.hasNext()) {
            ShenyuWebsocketClient websocketClient = iterator.next();
            if (!websocketClient.isOpen()) {
                websocketClient.nowClose();
                iterator.remove();
                continue;
            }
            String runningMode = websocketClient.getRunningMode();
            // check running mode
            if (Objects.equals(runningMode, RunningModeEnum.STANDALONE.name())) {
                LOG.info("admin running in standalone mode...");
                if (Objects.nonNull(timerTask)) {
                    timerTask.cancel();
                }
                return;
            }
            
            // An open socket may still be waiting for its running-mode reply.
            if (Objects.equals(runningMode, RunningModeEnum.CLUSTER.name()) && !websocketClient.isConnectedToMaster()) {
                URI masterUri = parseMasterUri(websocketClient.getMasterUrl());
                URI currentUri = websocketClient.getURI();
                if (Objects.nonNull(masterUri) && !masterUri.equals(currentUri)) {
                    if (!isTrustedMaster(masterUri, currentUri)) {
                        // keep the trusted follower connection instead of reconnecting to an
                        // advertised destination that must not receive the sync credential
                        LOG.warn("Ignore untrusted websocket master [{}] advertised by [{}]",
                                describeOrigin(masterUri), describeOrigin(currentUri));
                        continue;
                    }
                    masterUrls.add(masterUri);
                }
                websocketClient.nowClose();
                iterator.remove();
            }
        }
        for (URI masterUri : masterUrls) {
            if (boundPort > 0 && clients.stream().noneMatch(client -> masterUri.equals(client.getURI()))) {
                try {
                    clients.add(createClient(masterUri.toString()));
                } catch (RuntimeException ex) {
                    LOG.warn("websocket connection to advertised master [{}] failed", masterUri, ex);
                }
            }
        }
        if (CollectionUtils.isEmpty(clients)) {
            addSeedClients();
        }
    }

    /**
     * Connect using the application listener port. A management server is not the gateway listener,
     * and clients stay disconnected until a positive bound port is known. Closing the service first
     * discards a later initialization event.
     *
     * @param event web server initialization event
     */
    @EventListener
    public synchronized void onWebServerInitialized(final WebServerInitializedEvent event) {
        if (closed || Objects.isNull(event.getWebServer()) || Objects.isNull(event.getApplicationContext())) {
            return;
        }
        OptionalInt resolved = BoundServerPort.resolve(event.getApplicationContext().getServerNamespace(), event.getWebServer().getPort());
        if (resolved.isEmpty()) {
            LOG.debug("Ignore websocket client port from namespace {} port {}", event.getApplicationContext().getServerNamespace(), event.getWebServer().getPort());
            return;
        }
        int observed = resolved.getAsInt();
        if (boundPort == observed) {
            return;
        }
        boundPort = observed;
        LOG.info("Application server bound on port {}, websocket sync will report it", observed);
        if (CollectionUtils.isEmpty(clients)) {
            addSeedClients();
        }
    }

    private void addSeedClients() {
        if (boundPort <= 0) {
            return;
        }
        List<String> urls = websocketConfig.getUrls();
        if (CollectionUtils.isEmpty(urls)) {
            return;
        }
        LOG.info("start init connecting on port {}...", boundPort);
        for (String url : urls) {
            clients.add(createClient(url));
        }
    }

    private boolean isTrustedMaster(final URI advertised, final URI current) {
        final List<URI> connected = clients.stream()
                .map(ShenyuWebsocketClient::getURI)
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
        return MasterTargetPolicy.isTrustedMaster(advertised, current, connected, websocketConfig.getUrls(),
                websocketConfig.getTrustedMasterHosts(), websocketConfig.isAllowInsecureMasterRedirect());
    }

    private String describeOrigin(final URI uri) {
        if (Objects.isNull(uri)) {
            return "null";
        }
        return uri.getScheme() + "://" + uri.getHost() + ":" + uri.getPort();
    }

    private URI parseMasterUri(final String masterUrl) {
        if (StringUtils.isBlank(masterUrl)) {
            return null;
        }
        try {
            URI uri = URI.create(masterUrl);
            if (("ws".equalsIgnoreCase(uri.getScheme()) || "wss".equalsIgnoreCase(uri.getScheme())) && StringUtils.isNotBlank(uri.getHost())) {
                return uri;
            }
        } catch (IllegalArgumentException ex) {
            LOG.warn("Ignore invalid websocket master address [{}]", masterUrl, ex);
            return null;
        }
        LOG.warn("Ignore invalid websocket master address [{}]", masterUrl);
        return null;
    }

    private void onMasterLost(final ShenyuWebsocketClient client) {
        if (closed) {
            return;
        }
        LOG.info("master lost detected from websocket server[{}], scheduling rediscovery", client.getURI());
        triggerMasterRediscovery();
    }

    private void triggerMasterRediscovery() {
        if (closed) {
            return;
        }
        if (rediscoveryScheduled.compareAndSet(false, true)) {
            try {
                this.timer.add(new TimerTask(200, TimeUnit.MILLISECONDS) {
                    @Override
                    public void run(final TaskEntity taskEntity) {
                        rediscoveryScheduled.set(false);
                        masterCheck();
                    }
                });
            } catch (Exception ex) {
                rediscoveryScheduled.set(false);
                LOG.warn("Failed to schedule master rediscovery task", ex);
            }
        }
    }
    
    @Override
    public synchronized void close() {
        if (closed) {
            return;
        }
        closed = true;
        try {
            if (Objects.nonNull(timerTask)) {
                timerTask.cancel();
            }
            if (CollectionUtils.isNotEmpty(clients)) {
                for (ShenyuWebsocketClient client : clients) {
                    if (Objects.nonNull(client)) {
                        client.nowClose();
                    }
                }
            }
        } finally {
            timer.shutdown();
        }
    }

    private ShenyuWebsocketClient createClient(final String url) {
        Map<String, String> headers = new HashMap<>();
        if (StringUtils.isNotEmpty(websocketConfig.getAllowOrigin())) {
            headers.put(ORIGIN_HEADER_NAME, websocketConfig.getAllowOrigin());
        }
        if (StringUtils.isNotBlank(websocketConfig.getToken())) {
            headers.put(Constants.X_SHENYU_SYNC_TOKEN, websocketConfig.getToken());
        }
        if (websocketConfig.isCompactDiscoveryPayload()) {
            headers.put(Constants.X_SHENYU_DISCOVERY_PAYLOAD, Constants.COMPACT_DISCOVERY_PAYLOAD_V1);
        }
        return new ShenyuWebsocketClient(
                URI.create(url),
                headers,
                Objects.requireNonNull(pluginDataSubscriber),
                metaDataSubscribers,
                authDataSubscribers,
                proxySelectorDataSubscribers,
                discoveryUpstreamDataSubscribers,
                this.aiProxyApiKeyDataSubscribers,
                namespaceId,
                boundPort,
                websocketConfig.isInitialSyncReadiness() ? initialSyncReady : null,
                this::onMasterLost);
    }
    
    /**
     * get websocket config.
     *
     * @return websocket config
     */
    public WebsocketConfig getWebsocketConfig() {
        return websocketConfig;
    }

    /**
     * Whether initial configuration callbacks have completed successfully.
     * @return startup readiness
     */
    public boolean isInitialSyncReady() {
        return initialSyncReady.get();
    }
    
    /**
     * get plugin data subscriber.
     *
     * @return plugin data subscriber
     */
    public PluginDataSubscriber getPluginDataSubscriber() {
        return pluginDataSubscriber;
    }
    
    /**
     * get meta data subscriber.
     *
     * @return meta data subscriber
     */
    public List<MetaDataSubscriber> getMetaDataSubscribers() {
        return metaDataSubscribers;
    }
    
    /**
     * get auth data subscriber.
     *
     * @return auth data subscriber
     */
    public List<AuthDataSubscriber> getAuthDataSubscribers() {
        return authDataSubscribers;
    }
    
    /**
     * get proxy selector data subscriber.
     *
     * @return proxy selector data subscriber
     */
    public List<ProxySelectorDataSubscriber> getProxySelectorDataSubscribers() {
        return proxySelectorDataSubscribers;
    }
    
    /**
     * get discovery upstream data subscriber.
     *
     * @return discovery upstream data subscriber
     */
    public List<DiscoveryUpstreamDataSubscriber> getDiscoveryUpstreamDataSubscribers() {
        return discoveryUpstreamDataSubscribers;
    }
}
