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

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Lists;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.enums.RunningModeEnum;
import org.apache.shenyu.common.timer.AbstractRoundTask;
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

import java.net.URI;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

/**
 * Websocket sync data service.
 */
public class WebsocketSyncDataService implements SyncDataService {
    
    /**
     * logger.
     */
    private static final Logger LOG = LoggerFactory.getLogger(WebsocketSyncDataService.class);
    
    /**
     * see https://github.com/apache/tomcat/blob/main/java/org/apache/tomcat/websocket/Constants.java#L99.
     */
    private static final String ORIGIN_HEADER_NAME = "Origin";
    
    private final WebsocketConfig websocketConfig;
    
    private final PluginDataSubscriber pluginDataSubscriber;
    
    private final List<MetaDataSubscriber> metaDataSubscribers;
    
    private final List<AuthDataSubscriber> authDataSubscribers;
    
    private final List<ProxySelectorDataSubscriber> proxySelectorDataSubscribers;
    
    private final List<DiscoveryUpstreamDataSubscriber> discoveryUpstreamDataSubscribers;
    
    private final List<ShenyuWebsocketClient> clients = Lists.newArrayList();
    
    private final String namespaceId;
    
    private final Timer timer;
    
    private TimerTask timerTask;
    
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
     */
    public WebsocketSyncDataService(final WebsocketConfig websocketConfig,
                                    final ShenyuConfig shenyuConfig,
                                    final PluginDataSubscriber pluginDataSubscriber,
                                    final List<MetaDataSubscriber> metaDataSubscribers,
                                    final List<AuthDataSubscriber> authDataSubscribers,
                                    final List<ProxySelectorDataSubscriber> proxySelectorDataSubscribers,
                                    final List<DiscoveryUpstreamDataSubscriber> discoveryUpstreamDataSubscribers
    ) {
        this.timer = WheelTimerFactory.getSharedTimer();
        this.websocketConfig = websocketConfig;
        this.pluginDataSubscriber = pluginDataSubscriber;
        this.metaDataSubscribers = metaDataSubscribers;
        this.authDataSubscribers = authDataSubscribers;
        this.proxySelectorDataSubscribers = proxySelectorDataSubscribers;
        this.discoveryUpstreamDataSubscribers = discoveryUpstreamDataSubscribers;
        this.namespaceId = shenyuConfig.getNamespace();
        LOG.info("start init connecting...");
        List<String> urls = websocketConfig.getUrls();
        for (String url : urls) {
            if (StringUtils.isNotEmpty(websocketConfig.getAllowOrigin())) {
                Map<String, String> headers = ImmutableMap.of(ORIGIN_HEADER_NAME, websocketConfig.getAllowOrigin());
                clients.add(new ShenyuWebsocketClient(URI.create(url),
                        headers,
                        Objects.requireNonNull(pluginDataSubscriber),
                        metaDataSubscribers,
                        authDataSubscribers,
                        proxySelectorDataSubscribers,
                        discoveryUpstreamDataSubscribers,
                        namespaceId));
            } else {
                clients.add(new ShenyuWebsocketClient(URI.create(url),
                        Objects.requireNonNull(pluginDataSubscriber),
                        metaDataSubscribers,
                        authDataSubscribers,
                        proxySelectorDataSubscribers,
                        discoveryUpstreamDataSubscribers,
                        namespaceId));
            }
        }
        LOG.info("start check task...");
        this.timer.add(timerTask = new AbstractRoundTask(null, TimeUnit.SECONDS.toMillis(60)) {
            @Override
            public void doRun(final String key, final TimerTask timerTask) {
                masterCheck();
            }
        });
    }
    
    private void masterCheck() {
        if (LOG.isDebugEnabled()) {
            LOG.debug("master checking task start...");
        }
        if (CollectionUtils.isEmpty(clients)) {
            List<String> urls = websocketConfig.getUrls();
            for (String url : urls) {
                if (StringUtils.isNotEmpty(websocketConfig.getAllowOrigin())) {
                    Map<String, String> headers = ImmutableMap.of(ORIGIN_HEADER_NAME, websocketConfig.getAllowOrigin());
                    clients.add(new ShenyuWebsocketClient(URI.create(url), headers, Objects.requireNonNull(pluginDataSubscriber), metaDataSubscribers,
                            authDataSubscribers, proxySelectorDataSubscribers, discoveryUpstreamDataSubscribers, namespaceId));
                } else {
                    clients.add(new ShenyuWebsocketClient(URI.create(url), Objects.requireNonNull(pluginDataSubscriber),
                            metaDataSubscribers, authDataSubscribers, proxySelectorDataSubscribers, discoveryUpstreamDataSubscribers, namespaceId));
                }
            }
        }
        Iterator<ShenyuWebsocketClient> iterator = clients.iterator();
        while (iterator.hasNext()) {
            ShenyuWebsocketClient websocketClient = iterator.next();
            if (!websocketClient.isOpen()) {
                iterator.remove();
                continue;
            }
            String runningMode = websocketClient.getRunningMode();
            // check running mode
            if (Objects.equals(runningMode, RunningModeEnum.STANDALONE.name())) {
                LOG.info("admin running in standalone mode...");
                timerTask.cancel();
                return;
            }
            
            if (!websocketClient.isConnectedToMaster()) {
                websocketClient.nowClose();
                iterator.remove();
            }
        }
    }
    
    @Override
    public void close() {
        if (CollectionUtils.isNotEmpty(clients)) {
            for (ShenyuWebsocketClient client : clients) {
                if (Objects.nonNull(client)) {
                    client.close();
                }
            }
        }
        if (Objects.nonNull(timerTask)) {
            timerTask.cancel();
        }
        timer.shutdown();
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
