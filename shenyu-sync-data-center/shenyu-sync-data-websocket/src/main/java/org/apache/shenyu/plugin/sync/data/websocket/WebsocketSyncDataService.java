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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.plugin.sync.data.websocket.client.ShenyuWebsocketClient;
import org.apache.shenyu.plugin.sync.data.websocket.config.WebsocketConfig;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.api.SyncDataService;
import org.java_websocket.client.WebSocketClient;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * Websocket sync data service.
 */
public class WebsocketSyncDataService implements SyncDataService, AutoCloseable {

    /**
     * logger. 
     */
    private static final Logger LOG = LoggerFactory.getLogger(WebsocketSyncDataService.class);

    private final List<WebSocketClient> clients = new ArrayList<>();

    private final ScheduledThreadPoolExecutor executor;

    /**
     * Instantiates a new Websocket sync cache.
     *
     * @param websocketConfig      the websocket config
     * @param pluginDataSubscriber the plugin data subscriber
     * @param metaDataSubscribers  the meta data subscribers
     * @param authDataSubscribers  the auth data subscribers
     */
    public WebsocketSyncDataService(final WebsocketConfig websocketConfig,
                                    final PluginDataSubscriber pluginDataSubscriber,
                                    final List<MetaDataSubscriber> metaDataSubscribers,
                                    final List<AuthDataSubscriber> authDataSubscribers) {
        String[] urls = StringUtils.split(websocketConfig.getUrls(), ",");
        executor = new ScheduledThreadPoolExecutor(urls.length, ShenyuThreadFactory.create("websocket-connect", true));
        for (String url : urls) {
            try {
                clients.add(new ShenyuWebsocketClient(new URI(url), Objects.requireNonNull(pluginDataSubscriber), metaDataSubscribers, authDataSubscribers));
            } catch (URISyntaxException e) {
                LOG.error("websocket url({}) is error", url, e);
            }
        }
        try {
            for (WebSocketClient client : clients) {
                boolean success = client.connectBlocking(3000, TimeUnit.MILLISECONDS);
                if (success) {
                    LOG.info("websocket connection is successful.....");
                } else {
                    LOG.error("websocket connection is error.....");
                }
                executor.scheduleAtFixedRate(() -> {
                    try {
                        if (client.isClosed()) {
                            boolean reconnectSuccess = client.reconnectBlocking();
                            if (reconnectSuccess) {
                                LOG.info("websocket reconnect server[{}] is successful.....", client.getURI().toString());
                            } else {
                                LOG.error("websocket reconnection server[{}] is error.....", client.getURI().toString());
                            }
                        } else {
                            client.sendPing();
                            LOG.debug("websocket send to [{}] ping message successful", client.getURI().toString());
                        }
                    } catch (InterruptedException e) {
                        LOG.error("websocket connect is error :{}", e.getMessage());
                    }
                }, 10, 10, TimeUnit.SECONDS);
            }
            /* client.setProxy(new Proxy(Proxy.Type.HTTP, new InetSocketAddress("proxyaddress", 80)));*/
        } catch (InterruptedException e) {
            LOG.info("websocket connection...exception....", e);
        }

    }

    @Override
    public void close() {
        for (WebSocketClient client : clients) {
            if (!client.isClosed()) {
                client.close();
            }
        }
        if (Objects.nonNull(executor)) {
            executor.shutdown();
        }
    }
}
