/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * Contributor license agreements.See the NOTICE file distributed with
 * This work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * he License.You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.dromara.soul.web.cache;

import org.dromara.soul.common.concurrent.SoulThreadFactory;
import org.dromara.soul.common.dto.AppAuthData;
import org.dromara.soul.common.dto.MetaData;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.dto.WebsocketData;
import org.dromara.soul.common.enums.ConfigGroupEnum;
import org.dromara.soul.common.enums.DataEventTypeEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.web.config.SoulConfig;
import org.java_websocket.client.WebSocketClient;
import org.java_websocket.handshake.ServerHandshake;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * The type Websocket sync cache.
 *
 * @author xiaoyu(Myth)
 */
public class WebsocketSyncCache extends WebsocketCacheHandler {

    private static final Logger LOGGER = LoggerFactory.getLogger(WebsocketSyncCache.class);

    /**
     * The Client.
     */
    private WebSocketClient client;

    private volatile boolean alreadySync = Boolean.FALSE;

    /**
     * Instantiates a new Websocket sync cache.
     *
     * @param websocketConfig the websocket config
     */
    public WebsocketSyncCache(final SoulConfig.WebsocketConfig websocketConfig) {
        ScheduledThreadPoolExecutor executor = new ScheduledThreadPoolExecutor(1,
                SoulThreadFactory.create("websocket-connect", true));
        try {
            client = new WebSocketClient(new URI(websocketConfig.getUrl())) {
                @Override
                public void onOpen(final ServerHandshake serverHandshake) {
                    if (!alreadySync) {
                        client.send(DataEventTypeEnum.MYSELF.name());
                        alreadySync = true;
                    }
                }

                @Override
                public void onMessage(final String result) {
                    try {
                        handleResult(result);
                    } catch (Exception e) {
                        LOGGER.error("websocket handle data exception :", e);
                    }
                }

                @Override
                public void onClose(final int code, final String msg, final boolean b) {
                    client.close();
                }

                @Override
                public void onError(final Exception e) {
                    client.close();
                }
            };
        } catch (URISyntaxException e) {
            LOGGER.error("websocket url is error :", e);
        }
        try {
            boolean success = client.connectBlocking();
            if (success) {
                LOGGER.info("websocket connection is successful.....");
            } else {
                LOGGER.info("websocket connection is error.....");
            }
        } catch (InterruptedException e) {
            LOGGER.info("websocket connection...exception....", e);
        }
        executor.scheduleAtFixedRate(() -> {
            try {
                if (client != null && client.isClosed()) {
                    boolean success = client.reconnectBlocking();
                    if (success) {
                        LOGGER.info("websocket reconnect is successful.....");
                    } else {
                        LOGGER.info("websocket reconnection is error.....");
                    }
                }
            } catch (InterruptedException e) {
                LOGGER.error("websocket connect is error :{}", e.getMessage());
            }

        }, 10, 30, TimeUnit.SECONDS);
    }

    private void handleResult(final String result) {
        WebsocketData websocketData = GsonUtils.getInstance().fromJson(result, WebsocketData.class);
        ConfigGroupEnum groupEnum = ConfigGroupEnum.acquireByName(websocketData.getGroupType());
        String eventType = websocketData.getEventType();
        switch (groupEnum) {
            case PLUGIN:
                String pluginData = GsonUtils.getInstance().toJson(websocketData.getData());
                List<PluginData> pluginDataList =
                        GsonUtils.getInstance().fromList(pluginData, PluginData.class);
                handlePlugin(pluginDataList, eventType);
                break;
            case SELECTOR:
                String selectorData = GsonUtils.getInstance().toJson(websocketData.getData());
                List<SelectorData> selectorDataList =
                        GsonUtils.getInstance().fromList(selectorData, SelectorData.class);
                handleSelector(selectorDataList, eventType);
                break;
            case RULE:
                String ruleData = GsonUtils.getInstance().toJson(websocketData.getData());
                List<RuleData> ruleDataList =
                        GsonUtils.getInstance().fromList(ruleData, RuleData.class);
                handleRule(ruleDataList, eventType);
                break;
            case APP_AUTH:
                String appAuthData = GsonUtils.getInstance().toJson(websocketData.getData());
                List<AppAuthData> appAuthDataList =
                        GsonUtils.getInstance().fromList(appAuthData, AppAuthData.class);
                handleAppAuth(appAuthDataList, eventType);
                break;
            case META_DATA:
                String metaData = GsonUtils.getInstance().toJson(websocketData.getData());
                List<MetaData> metaDataList =
                        GsonUtils.getInstance().fromList(metaData, MetaData.class);
                handleMetaData(metaDataList, eventType);
                break;
            default:
                break;
        }
    }
}
