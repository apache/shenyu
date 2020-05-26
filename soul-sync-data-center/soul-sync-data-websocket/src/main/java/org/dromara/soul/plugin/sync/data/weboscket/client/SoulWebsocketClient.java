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

package org.dromara.soul.plugin.sync.data.weboscket.client;

import lombok.extern.slf4j.Slf4j;
import org.dromara.soul.common.dto.AppAuthData;
import org.dromara.soul.common.dto.MetaData;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.dto.WebsocketData;
import org.dromara.soul.common.enums.ConfigGroupEnum;
import org.dromara.soul.common.enums.DataEventTypeEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.plugin.sync.data.weboscket.handler.WebsocketDataHandler;
import org.dromara.soul.sync.data.api.AuthDataSubscriber;
import org.dromara.soul.sync.data.api.MetaDataSubscriber;
import org.dromara.soul.sync.data.api.PluginDataSubscriber;
import org.java_websocket.client.WebSocketClient;
import org.java_websocket.handshake.ServerHandshake;

import java.net.URI;
import java.util.List;

@Slf4j
public class SoulWebsocketClient extends WebSocketClient {
    
    private volatile boolean alreadySync = Boolean.FALSE;
    
    private final WebsocketDataHandler websocketDataHandler;
    
    public SoulWebsocketClient(final URI serverUri, final List<PluginDataSubscriber> pluginDataSubscribers,
                               final List<MetaDataSubscriber> metaDataSubscribers, final List<AuthDataSubscriber> authDataSubscribers) {
        super(serverUri);
        this.websocketDataHandler = new WebsocketDataHandler(pluginDataSubscribers, metaDataSubscribers, authDataSubscribers);
    }
    
    @Override
    public void onOpen(ServerHandshake serverHandshake) {
        if (!alreadySync) {
            send(DataEventTypeEnum.MYSELF.name());
            alreadySync = true;
        }
    }
    
    @Override
    public void onMessage(String result) {
        try {
            handleResult(result);
        } catch (Exception e) {
            log.error("websocket handle data exception :", e);
        }
    }
    
    @Override
    public void onClose(int i, String s, boolean b) {
        this.close();
    }
    
    @Override
    public void onError(Exception e) {
        this.close();
    }
    
    @SuppressWarnings("ALL")
    private void handleResult(final String result) {
        WebsocketData websocketData = GsonUtils.getInstance().fromJson(result, WebsocketData.class);
        ConfigGroupEnum groupEnum = ConfigGroupEnum.acquireByName(websocketData.getGroupType());
        String eventType = websocketData.getEventType();
        switch (groupEnum) {
            case PLUGIN:
                String pluginData = GsonUtils.getInstance().toJson(websocketData.getData());
                List<PluginData> pluginDataList = GsonUtils.getInstance().fromList(pluginData, PluginData.class);
                websocketDataHandler.handlePlugin(pluginDataList, eventType);
                break;
            case SELECTOR:
                String selectorData = GsonUtils.getInstance().toJson(websocketData.getData());
                List<SelectorData> selectorDataList = GsonUtils.getInstance().fromList(selectorData, SelectorData.class);
                websocketDataHandler.handleSelector(selectorDataList, eventType);
                break;
            case RULE:
                String ruleData = GsonUtils.getInstance().toJson(websocketData.getData());
                List<RuleData> ruleDataList = GsonUtils.getInstance().fromList(ruleData, RuleData.class);
                websocketDataHandler.handleRule(ruleDataList, eventType);
                break;
            case APP_AUTH:
                String appAuthData = GsonUtils.getInstance().toJson(websocketData.getData());
                List<AppAuthData> appAuthDataList = GsonUtils.getInstance().fromList(appAuthData, AppAuthData.class);
                websocketDataHandler.handleAppAuth(appAuthDataList, eventType);
                break;
            case META_DATA:
                String metaData = GsonUtils.getInstance().toJson(websocketData.getData());
                List<MetaData> metaDataList = GsonUtils.getInstance().fromList(metaData, MetaData.class);
                websocketDataHandler.handleMetaData(metaDataList, eventType);
                break;
            default:
                break;
        }
    }
}
