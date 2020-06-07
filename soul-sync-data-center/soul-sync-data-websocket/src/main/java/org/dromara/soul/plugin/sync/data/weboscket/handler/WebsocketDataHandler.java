/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 */

package org.dromara.soul.plugin.sync.data.weboscket.handler;

import java.util.List;
import org.apache.commons.collections4.CollectionUtils;
import org.dromara.soul.common.dto.AppAuthData;
import org.dromara.soul.common.dto.MetaData;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.enums.DataEventTypeEnum;
import org.dromara.soul.sync.data.api.AuthDataSubscriber;
import org.dromara.soul.sync.data.api.MetaDataSubscriber;
import org.dromara.soul.sync.data.api.PluginDataSubscriber;

/**
 * The type Websocket cache handler.
 *
 * @author xiaoyu(Myth)
 */
public class WebsocketDataHandler {
    
    private final PluginDataSubscriber pluginDataSubscriber;
    
    private final List<MetaDataSubscriber> metaDataSubscribers;
    
    private final List<AuthDataSubscriber> authDataSubscribers;
    
    /**
     * Instantiates a new Websocket data handler.
     *
     * @param pluginDataSubscriber the plugin data subscriber
     * @param metaDataSubscribers   the meta data subscribers
     * @param authDataSubscribers   the auth data subscribers
     */
    public WebsocketDataHandler(final PluginDataSubscriber pluginDataSubscriber,
                                final List<MetaDataSubscriber> metaDataSubscribers,
                                final List<AuthDataSubscriber> authDataSubscribers) {
        this.pluginDataSubscriber = pluginDataSubscriber;
        this.metaDataSubscribers = metaDataSubscribers;
        this.authDataSubscribers = authDataSubscribers;
    }
    
    /**
     * Handle plugin.
     *
     * @param pluginDataList the plugin data list
     * @param eventType      the event type
     */
    public void handlePlugin(final List<PluginData> pluginDataList, final String eventType) {
        if (CollectionUtils.isNotEmpty(pluginDataList)) {
            DataEventTypeEnum eventTypeEnum = DataEventTypeEnum.acquireByName(eventType);
            switch (eventTypeEnum) {
                case REFRESH:
                case MYSELF:
                    pluginDataSubscriber.refreshPluginData();
                    pluginDataList.forEach(pluginDataSubscriber::onSubscribe);
                    break;
                case UPDATE:
                case CREATE:
                    pluginDataList.forEach(pluginDataSubscriber::onSubscribe);
                    break;
                case DELETE:
                    pluginDataList.forEach(pluginDataSubscriber::unSubscribe);
                    break;
                default:
                    break;
            }
    
        }
    }
    
    /**
     * Handle selector.
     *
     * @param selectorDataList the selector data list
     * @param eventType        the event type
     */
    public void handleSelector(final List<SelectorData> selectorDataList, final String eventType) {
        if (CollectionUtils.isNotEmpty(selectorDataList)) {
            DataEventTypeEnum eventTypeEnum = DataEventTypeEnum.acquireByName(eventType);
            switch (eventTypeEnum) {
                case REFRESH:
                case MYSELF:
                    pluginDataSubscriber.refreshSelectorData();
                    selectorDataList.forEach(pluginDataSubscriber::onSelectorSubscribe);
                    break;
                case UPDATE:
                case CREATE:
                    selectorDataList.forEach(pluginDataSubscriber::onSelectorSubscribe);
                    break;
                case DELETE:
                    selectorDataList.forEach(pluginDataSubscriber::unSelectorSubscribe);
                    break;
                default:
                    break;
            }
        }
    }
    
    /**
     * Handle rule.
     *
     * @param ruleDataList the rule data list
     * @param eventType    the event type
     */
    public void handleRule(final List<RuleData> ruleDataList, final String eventType) {
        if (CollectionUtils.isNotEmpty(ruleDataList)) {
            DataEventTypeEnum eventTypeEnum = DataEventTypeEnum.acquireByName(eventType);
            switch (eventTypeEnum) {
                case REFRESH:
                case MYSELF:
                    pluginDataSubscriber.refreshRuleData();
                    ruleDataList.forEach(pluginDataSubscriber::onRuleSubscribe);
                    break;
                case UPDATE:
                case CREATE:
                    ruleDataList.forEach(pluginDataSubscriber::onRuleSubscribe);
                    break;
                case DELETE:
                    ruleDataList.forEach(pluginDataSubscriber::unRuleSubscribe);
                    break;
                default:
                    break;
            }
        }
    }
    
    /**
     * Handle app auth.
     *
     * @param appAuthDataList the app auth data list
     * @param eventType       the event type
     */
    public void handleAppAuth(final List<AppAuthData> appAuthDataList, final String eventType) {
        if (CollectionUtils.isNotEmpty(appAuthDataList)) {
            DataEventTypeEnum eventTypeEnum = DataEventTypeEnum.acquireByName(eventType);
            switch (eventTypeEnum) {
                case REFRESH:
                case MYSELF:
                    authDataSubscribers.forEach(AuthDataSubscriber::refresh);
                    appAuthDataList.forEach(appAuthData -> authDataSubscribers.forEach(authDataSubscriber -> authDataSubscriber.onSubscribe(appAuthData)));
                    break;
                case UPDATE:
                case CREATE:
                    appAuthDataList.forEach(appAuthData -> authDataSubscribers.forEach(authDataSubscriber -> authDataSubscriber.onSubscribe(appAuthData)));
                    break;
                case DELETE:
                    appAuthDataList.forEach(appAuthData -> authDataSubscribers.forEach(authDataSubscriber -> authDataSubscriber.unSubscribe(appAuthData)));
                    break;
                default:
                    break;
            }
        }
    }
    
    /**
     * Handle meta data.
     *
     * @param metaDataList the meta data list
     * @param eventType    the event type
     */
    public void handleMetaData(final List<MetaData> metaDataList, final String eventType) {
        if (CollectionUtils.isNotEmpty(metaDataList)) {
            DataEventTypeEnum eventTypeEnum = DataEventTypeEnum.acquireByName(eventType);
            switch (eventTypeEnum) {
                case REFRESH:
                case MYSELF:
                    metaDataSubscribers.forEach(MetaDataSubscriber::refresh);
                    metaDataList.forEach(metaData -> metaDataSubscribers.forEach(metaDataSubscriber -> metaDataSubscriber.onSubscribe(metaData)));
                    break;
                case UPDATE:
                case CREATE:
                    metaDataList.forEach(metaData -> metaDataSubscribers.forEach(metaDataSubscriber -> metaDataSubscriber.onSubscribe(metaData)));
                    break;
                case DELETE:
                    metaDataList.forEach(metaData -> metaDataSubscribers.forEach(metaDataSubscriber -> metaDataSubscriber.unSubscribe(metaData)));
                    break;
                default:
                    break;
            }
        }
    }
    
}
