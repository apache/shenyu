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

import org.apache.commons.collections4.CollectionUtils;
import org.dromara.soul.common.dto.*;
import org.dromara.soul.common.enums.DataEventTypeEnum;
import org.dromara.soul.sync.data.api.AuthDataSubscriber;
import org.dromara.soul.sync.data.api.MetaDataSubscriber;
import org.dromara.soul.sync.data.api.PluginDataSubscriber;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * The type Websocket cache handler.
 *
 * @author xiaoyu(Myth)
 */
public class WebsocketDataHandler {
    
    private final Map<String, PluginDataSubscriber> pluginDataSubscriberMap;
    
    private final List<MetaDataSubscriber> metaDataSubscribers;
    
    private final List<AuthDataSubscriber> authDataSubscribers;
    
    public WebsocketDataHandler(final List<PluginDataSubscriber> pluginDataSubscribers,
                                final List<MetaDataSubscriber> metaDataSubscribers,
                                final List<AuthDataSubscriber> authDataSubscribers) {
        this.pluginDataSubscriberMap = pluginDataSubscribers.stream().collect(Collectors.toMap(PluginDataSubscriber::pluginNamed, e -> e));
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
            for (PluginData pluginData : pluginDataList) {
                switch (eventTypeEnum) {
                    case REFRESH:
                    case MYSELF:
                    case UPDATE:
                    case CREATE:
                        Optional.ofNullable(pluginDataSubscriberMap.get(pluginData.getName())).ifPresent(e -> e.onSubscribe(pluginData));
                        break;
                    case DELETE:
                        Optional.ofNullable(pluginDataSubscriberMap.get(pluginData.getName())).ifPresent(e -> e.unSubscribe(pluginData));
                        break;
                    default:
                        break;
                }
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
            for (SelectorData selectorData : selectorDataList) {
                switch (eventTypeEnum) {
                    case REFRESH:
                    case MYSELF:
                    case UPDATE:
                    case CREATE:
                        Optional.ofNullable(pluginDataSubscriberMap.get(selectorData.getPluginName())).ifPresent(e -> e.onSelectorSubscribe(selectorData));
                        break;
                    case DELETE:
                        Optional.ofNullable(pluginDataSubscriberMap.get(selectorData.getPluginName())).ifPresent(e -> e.unSelectorSubscribe(selectorData));
                        break;
                    default:
                        break;
                }
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
            for (RuleData ruleData : ruleDataList) {
                switch (eventTypeEnum) {
                    case REFRESH:
                    case MYSELF:
                    case UPDATE:
                    case CREATE:
                        Optional.ofNullable(pluginDataSubscriberMap.get(ruleData.getPluginName())).ifPresent(e -> e.onRuleSubscribe(ruleData));
                        break;
                    case DELETE:
                        Optional.ofNullable(pluginDataSubscriberMap.get(ruleData.getPluginName())).ifPresent(e -> e.unRuleSubscribe(ruleData));
                        break;
                    default:
                        break;
                }
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
            for (AppAuthData appAuthData : appAuthDataList) {
                switch (eventTypeEnum) {
                    case REFRESH:
                    case MYSELF:
                    case UPDATE:
                    case CREATE:
                        authDataSubscribers.forEach(authDataSubscriber -> authDataSubscriber.onSubscribe(appAuthData));
                        break;
                    case DELETE:
                        authDataSubscribers.forEach(authDataSubscriber -> authDataSubscriber.unSubscribe(appAuthData));
                        break;
                    default:
                        break;
                }
            }
        }
    }
    
    public void handleMetaData(final List<MetaData> metaDataList, final String eventType) {
        if (CollectionUtils.isNotEmpty(metaDataList)) {
            DataEventTypeEnum eventTypeEnum = DataEventTypeEnum.acquireByName(eventType);
            for (MetaData metaData : metaDataList) {
                switch (eventTypeEnum) {
                    case REFRESH:
                    case MYSELF:
                    case UPDATE:
                    case CREATE:
                        metaDataSubscribers.forEach(metaDataSubscriber -> metaDataSubscriber.onSubscribe(metaData));
                        break;
                    case DELETE:
                        metaDataSubscribers.forEach(metaDataSubscriber -> metaDataSubscriber.unSubscribe(metaData));
                        break;
                    default:
                        break;
                }
            }
        }
    }
    
}
