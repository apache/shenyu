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

package org.dromara.soul.sync.data.http.handler;

import java.util.List;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.dromara.soul.common.dto.AppAuthData;
import org.dromara.soul.common.dto.MetaData;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.sync.data.api.AuthDataSubscriber;
import org.dromara.soul.sync.data.api.MetaDataSubscriber;
import org.dromara.soul.sync.data.api.PluginDataSubscriber;

/**
 * The type Http cache handler.
 *
 * @author xiaoyu(Myth)
 */
@Slf4j
public class HttpSyncDataHandler {
    
    private final PluginDataSubscriber pluginDataSubscriber;
    
    private final List<MetaDataSubscriber> metaDataSubscribers;
    
    private final List<AuthDataSubscriber> authDataSubscribers;
    
    /**
     * Instantiates a new Http sync data handler.
     *
     * @param metaDataSubscribers the meta data subscribers
     * @param authDataSubscribers the auth data subscribers
     */
    public HttpSyncDataHandler(final PluginDataSubscriber pluginDataSubscriber,
                               final List<MetaDataSubscriber> metaDataSubscribers,
                               final List<AuthDataSubscriber> authDataSubscribers) {
        this.pluginDataSubscriber = pluginDataSubscriber;
        this.metaDataSubscribers = metaDataSubscribers;
        this.authDataSubscribers = authDataSubscribers;
    }
    
    /**
     * Flush all plugin. If the collection is empty, the cache will be emptied.
     *
     * @param pluginDataList the plugin config
     */
    public void flushAllPlugin(final List<PluginData> pluginDataList) {
        if (CollectionUtils.isEmpty(pluginDataList)) {
            log.info("clear all plugin data cache");
            pluginDataSubscriber.refreshPluginData();
        } else {
            pluginDataSubscriber.refreshPluginData();
            pluginDataList.forEach(pluginDataSubscriber::onSubscribe);
        }
    }
    
    /**
     * Flush all selector.
     *
     * @param selectorDataList the selector config
     */
    public void flushAllSelector(final List<SelectorData> selectorDataList) {
        if (CollectionUtils.isEmpty(selectorDataList)) {
            log.info("clear all selector cache, old cache");
            selectorDataList.forEach(pluginDataSubscriber::unSelectorSubscribe);
            pluginDataSubscriber.refreshSelectorData();
        } else {
            // update cache for UpstreamCacheManager
            pluginDataSubscriber.refreshSelectorData();
            selectorDataList.forEach(pluginDataSubscriber::onSelectorSubscribe);
        }
    }
    
    /**
     * Flush all rule.
     *
     * @param ruleDataList the rule config
     */
    public void flushAllRule(final List<RuleData> ruleDataList) {
        if (CollectionUtils.isEmpty(ruleDataList)) {
            log.info("clear all rule cache");
            pluginDataSubscriber.refreshRuleData();
        } else {
            pluginDataSubscriber.refreshRuleData();
            ruleDataList.forEach(pluginDataSubscriber::onRuleSubscribe);
        }
    }
    
    /**
     * Flush all app auth.
     *
     * @param appAuthDataList the app auth config
     */
    public void flushAllAppAuth(final List<AppAuthData> appAuthDataList) {
        if (CollectionUtils.isEmpty(appAuthDataList)) {
            log.info("clear all appAuth data cache");
            authDataSubscribers.forEach(AuthDataSubscriber::refresh);
        } else {
            appAuthDataList.forEach(authData -> authDataSubscribers.forEach(subscriber -> subscriber.onSubscribe(authData)));
        }
    }
    
    /**
     * Flush meta data.
     *
     * @param metaDataList the meta data list
     */
    public void flushMetaData(final List<MetaData> metaDataList) {
        if (CollectionUtils.isEmpty(metaDataList)) {
            log.info("clear all metaData cache}");
            metaDataSubscribers.forEach(MetaDataSubscriber::refresh);
        } else {
            metaDataList.forEach(metaData -> metaDataSubscribers.forEach(subscriber -> subscriber.onSubscribe(metaData)));
        }
    }
}
