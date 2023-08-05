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

package org.apache.shenyu.sync.data.api;

import org.apache.shenyu.common.constant.ListDataNodePathConstants;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

public abstract class AbstarctListDataNodeSyncDataService implements SyncDataService {

    private static final Logger LOG = LoggerFactory.getLogger(AbstarctListDataNodeSyncDataService.class);

    private final PluginDataSubscriber pluginDataSubscriber;

    private final List<MetaDataSubscriber> metaDataSubscribers;

    private final List<AuthDataSubscriber> authDataSubscribers;

    private final List<ProxySelectorDataSubscriber> proxySelectorDataSubscribers;

    private final List<DiscoveryUpstreamDataSubscriber> discoveryUpstreamDataSubscribers;

    public AbstarctListDataNodeSyncDataService(final PluginDataSubscriber pluginDataSubscriber,
                                               final List<MetaDataSubscriber> metaDataSubscribers,
                                               final List<AuthDataSubscriber> authDataSubscribers,
                                               final List<ProxySelectorDataSubscriber> proxySelectorDataSubscribers,
                                               final List<DiscoveryUpstreamDataSubscriber> discoveryUpstreamDataSubscribers) {
        this.pluginDataSubscriber = pluginDataSubscriber;
        this.metaDataSubscribers = metaDataSubscribers;
        this.authDataSubscribers = authDataSubscribers;
        this.proxySelectorDataSubscribers = proxySelectorDataSubscribers;
        this.discoveryUpstreamDataSubscribers = discoveryUpstreamDataSubscribers;
    }

    /**
     * sub all data.
     */
    public void subAllData() {
        subscriberAuthData(readData(ListDataNodePathConstants.AUTH_DATA_ID));
        subscriberMetaData(readData(ListDataNodePathConstants.META_DATA_ID));
        subscriberPluginData(readData(ListDataNodePathConstants.PLUGIN_DATA_ID));
        subscriberRuleData(readData(ListDataNodePathConstants.RULE_DATA_ID));
        subscriberSelectorData(readData(ListDataNodePathConstants.SELECTOR_DATA_ID));
        subscriberProxySelectorData(readData(ListDataNodePathConstants.PROXY_SELECTOR_DATA_ID));
        subscriberDiscoverySyncData(readData(ListDataNodePathConstants.DISCOVERY_DATA_ID));
    }

    /**
     * watch plugin data.
     * @param configInfo config info
     * @return plugin data list
     */
    public List<PluginData> subscriberPluginData(final String configInfo) {
        List<PluginData> pluginDataList = new ArrayList<>(GsonUtils.getInstance().toObjectMap(configInfo, PluginData.class).values());
        pluginDataList.forEach(pluginData -> Optional.ofNullable(pluginDataSubscriber).ifPresent(subscriber -> {
            subscriber.unSubscribe(pluginData);
            subscriber.onSubscribe(pluginData);
        }));
        return pluginDataList;
    }


    /**
     * subscriber selector data.
     * @param configInfo config info
     * @return selector data list
     */
    public List<SelectorData> subscriberSelectorData(final String configInfo) {
        List<SelectorData> selectorDataList = GsonUtils.getInstance().toObjectMapList(configInfo, SelectorData.class).values()
                .stream().flatMap(Collection::stream)
                .collect(Collectors.toList());
        selectorDataList.forEach(selectorData -> Optional.ofNullable(pluginDataSubscriber).ifPresent(subscriber -> {
            subscriber.unSelectorSubscribe(selectorData);
            subscriber.onSelectorSubscribe(selectorData);
        }));
        return selectorDataList;
    }

    /**
     * subscriber rule data.
     * @param configInfo config info
     * @return rule data list
     */
    public List<RuleData> subscriberRuleData(final String configInfo) {
        List<RuleData> ruleDataList = GsonUtils.getInstance().toObjectMapList(configInfo, RuleData.class).values()
                .stream().flatMap(Collection::stream)
                .collect(Collectors.toList());
        ruleDataList.forEach(ruleData -> Optional.ofNullable(pluginDataSubscriber).ifPresent(subscriber -> {
            subscriber.unRuleSubscribe(ruleData);
            subscriber.onRuleSubscribe(ruleData);
        }));
        return ruleDataList;
    }

    /**
     * subscriber meta data.
     * @param configInfo config info
     * @return meta data list
     */
    public List<MetaData> subscriberMetaData(final String configInfo) {
        List<MetaData> metaDataList = new ArrayList<>(GsonUtils.getInstance().toObjectMap(configInfo, MetaData.class).values());
        metaDataList.forEach(metaData -> metaDataSubscribers.forEach(subscriber -> {
            subscriber.unSubscribe(metaData);
            subscriber.onSubscribe(metaData);
        }));
        return metaDataList;
    }

    /**
     * subscriber auth data.
     * @param configInfo config info
     * @return auth data list
     */
    public List<AppAuthData> subscriberAuthData(final String configInfo) {
        List<AppAuthData> appAuthDataList = new ArrayList<>(GsonUtils.getInstance().toObjectMap(configInfo, AppAuthData.class).values());
        appAuthDataList.forEach(appAuthData -> authDataSubscribers.forEach(subscriber -> {
            subscriber.unSubscribe(appAuthData);
            subscriber.onSubscribe(appAuthData);
        }));
        return appAuthDataList;
    }

    /**
     * subscriber proxy selector data.
     * @param configInfo config info
     * @return proxy selector data list
     */
    public List<ProxySelectorData> subscriberProxySelectorData(final String configInfo) {
        List<ProxySelectorData> proxySelectorDataList = new ArrayList<>(GsonUtils.getInstance().toObjectMap(configInfo,
                ProxySelectorData.class).values());
        proxySelectorDataList.forEach(discoverySyncData -> proxySelectorDataSubscribers.forEach(subscriber -> {
            subscriber.unSubscribe(discoverySyncData);
        }));
        return proxySelectorDataList;
    }

    /**
     * subscriber discovery sync data.
     * @param configInfo config info
     * @return discovery sync data list
     */
    public List<DiscoverySyncData> subscriberDiscoverySyncData(final String configInfo) {
        List<DiscoverySyncData> discoverySyncDataList = new ArrayList<>(GsonUtils.getInstance().toObjectMap(configInfo,
                DiscoverySyncData.class).values());
        discoverySyncDataList.forEach(discoverySyncData -> discoveryUpstreamDataSubscribers.forEach(subscriber -> {
            subscriber.unSubscribe(discoverySyncData);
        }));
        return discoverySyncDataList;
    }

    /**
     * watch plugin data.
     */
    protected abstract void watchData();

    /**
     * read data.
     *
     * @param configName config name
     * @return config value
     */
    protected abstract String readData(String configName);

    @Override
    public void close() throws Exception {
    }
}
