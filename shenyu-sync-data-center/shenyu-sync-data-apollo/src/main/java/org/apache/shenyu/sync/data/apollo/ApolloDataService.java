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

package org.apache.shenyu.sync.data.apollo;

import com.ctrip.framework.apollo.Config;
import com.ctrip.framework.apollo.ConfigChangeListener;
import org.apache.shenyu.common.constant.ApolloPathConstants;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.api.SyncDataService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

public class ApolloDataService implements SyncDataService {

    /**
     * logger.
     */
    private static final Logger LOG = LoggerFactory.getLogger(ApolloDataService.class);

    private final Config configService;

    private final PluginDataSubscriber pluginDataSubscriber;

    private final List<MetaDataSubscriber> metaDataSubscribers;

    private final List<AuthDataSubscriber> authDataSubscribers;

    private final Map<String, ConfigChangeListener> cache = new ConcurrentHashMap<>();

    /**
     * Instantiates a new Apollo data service.
     *
     * @param configService the config service
     * @param pluginDataSubscriber the plugin data subscriber
     * @param metaDataSubscribers the meta data subscribers
     * @param authDataSubscribers the auth data subscribers
     */
    public ApolloDataService(final Config configService, final PluginDataSubscriber pluginDataSubscriber,
                             final List<MetaDataSubscriber> metaDataSubscribers,
                             final List<AuthDataSubscriber> authDataSubscribers) {
        this.configService = configService;
        this.pluginDataSubscriber = pluginDataSubscriber;
        this.metaDataSubscribers = metaDataSubscribers;
        this.authDataSubscribers = authDataSubscribers;
        subAllData();
        watchData();

    }

    /**
     * sub all data.
     */
    public void subAllData() {
        subscriberAuthData();
        subscriberMetaData();
        subscriberPluginData();
        subscriberRuleData();
        subscriberSelectorData();
    }

    /**
     * watch plugin data.
     * @return plugin data list
     */
    public List<PluginData> subscriberPluginData() {
        List<PluginData> pluginDataList = new ArrayList<>(GsonUtils.getInstance().toObjectMap(configService.getProperty(ApolloPathConstants.PLUGIN_DATA_ID, "{}"), PluginData.class).values());
        pluginDataList.forEach(pluginData -> Optional.ofNullable(pluginDataSubscriber).ifPresent(subscriber -> {
            subscriber.unSubscribe(pluginData);
            subscriber.onSubscribe(pluginData);
        }));
        return pluginDataList;
    }

    /**
     * subscriber selector data.
     * @return selector data list
     */
    public List<SelectorData> subscriberSelectorData() {
        List<SelectorData> selectorDataList = GsonUtils.getInstance().toObjectMapList(configService.getProperty(ApolloPathConstants.SELECTOR_DATA_ID, "{}"), SelectorData.class).values()
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
     * @return rule data list
     */
    public List<RuleData> subscriberRuleData() {
        List<RuleData> ruleDataList = GsonUtils.getInstance().toObjectMapList(configService.getProperty(ApolloPathConstants.RULE_DATA_ID, "{}"), RuleData.class).values()
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
     * @return meta data list
     */
    public List<MetaData> subscriberMetaData() {
        List<MetaData> metaDataList = new ArrayList<>(GsonUtils.getInstance().toObjectMap(configService.getProperty(ApolloPathConstants.META_DATA_ID, "{}"), MetaData.class).values());
        metaDataList.forEach(metaData -> metaDataSubscribers.forEach(subscriber -> {
            subscriber.unSubscribe(metaData);
            subscriber.onSubscribe(metaData);
        }));
        return metaDataList;
    }

    /**
     * subscriber auth data.
     * @return auth data list
     */
    public List<AppAuthData> subscriberAuthData() {
        List<AppAuthData> appAuthDataList = new ArrayList<>(GsonUtils.getInstance().toObjectMap(configService.getProperty(ApolloPathConstants.AUTH_DATA_ID, "{}"), AppAuthData.class).values());
        appAuthDataList.forEach(appAuthData -> authDataSubscribers.forEach(subscriber -> {
            subscriber.unSubscribe(appAuthData);
            subscriber.onSubscribe(appAuthData);
        }));
        return appAuthDataList;
    }

    /**
     * watch plugin data.
     */
    private void watchData() {
        ConfigChangeListener configChangeListener = changeEvent -> changeEvent.changedKeys().forEach(key -> {
            switch (key) {
                case ApolloPathConstants.PLUGIN_DATA_ID:
                    List<PluginData> pluginData = subscriberPluginData();
                    LOG.info("apollo listener pluginData: {}", pluginData);
                    break;
                case ApolloPathConstants.SELECTOR_DATA_ID:
                    List<SelectorData> selectorDataList = subscriberSelectorData();
                    LOG.info("apollo listener selectorData: {}", selectorDataList);
                    break;
                case ApolloPathConstants.RULE_DATA_ID:
                    List<RuleData> ruleDataList = subscriberRuleData();
                    LOG.info("apollo listener ruleData: {}", ruleDataList);
                    break;
                case ApolloPathConstants.META_DATA_ID:
                    List<MetaData> metaDataList = subscriberMetaData();
                    LOG.info("apollo listener metaData: {}", metaDataList);
                    break;
                case ApolloPathConstants.AUTH_DATA_ID:
                    List<AppAuthData> appAuthDataList = subscriberAuthData();
                    LOG.info("apollo listener authData: {}", appAuthDataList);
                    break;
                default:
                    break;
            }
        });
        cache.put(ApolloPathConstants.pathKeySet().toString(), configChangeListener);
        configService.addChangeListener(configChangeListener, ApolloPathConstants.pathKeySet());
    }

    /**
     * close.
     */
    @Override
    public void close() {
        cache.forEach((key, value) -> configService.removeChangeListener(value));
    }
}
