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
import org.apache.shenyu.common.constant.ListDataNodePathConstants;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.sync.data.api.AbstarctListDataNodeSyncDataService;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.DiscoveryUpstreamDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.api.ProxySelectorDataSubscriber;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * The type Apollo data service.
 */
public class ApolloDataService extends AbstarctListDataNodeSyncDataService {

    /**
     * logger.
     */
    private static final Logger LOG = LoggerFactory.getLogger(ApolloDataService.class);

    private final Config configService;

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
                             final List<AuthDataSubscriber> authDataSubscribers,
                             final List<ProxySelectorDataSubscriber> proxySelectorDataSubscribers,
                             final List<DiscoveryUpstreamDataSubscriber> discoveryUpstreamDataSubscribers) {
        super(pluginDataSubscriber, metaDataSubscribers, authDataSubscribers, proxySelectorDataSubscribers, discoveryUpstreamDataSubscribers);
        this.configService = configService;
        subAllData();
        watchData();

    }

    /**
     * watch plugin data.
     */
    @Override
    protected void watchData() {
        ConfigChangeListener configChangeListener = changeEvent -> changeEvent.changedKeys().forEach(key -> {
            switch (key) {
                case ListDataNodePathConstants.PLUGIN_DATA_ID:
                    List<PluginData> pluginData = subscriberPluginData(readData(key));
                    LOG.info("apollo listener pluginData: {}", pluginData);
                    break;
                case ListDataNodePathConstants.SELECTOR_DATA_ID:
                    List<SelectorData> selectorDataList = subscriberSelectorData(readData(key));
                    LOG.info("apollo listener selectorData: {}", selectorDataList);
                    break;
                case ListDataNodePathConstants.RULE_DATA_ID:
                    List<RuleData> ruleDataList = subscriberRuleData(readData(key));
                    LOG.info("apollo listener ruleData: {}", ruleDataList);
                    break;
                case ListDataNodePathConstants.META_DATA_ID:
                    List<MetaData> metaDataList = subscriberMetaData(readData(key));
                    LOG.info("apollo listener metaData: {}", metaDataList);
                    break;
                case ListDataNodePathConstants.AUTH_DATA_ID:
                    List<AppAuthData> appAuthDataList = subscriberAuthData(readData(key));
                    LOG.info("apollo listener authData: {}", appAuthDataList);
                    break;
                case ListDataNodePathConstants.PROXY_SELECTOR_DATA_ID:
                    List<ProxySelectorData> proxySelectorData = subscriberProxySelectorData(readData(key));
                    LOG.info("apollo listener ProxySelectorData: {}", proxySelectorData);
                    break;
                case ListDataNodePathConstants.DISCOVERY_DATA_ID:
                    List<DiscoverySyncData> discoverySyncData = subscriberDiscoverySyncData(readData(key));
                    LOG.info("apollo listener discoverySyncData: {}", discoverySyncData);
                    break;
                default:
                    break;
            }
        });
        cache.put(ListDataNodePathConstants.pathKeySet().toString(), configChangeListener);
        configService.addChangeListener(configChangeListener, ListDataNodePathConstants.pathKeySet());
    }

    /**
     * read data.
     * @param configName config name
     * @return config value
     */
    @Override
    protected String readData(final String configName) {
        return configService.getProperty(configName, "{}");
    }

    /**
     * close.
     */
    @Override
    public void close() {
        cache.forEach((key, value) -> configService.removeChangeListener(value));
    }
}
