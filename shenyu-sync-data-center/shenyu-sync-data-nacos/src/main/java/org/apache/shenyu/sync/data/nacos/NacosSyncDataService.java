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

package org.apache.shenyu.sync.data.nacos;

import com.alibaba.nacos.api.config.ConfigService;
import com.alibaba.nacos.api.config.listener.Listener;
import com.alibaba.nacos.api.exception.NacosException;
import com.google.common.collect.Maps;
import org.apache.shenyu.common.constant.ListDataNodePathConstants;
import org.apache.shenyu.common.utils.MapUtils;
import org.apache.shenyu.sync.data.api.AbstarctListDataNodeSyncDataService;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.DiscoveryUpstreamDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.api.ProxySelectorDataSubscriber;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.Executor;

/**
 * The type Nacos sync data service.
 */
public class NacosSyncDataService extends AbstarctListDataNodeSyncDataService {

    protected static final Map<String, List<Listener>> LISTENERS = Maps.newConcurrentMap();

    private static final Logger LOG = LoggerFactory.getLogger(NacosSyncDataService.class);

    private final ConfigService configService;

    /**
     * Instantiates a new Nacos sync data service.
     *
     * @param configService                    the config service
     * @param pluginDataSubscriber             the plugin data subscriber
     * @param metaDataSubscribers              the meta data subscribers
     * @param authDataSubscribers              the auth data subscribers
     * @param proxySelectorDataSubscribers     the proxy selector data subscribers
     * @param discoveryUpstreamDataSubscribers the discovery upstream data subscribers
     */
    public NacosSyncDataService(final ConfigService configService, final PluginDataSubscriber pluginDataSubscriber,
                                final List<MetaDataSubscriber> metaDataSubscribers,
                                final List<AuthDataSubscriber> authDataSubscribers,
                                final List<ProxySelectorDataSubscriber> proxySelectorDataSubscribers,
                                final List<DiscoveryUpstreamDataSubscriber> discoveryUpstreamDataSubscribers) {
        super(pluginDataSubscriber, metaDataSubscribers, authDataSubscribers, proxySelectorDataSubscribers, discoveryUpstreamDataSubscribers);
        this.configService = configService;
        watchData();
    }

    /**
     * Watch data.
     */
    @Override
    public void watchData() {
        startWatcherData(ListDataNodePathConstants.PLUGIN_DATA_ID, this::subscriberPluginData);
        startWatcherData(ListDataNodePathConstants.SELECTOR_DATA_ID, this::subscriberSelectorData);
        startWatcherData(ListDataNodePathConstants.RULE_DATA_ID, this::subscriberRuleData);
        startWatcherData(ListDataNodePathConstants.META_DATA_ID, this::subscriberMetaData);
        startWatcherData(ListDataNodePathConstants.AUTH_DATA_ID, this::subscriberAuthData);
        startWatcherData(ListDataNodePathConstants.PROXY_SELECTOR_DATA_ID, this::subscriberProxySelectorData);
        startWatcherData(ListDataNodePathConstants.DISCOVERY_DATA_ID, this::subscriberDiscoverySyncData);
    }

    /**
     * Subscriber plugin data.
     *
     * @param configName config name
     * @return plugin data
     */
    @Override
    public String readData(final String configName) {
        //no need to implement, because has getConfigAndSignListener
        return null;
    }

    /**
     * Close.
     */
    @Override
    public void close() {
        LISTENERS.forEach((dataId, lss) -> {
            lss.forEach(listener -> configService.removeListener(dataId, ListDataNodePathConstants.GROUP, listener));
            lss.clear();
        });
        LISTENERS.clear();
    }

    /**
     * Start watcher data.
     *
     * @param dataId dataId
     * @param oc     oc
     */
    public void startWatcherData(final String dataId, final OnChange oc) {
        Listener listener = new Listener() {
            @Override
            public void receiveConfigInfo(final String configInfo) {
                oc.change(configInfo);
            }

            @Override
            public Executor getExecutor() {
                return null;
            }
        };
        oc.change(getConfigAndSignListener(dataId, listener));
        MapUtils.computeIfAbsent(LISTENERS, dataId, key -> new ArrayList<>()).add(listener);
    }

    /**
     * Gets config and sign listener.
     *
     * @param dataId   dataId
     * @param listener listener
     * @return config
     */
    private String getConfigAndSignListener(final String dataId, final Listener listener) {
        String config = null;
        try {
            config = configService.getConfigAndSignListener(dataId, ListDataNodePathConstants.GROUP, ListDataNodePathConstants.DEFAULT_TIME_OUT, listener);
        } catch (NacosException e) {
            LOG.error(e.getMessage(), e);
        }
        if (Objects.isNull(config)) {
            config = "{}";
        }
        return config;
    }

    /**
     * The interface On change.
     */
    public interface OnChange {
        /**
         * Change.
         * @param changeData changeData
         */
        void change(String changeData);
    }

}
