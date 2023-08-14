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
import com.ctrip.framework.apollo.enums.PropertyChangeType;
import com.ctrip.framework.apollo.model.ConfigChange;
import org.apache.shenyu.common.constant.ApolloPathConstants;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.DiscoveryUpstreamDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.api.ProxySelectorDataSubscriber;
import org.apache.shenyu.sync.data.api.SyncDataService;
import org.apache.shenyu.sync.data.core.AbstractListDataSyncService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Consumer;

public class ApolloDataService extends AbstractListDataSyncService implements SyncDataService {

    private static final Logger LOG = LoggerFactory.getLogger(ApolloDataService.class);

    private final Config configService;

    private final Map<String, ConfigChangeListener> watchCache = new ConcurrentHashMap<>();

    /**
     * Instantiates a new Nacos sync data service.
     *
     * @param configService         the config service
     * @param pluginDataSubscriber the plugin data subscriber
     * @param metaDataSubscribers   the meta data subscribers
     * @param authDataSubscribers   the auth data subscribers
     */
    public ApolloDataService(final Config configService, final PluginDataSubscriber pluginDataSubscriber,
                             final List<MetaDataSubscriber> metaDataSubscribers,
                             final List<AuthDataSubscriber> authDataSubscribers,
                             final List<ProxySelectorDataSubscriber> proxySelectorDataSubscribers,
                             final List<DiscoveryUpstreamDataSubscriber> discoveryUpstreamDataSubscribers) {
        super(pluginDataSubscriber, metaDataSubscribers, authDataSubscribers, proxySelectorDataSubscribers, discoveryUpstreamDataSubscribers);
        this.configService = configService;

        startWatch();
        apolloWatchPrefixes();
    }

    private void apolloWatchPrefixes() {
        final ConfigChangeListener listener = changeEvent -> {
            changeEvent.changedKeys().forEach(changeKey -> {
                try {
                    final ConfigChange configChange = changeEvent.getChange(changeKey);
                    if (configChange == null) {
                        LOG.error("apollo watchPrefixes error configChange is null {}", changeKey);
                        return;
                    }
                    final String newValue = configChange.getNewValue();
                    if (changeKey.lastIndexOf("list") == changeKey.length() - 4) {
                        return;
                    }
                    if (changeKey.indexOf(ApolloPathConstants.PLUGIN_DATA_ID) == 0) {
                        if (PropertyChangeType.DELETED.equals(configChange.getChangeType())) {
                            unCachePluginData(changeKey);
                        } else {
                            cachePluginData(newValue);
                        }
                    } else if (changeKey.indexOf(ApolloPathConstants.SELECTOR_DATA_ID) == 0) {
                        if (PropertyChangeType.DELETED.equals(configChange.getChangeType())) {
                            unCacheSelectorData(changeKey);
                        } else {
                            cacheSelectorData(newValue);
                        }
                    } else if (changeKey.indexOf(ApolloPathConstants.RULE_DATA_ID) == 0) {
                        if (PropertyChangeType.DELETED.equals(configChange.getChangeType())) {
                            unCacheRuleData(changeKey);
                        } else {
                            cacheRuleData(newValue);
                        }
                    } else if (changeKey.indexOf(ApolloPathConstants.AUTH_DATA_ID) == 0) {
                        if (PropertyChangeType.DELETED.equals(configChange.getChangeType())) {
                            unCacheAuthData(changeKey);
                        } else {
                            cacheAuthData(newValue);
                        }
                    } else if (changeKey.indexOf(ApolloPathConstants.META_DATA_ID) == 0) {
                        if (PropertyChangeType.DELETED.equals(configChange.getChangeType())) {
                            unCacheMetaData(changeKey);
                        } else {
                            cacheMetaData(newValue);
                        }
                    } else if (changeKey.indexOf(ApolloPathConstants.PROXY_SELECTOR_DATA_ID) == 0) {
                        if (PropertyChangeType.DELETED.equals(configChange.getChangeType())) {
                            unCacheProxySelectorData(changeKey);
                        } else {
                            cacheProxySelectorData(newValue);
                        }
                    } else if (changeKey.indexOf(ApolloPathConstants.DISCOVERY_DATA_ID) == 0) {
                        if (PropertyChangeType.DELETED.equals(configChange.getChangeType())) {
                            unCacheProxySelectorData(changeKey);
                        } else {
                            cacheDiscoveryUpstreamData(newValue);
                        }
                    }

                } catch (Exception e) {
                    LOG.error("nacos sync listener receiveConfigInfo error", e);
                }
            });
        };

        configService.addChangeListener(listener, Collections.emptySet(), ApolloPathConstants.pathKeySet());

    }

    @Override
    protected void removeListener(final String key) {
        // No need to implement
    }

    @Override
    protected List<String> getConfigListOnWatch(final String key, final Consumer<String> updateHandler) {
        try {
            if (watchCache.containsKey(key)) {
                return GsonUtils.getInstance().fromList(configService.getProperty(key, null), String.class);
            }
            final String serviceConfig = getServiceConfig(key, updateHandler, null);
            return GsonUtils.getInstance().fromList(serviceConfig, String.class);
        } catch (Exception e) {
            throw new ShenyuException(e);
        }
    }

    @Override
    protected String getServiceConfig(final String key, final Consumer<String> updateHandler,
                                      final Consumer<String> deleteHandler) {
        return configService.getProperty(key, null);
    }

    @Override
    protected String getConfigOnWatch(final String key, final Consumer<String> updateHandler, final Consumer<String> deleteHandler) {
        try {
            return getServiceConfig(key, updateHandler, deleteHandler);
        } catch (Exception e) {
            throw new ShenyuException(e);
        }
    }

    /**
     * close.
     */
    @Override
    public void close() {
        watchCache.forEach((key, value) -> configService.removeChangeListener(value));
    }
}
