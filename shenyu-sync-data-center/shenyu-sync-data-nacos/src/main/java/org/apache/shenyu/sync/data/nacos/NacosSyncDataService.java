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
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.NacosPathConstants;
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

import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executor;
import java.util.function.Consumer;

/**
 * The type Nacos sync data service.
 */
public class NacosSyncDataService extends AbstractListDataSyncService implements SyncDataService {

    private static final Logger LOG = LoggerFactory.getLogger(NacosSyncDataService.class);

    private final ConcurrentHashMap<String, Listener> watchCache = new ConcurrentHashMap<>();

    private final ConfigService configService;

    /**
     * Instantiates a new Nacos sync data service.
     *
     * @param configService         the config service
     * @param pluginDataSubscriber the plugin data subscriber
     * @param metaDataSubscribers   the meta data subscribers
     * @param authDataSubscribers   the auth data subscribers
     */
    public NacosSyncDataService(final ConfigService configService, final PluginDataSubscriber pluginDataSubscriber,
                                final List<MetaDataSubscriber> metaDataSubscribers,
                                final List<AuthDataSubscriber> authDataSubscribers,
                                final List<ProxySelectorDataSubscriber> proxySelectorDataSubscribers,
                                final List<DiscoveryUpstreamDataSubscriber> discoveryUpstreamDataSubscribers) {
        super(pluginDataSubscriber, metaDataSubscribers, authDataSubscribers, proxySelectorDataSubscribers, discoveryUpstreamDataSubscribers);
        this.configService = configService;
        startWatch();
    }

    @Override
    protected void removeListener(final String key) {
        final Listener listener = watchCache.get(key);
        if (!ObjectUtils.isEmpty(listener)) {
            configService.removeListener(key, NacosPathConstants.GROUP, listener);
            watchCache.remove(key);
            LOG.info("nacos sync remove listener key:{}", key);
        }
    }

    @Override
    protected List<String> getConfigListOnWatch(final String key, final Consumer<String> updateHandler) {
        try {
            if (watchCache.containsKey(key)) {
                return GsonUtils.getInstance().fromList(configService.getConfig(key, NacosPathConstants.GROUP, 3000), String.class);
            }
            final String serviceConfig = getServiceConfig(key, updateHandler, null);
            return GsonUtils.getInstance().fromList(serviceConfig, String.class);
        } catch (Exception e) {
            throw new ShenyuException(e);
        }
    }

    @Override
    protected String getServiceConfig(final String key, final Consumer<String> updateHandler, final Consumer<String> deleteHandler) {
        try {
            final Listener listener = new Listener() {
                @Override
                public Executor getExecutor() {
                    return null;
                }

                @Override
                public void receiveConfigInfo(final String configInfo) {
                    try {
                        if (StringUtils.isBlank(configInfo) && deleteHandler != null) {
                            deleteHandler.accept(key);
                        } else {
                            updateHandler.accept(configInfo);
                        }
                    } catch (Exception e) {
                        LOG.error("nacos sync listener receiveConfigInfo error", e);
                    }
                }
            };
            final String serviceConfig = configService.getConfigAndSignListener(key, NacosPathConstants.GROUP, 3000, listener);
            watchCache.put(key, listener);
            return serviceConfig;
        } catch (Exception e) {
            throw new ShenyuException(e);
        }
    }

    @Override
    protected String getConfigOnWatch(final String key, final Consumer<String> updateHandler, final Consumer<String> deleteHandler) {
        try {
            if (watchCache.containsKey(key)) {
                return null;
            }
            return getServiceConfig(key, updateHandler, deleteHandler);
        } catch (Exception e) {
            throw new ShenyuException(e);
        }
    }

    @Override
    public void close() {
        watchCache.forEach((dataId, lss) -> {
            configService.removeListener(dataId, NacosPathConstants.GROUP, lss);
            watchCache.remove(dataId);
            LOG.info("nacos sync remove listener key:{}", dataId);
        });
    }
}
