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

package org.apache.shenyu.sync.data.polaris;

import com.tencent.polaris.configuration.api.core.ChangeType;
import com.tencent.polaris.configuration.api.core.ConfigFile;
import com.tencent.polaris.configuration.api.core.ConfigFileChangeListener;
import com.tencent.polaris.configuration.api.core.ConfigFileService;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.NacosPathConstants;
import org.apache.shenyu.common.constant.PolarisPathConstants;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.DiscoveryUpstreamDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.api.ProxySelectorDataSubscriber;
import org.apache.shenyu.sync.data.api.SyncDataService;
import org.apache.shenyu.sync.data.core.AbstractNodeDataSyncService;
import org.apache.shenyu.sync.data.polaris.config.PolarisConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Consumer;

/**
 * The type Polaris sync data service.
 */
public class PolarisSyncDataService extends AbstractNodeDataSyncService implements SyncDataService {

    private static final Logger LOG = LoggerFactory.getLogger(PolarisSyncDataService.class);

    private final PolarisConfig polarisConfig;

    private final ConfigFileService configFileService;

    private final Map<String, ConfigFileChangeListener> watchCache = new ConcurrentHashMap<>();

    public PolarisSyncDataService(final PolarisConfig polarisConfig, final ConfigFileService configFileService, final PluginDataSubscriber pluginDataSubscriber,
                                  final List<MetaDataSubscriber> metaDataSubscribers, final List<AuthDataSubscriber> authDataSubscribers,
                                  final List<ProxySelectorDataSubscriber> proxySelectorDataSubscribers,
                                  final List<DiscoveryUpstreamDataSubscriber> discoveryUpstreamDataSubscribers,
                                  final ShenyuConfig shenyuConfig) {
        super(new ChangeData(PolarisPathConstants.PLUGIN_DATA_FILE_NAME, PolarisPathConstants.SELECTOR_DATA_FILE_NAME,
                        PolarisPathConstants.RULE_DATA_FILE_NAME, PolarisPathConstants.AUTH_DATA_ID_FILE_NAME,
                        PolarisPathConstants.META_DATA_FILE_NAME, PolarisPathConstants.PROXY_SELECTOR_FILE_NAME, NacosPathConstants.DISCOVERY_DATA_ID),
                pluginDataSubscriber, metaDataSubscribers,
                authDataSubscribers, proxySelectorDataSubscribers, discoveryUpstreamDataSubscribers, shenyuConfig);
        this.polarisConfig = polarisConfig;
        this.configFileService = configFileService;
        startWatch();
    }

    @Override
    protected String getServiceConfig(final String key, final Consumer<String> updateHandler, final Consumer<String> deleteHandler) {
        if (watchCache.containsKey(key)) {
            return null;
        }
        try {
            final ConfigFileChangeListener listener = event -> {
                try {
                    if (event.getChangeType().equals(ChangeType.DELETED) || StringUtils.isBlank(event.getNewValue())) {
                        Optional.ofNullable(deleteHandler).ifPresent(deleteHandler2 -> deleteHandler2.accept(key));
                    } else {
                        if (event.getNewValue().equals(event.getOldValue())) {
                            return;
                        }
                        updateHandler.accept(event.getNewValue());
                    }
                } catch (Exception e) {
                    LOG.error("Polaris sync listener receiveConfigInfo error", e);
                }
            };
            final ConfigFile configFile = configFileService.getConfigFile(polarisConfig.getNamespace(), polarisConfig.getFileGroup(), key);
            configFile.addChangeListener(listener);
            watchCache.put(key, listener);
            return configFile.hasContent() ? configFile.getContent() : null;
        } catch (Exception e) {
            throw new ShenyuException(e);
        }
    }

    @Override
    protected void doRemoveListener(final String removeKey) {
        final ConfigFileChangeListener configFileChangeListener = watchCache.get(removeKey);
        if (Objects.nonNull(configFileChangeListener)) {
            final ConfigFile configFile = configFileService.getConfigFile(polarisConfig.getNamespace(), polarisConfig.getFileGroup(), removeKey);
            configFile.removeChangeListener(configFileChangeListener);
        }
    }

    @Override
    public void close() {
        watchCache.forEach((key, configFileChangeListener) -> {
            if (Objects.nonNull(configFileChangeListener)) {
                final ConfigFile configFile = configFileService.getConfigFile(polarisConfig.getNamespace(), polarisConfig.getFileGroup(), key);
                configFile.removeChangeListener(configFileChangeListener);
            }
        });
    }
}
