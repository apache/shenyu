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

import com.tencent.polaris.configuration.api.core.ConfigFileService;
import org.apache.shenyu.common.constant.PolarisPathConstants;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.api.ProxySelectorDataSubscriber;
import org.apache.shenyu.sync.data.api.SyncDataService;
import org.apache.shenyu.sync.data.polaris.config.PolarisConfig;
import org.apache.shenyu.sync.data.polaris.handler.PolarisCacheHandler;

import java.util.List;

/**
 * The type Polaris sync data service.
 */
public class PolarisSyncDataService extends PolarisCacheHandler implements SyncDataService {

    private final PolarisConfig polarisConfig;

    public PolarisSyncDataService(final PolarisConfig polarisConfig, final ConfigFileService configFileService, final PluginDataSubscriber pluginDataSubscriber,
                                  final List<MetaDataSubscriber> metaDataSubscribers, final List<AuthDataSubscriber> authDataSubscribers,
                                  final List<ProxySelectorDataSubscriber> proxySelectorDataSubscribers) {

        super(configFileService, pluginDataSubscriber, metaDataSubscribers, authDataSubscribers, proxySelectorDataSubscribers);
        this.polarisConfig = polarisConfig;
        start();
    }

    /**
     * Start.
     */
    public void start() {
        watcherData(PolarisPathConstants.RULE_DATA_FILE_NAME, this::updateRuleMap);
        watcherData(PolarisPathConstants.AUTH_DATA_ID_FILE_NAME, this::updateAuthMap);
        watcherData(PolarisPathConstants.PLUGIN_DATA_FILE_NAME, this::updatePluginMap);
        watcherData(PolarisPathConstants.META_DATA_FILE_NAME, this::updateMetaDataMap);
        watcherData(PolarisPathConstants.SELECTOR_DATA_FILE_NAME, this::updateSelectorMap);
        watcherData(PolarisPathConstants.PROXY_SELECTOR_FILE_NAME, this::updateProxySelectorMap);
    }

    @Override
    public void close() {
        LISTENERS.forEach((dataId, lss) -> {
            lss.forEach(listener -> getConfigFileService()
                    .getConfigFile(polarisConfig.getNamespace(), polarisConfig.getFileGroup(), dataId)
                    .removeChangeListener(listener));
            lss.clear();
        });
        LISTENERS.clear();
    }
}
