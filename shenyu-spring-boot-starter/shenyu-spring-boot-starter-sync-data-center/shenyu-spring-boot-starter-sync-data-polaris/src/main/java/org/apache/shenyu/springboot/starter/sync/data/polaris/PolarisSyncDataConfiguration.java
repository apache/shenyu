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

package org.apache.shenyu.springboot.starter.sync.data.polaris;

import com.tencent.polaris.configuration.api.core.ConfigFileService;
import com.tencent.polaris.configuration.factory.ConfigFileServiceFactory;
import com.tencent.polaris.factory.ConfigAPIFactory;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.DiscoveryUpstreamDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.api.ProxySelectorDataSubscriber;
import org.apache.shenyu.sync.data.api.SyncDataService;
import org.apache.shenyu.sync.data.polaris.PolarisSyncDataService;
import org.apache.shenyu.sync.data.polaris.config.PolarisConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.Collections;
import java.util.List;

/**
 * Polaris sync data configuration for spring boot.
 */
@Configuration
@ConditionalOnClass(PolarisSyncDataService.class)
@ConditionalOnProperty(prefix = "shenyu.sync.polaris", name = "url")
public class PolarisSyncDataConfiguration {

    private static final Logger LOGGER = LoggerFactory.getLogger(PolarisSyncDataConfiguration.class);

    /**
     * Polaris sync data service.
     *
     * @param polarisConfig the polaris config
     * @param configFileServices the config service
     * @param pluginSubscriber  the plugin subscriber
     * @param metaSubscribers  the meta subscribers
     * @param authSubscribers  the auth subscribers
     * @param proxySelectorSubscribers   the auth subscribers
     * @param discoveryUpstreamDataSubscribers discoveryUpstreamDataSubscribers
     * @param shenyuConfig the shenyu config
     * @return the sync data service
     */
    @Bean
    public SyncDataService polarisSyncDataService(final ObjectProvider<PolarisConfig> polarisConfig, final ObjectProvider<ConfigFileService> configFileServices,
                                                  final ObjectProvider<PluginDataSubscriber> pluginSubscriber,
                                                  final ObjectProvider<List<MetaDataSubscriber>> metaSubscribers,
                                                  final ObjectProvider<List<AuthDataSubscriber>> authSubscribers,
                                                  final ObjectProvider<List<ProxySelectorDataSubscriber>> proxySelectorSubscribers,
                                                  final ObjectProvider<List<DiscoveryUpstreamDataSubscriber>> discoveryUpstreamDataSubscribers,
                                                  final ObjectProvider<ShenyuConfig> shenyuConfig) {
        LOGGER.info("you use polaris sync shenyu data.......");
        return new PolarisSyncDataService(polarisConfig.getIfAvailable(), configFileServices.getIfAvailable(), pluginSubscriber.getIfAvailable(),
                metaSubscribers.getIfAvailable(Collections::emptyList), authSubscribers.getIfAvailable(Collections::emptyList),
                proxySelectorSubscribers.getIfAvailable(), discoveryUpstreamDataSubscribers.getIfAvailable(),
                shenyuConfig.getIfAvailable());
    }

    /**
     * Polaris configFileService.
     *
     * @param polarisConfig the polaris config
     *
     * @return the config service
     */
    @Bean
    public ConfigFileService polarisConfigServices(final PolarisConfig polarisConfig) {
        com.tencent.polaris.api.config.Configuration configuration = ConfigAPIFactory.defaultConfig();
        configuration.getConfigFile().getServerConnector().setAddresses(Collections.singletonList(polarisConfig.getUrl()));
        return ConfigFileServiceFactory.createConfigFileService(configuration);
    }

    /**
     * Polaris config service.
     *
     * @return the config.
     */
    @Bean
    @ConfigurationProperties(prefix = "shenyu.sync.polaris")
    public PolarisConfig polarisConfig() {
        return new PolarisConfig();
    }

}
