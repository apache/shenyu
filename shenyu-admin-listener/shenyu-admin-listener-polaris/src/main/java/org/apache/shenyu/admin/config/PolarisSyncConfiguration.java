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

package org.apache.shenyu.admin.config;

import com.tencent.polaris.configuration.api.core.ConfigFilePublishService;
import com.tencent.polaris.configuration.api.core.ConfigFileService;
import com.tencent.polaris.configuration.factory.ConfigFileServiceFactory;
import com.tencent.polaris.configuration.factory.ConfigFileServicePublishFactory;
import com.tencent.polaris.factory.ConfigAPIFactory;
import org.apache.shenyu.admin.config.properties.PolarisProperties;
import org.apache.shenyu.admin.listener.DataChangedInit;
import org.apache.shenyu.admin.listener.DataChangedListener;
import org.apache.shenyu.admin.listener.polaris.PolarisDataChangedInit;
import org.apache.shenyu.admin.listener.polaris.PolarisDataChangedListener;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.Collections;

/**
 * The type Polaris listener.
 */
@Configuration
@ConditionalOnProperty(prefix = "shenyu.sync.polaris", name = "url")
@EnableConfigurationProperties(PolarisProperties.class)
public class PolarisSyncConfiguration {

    /**
     * register configFileService in spring ioc.
     *
     * @param polarisProperties polarisProperties
     * @return ConfigFileService {@linkplain ConfigFileService}
     */
    @Bean
    @ConditionalOnMissingBean(ConfigFileService.class)
    public ConfigFileService polarisConfigFileService(final PolarisProperties polarisProperties) {
        com.tencent.polaris.api.config.Configuration configuration = ConfigAPIFactory.defaultConfig();
        configuration.getConfigFile().getServerConnector().setAddresses(Collections.singletonList(polarisProperties.getUrl()));
        return ConfigFileServiceFactory.createConfigFileService(configuration);
    }

    /**
     * register configFilePublishService in spring ioc.
     *
     * @param polarisProperties polarisProperties
     * @return ConfigFilePublishService {@linkplain ConfigFilePublishService}
     */
    @Bean
    @ConditionalOnMissingBean(ConfigFilePublishService.class)
    public ConfigFilePublishService polarisConfigFilePublishService(final PolarisProperties polarisProperties) {
        com.tencent.polaris.api.config.Configuration configuration = ConfigAPIFactory.defaultConfig();
        configuration.getConfigFile().getServerConnector().setAddresses(Collections.singletonList(polarisProperties.getUrl()));
        return ConfigFileServicePublishFactory.createConfigFilePublishService(configuration);
    }

    /**
     * Data changed listener data changed listener.
     *
     * @param polarisProperties polarisProperties
     * @param configFilePublishService configFilePublishService
     * @param configFileService the config service
     * @return the data changed listener
     */
    @Bean
    @ConditionalOnMissingBean(PolarisDataChangedListener.class)
    public DataChangedListener polarisDataChangedListener(final PolarisProperties polarisProperties, final ConfigFileService configFileService,
                                                          final ConfigFilePublishService configFilePublishService) {
        return new PolarisDataChangedListener(polarisProperties, configFileService, configFilePublishService);
    }

    /**
     * Polaris data init polaris data init.
     *
     * @param polarisProperties polarisProperties
     * @param configFileService the config service
     * @return the polaris data init
     */
    @Bean
    @ConditionalOnMissingBean(PolarisDataChangedInit.class)
    public DataChangedInit polarisDataChangedInit(final PolarisProperties polarisProperties, final ConfigFileService configFileService) {
        return new PolarisDataChangedInit(polarisProperties, configFileService);
    }
}
