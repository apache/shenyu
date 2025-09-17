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

package org.apache.shenyu.springboot.starter.sync.data.nacos;

import com.alibaba.nacos.api.NacosFactory;
import com.alibaba.nacos.api.PropertyKeyConst;
import com.alibaba.nacos.api.config.ConfigService;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.infra.nacos.autoconfig.ConditionOnSyncNacos;
import org.apache.shenyu.infra.nacos.autoconfig.NacosProperties;
import org.apache.shenyu.infra.nacos.config.NacosConfig;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.DiscoveryUpstreamDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.api.ProxySelectorDataSubscriber;
import org.apache.shenyu.sync.data.api.SyncDataService;
import org.apache.shenyu.sync.data.nacos.NacosSyncDataService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Properties;

/**
 * Nacos sync data configuration for spring boot.
 */

@Configuration
@ConditionOnSyncNacos
@ConditionalOnClass({NacosSyncDataService.class, ConfigService.class})
@EnableConfigurationProperties(NacosProperties.class)
public class NacosSyncDataConfiguration {

    private static final Logger LOGGER = LoggerFactory.getLogger(NacosSyncDataConfiguration.class);

    /**
     * Nacos sync data service.
     *
     * @param configService     the config service
     * @param pluginSubscriber the plugin subscriber
     * @param metaSubscribers   the meta subscribers
     * @param authSubscribers   the auth subscribers
     * @param proxySelectorSubscribers   the auth subscribers
     * @param discoveryUpstreamDataSubscribers discoveryUpstreamDataSubscribers
     * @param shenyuConfig      the shenyu config
     * @return the sync data service
     */
    @Bean
    public SyncDataService nacosSyncDataService(final ObjectProvider<ConfigService> configService, final ObjectProvider<PluginDataSubscriber> pluginSubscriber,
                                                final ObjectProvider<List<MetaDataSubscriber>> metaSubscribers, final ObjectProvider<List<AuthDataSubscriber>> authSubscribers,
                                                final ObjectProvider<List<ProxySelectorDataSubscriber>> proxySelectorSubscribers,
                                                final ObjectProvider<List<DiscoveryUpstreamDataSubscriber>> discoveryUpstreamDataSubscribers,
                                                final ObjectProvider<ShenyuConfig> shenyuConfig) {
        LOGGER.info("you use nacos sync shenyu data.......");
        return new NacosSyncDataService(configService.getIfAvailable(), pluginSubscriber.getIfAvailable(),
                metaSubscribers.getIfAvailable(Collections::emptyList), authSubscribers.getIfAvailable(Collections::emptyList),
                proxySelectorSubscribers.getIfAvailable(), discoveryUpstreamDataSubscribers.getIfAvailable(), shenyuConfig.getIfAvailable());
    }

    /**
     * Nacos config service.
     *
     * @param nacosConfig the nacos config
     * @return the config service
     * @throws Exception the exception
     */
    @Bean
    public ConfigService nacosConfigService(final NacosConfig nacosConfig) throws Exception {
        Properties properties = new Properties();
        if (Objects.nonNull(nacosConfig.getAcm()) && nacosConfig.getAcm().isEnabled()) {
            properties.put(PropertyKeyConst.ENDPOINT, nacosConfig.getAcm().getEndpoint());
            properties.put(PropertyKeyConst.NAMESPACE, nacosConfig.getAcm().getNamespace());
            properties.put(PropertyKeyConst.ACCESS_KEY, nacosConfig.getAcm().getAccessKey());
            properties.put(PropertyKeyConst.SECRET_KEY, nacosConfig.getAcm().getSecretKey());
        } else {
            properties.put(PropertyKeyConst.SERVER_ADDR, nacosConfig.getUrl());
            if (StringUtils.isNotBlank(nacosConfig.getNamespace())) {
                properties.put(PropertyKeyConst.NAMESPACE, nacosConfig.getNamespace());
            }
            if (Objects.nonNull(nacosConfig.getUsername())) {
                properties.put(PropertyKeyConst.USERNAME, nacosConfig.getUsername());
            }
            if (Objects.nonNull(nacosConfig.getPassword())) {
                properties.put(PropertyKeyConst.PASSWORD, nacosConfig.getPassword());
            }
            if (StringUtils.isNotBlank(nacosConfig.getContextPath())) {
                properties.put(PropertyKeyConst.CONTEXT_PATH, nacosConfig.getContextPath());
            }
        }
        return NacosFactory.createConfigService(properties);
    }

    /**
     * nacos config.
     *
     * @param nacosProperties the nacos properties
     * @return the nacos config
     */
    @Bean
    @ConditionOnSyncNacos
    public NacosConfig nacosConfig(final NacosProperties nacosProperties) {
        return nacosProperties.getNacos();
    }
}
