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

package org.apache.shenyu.springboot.sync.data.consul;

import com.ecwid.consul.v1.ConsulClient;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.DiscoveryUpstreamDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.api.ProxySelectorDataSubscriber;
import org.apache.shenyu.sync.data.api.SyncDataService;
import org.apache.shenyu.sync.data.consul.ConsulSyncDataService;
import org.apache.shenyu.sync.data.consul.config.ConsulConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Collections;
import java.util.List;

/**
 * Consul sync data configuration for spring boot.
 */
@Configuration
@ConditionalOnClass(ConsulSyncDataConfiguration.class)
@ConditionalOnProperty(prefix = "shenyu.sync.consul", name = "url")
public class ConsulSyncDataConfiguration {

    private static final Logger LOGGER = LoggerFactory.getLogger(ConsulSyncDataConfiguration.class);

    /**
     * Sync data service.
     *
     * @param shenyuConfig     the shenyu config
     * @param consulClient     the consul client
     * @param consulConfig     the consul config
     * @param pluginSubscriber the plugin subscriber
     * @param metaSubscribers   the meta subscribers
     * @param authSubscribers   the auth subscribers
     * @param proxySelectorSubscribers   the proxySelectorSubscribers
     * @param discoveryUpstreamSubscribers   the discoveryUpstreamSubscribers
     * @return the sync data service
     */
    @Bean
    public SyncDataService syncDataService(final ObjectProvider<ShenyuConfig> shenyuConfig,
                                           final ObjectProvider<ConsulClient> consulClient,
                                           final ObjectProvider<ConsulConfig> consulConfig,
                                           final ObjectProvider<PluginDataSubscriber> pluginSubscriber,
                                           final ObjectProvider<List<MetaDataSubscriber>> metaSubscribers,
                                           final ObjectProvider<List<AuthDataSubscriber>> authSubscribers,
                                           final ObjectProvider<List<ProxySelectorDataSubscriber>> proxySelectorSubscribers,
                                           final ObjectProvider<List<DiscoveryUpstreamDataSubscriber>> discoveryUpstreamSubscribers) {
        LOGGER.info("you use consul sync shenyu data.......");
        return new ConsulSyncDataService(shenyuConfig.getIfAvailable(), consulClient.getIfAvailable(), consulConfig.getIfAvailable(), pluginSubscriber.getIfAvailable(),
                metaSubscribers.getIfAvailable(Collections::emptyList), authSubscribers.getIfAvailable(Collections::emptyList),
                proxySelectorSubscribers.getIfAvailable(Collections::emptyList), discoveryUpstreamSubscribers.getIfAvailable(Collections::emptyList));
    }

    /**
     * Consul config.
     *
     * @return the consul config
     */
    @Bean
    @ConfigurationProperties(prefix = "shenyu.sync.consul")
    public ConsulConfig consulConfig() {
        return new ConsulConfig();
    }


    /**
     * init Consul client.
     * @param consulConfig the consul config
     * @return Consul client
     */
    @Bean
    public ConsulClient consulClient(final ConsulConfig consulConfig) {
        String url = consulConfig.getUrl();
        if (StringUtils.isBlank(url)) {
            throw new ShenyuException("sync.consul.url can not be null.");
        }
        try {
            URL consulUrl = new URL(url);
            return consulUrl.getPort() < 0 ? new ConsulClient(consulUrl.getHost()) : new ConsulClient(consulUrl.getHost(), consulUrl.getPort());
        } catch (MalformedURLException e) {
            throw new ShenyuException("sync.consul.url formatter is not incorrect.");
        }
    }
}
