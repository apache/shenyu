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
import lombok.extern.slf4j.Slf4j;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.api.SyncDataService;
import org.apache.shenyu.sync.data.consul.ConsulSyncDataService;
import org.apache.shenyu.sync.data.consul.config.ConsulConfig;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.Collections;
import java.util.List;

/**
 * Consul sync data configuration for spring boot.
 */
@Configuration
@ConditionalOnClass(ConsulSyncDataConfiguration.class)
@ConditionalOnProperty(prefix = "shenyu.sync.consul", name = "url")
@Slf4j
public class ConsulSyncDataConfiguration {

    /**
     * Sync data service.
     *
     * @param consulClient     the consul client
     * @param consulConfig     the consul config
     * @param pluginSubscriber the plugin subscriber
     * @param metaSubscribers   the meta subscribers
     * @param authSubscribers   the auth subscribers
     * @return the sync data service
     */
    @Bean
    public SyncDataService syncDataService(final ObjectProvider<ConsulClient> consulClient,
                                           final ObjectProvider<ConsulConfig> consulConfig,
                                           final ObjectProvider<PluginDataSubscriber> pluginSubscriber,
                                           final ObjectProvider<List<MetaDataSubscriber>> metaSubscribers,
                                           final ObjectProvider<List<AuthDataSubscriber>> authSubscribers) {
        log.info("you use consul sync shenyu data.......");
        return new ConsulSyncDataService(consulClient.getIfAvailable(), consulConfig.getIfAvailable(), pluginSubscriber.getIfAvailable(),
                metaSubscribers.getIfAvailable(Collections::emptyList), authSubscribers.getIfAvailable(Collections::emptyList));
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
        return new ConsulClient(consulConfig.getUrl());
    }
}
