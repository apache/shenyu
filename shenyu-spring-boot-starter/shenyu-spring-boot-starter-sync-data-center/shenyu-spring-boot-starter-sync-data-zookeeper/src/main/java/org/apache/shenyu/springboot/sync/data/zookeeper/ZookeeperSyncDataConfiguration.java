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

package org.apache.shenyu.springboot.sync.data.zookeeper;

import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.api.SyncDataService;
import org.apache.shenyu.sync.data.zookeeper.ZookeeperClient;
import org.apache.shenyu.sync.data.zookeeper.ZookeeperConfig;
import org.apache.shenyu.sync.data.zookeeper.ZookeeperSyncDataService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.Collections;
import java.util.List;
import java.util.Objects;

/**
 * Zookeeper sync data configuration for spring boot.
 */
@Configuration
@ConditionalOnClass(ZookeeperSyncDataService.class)
@ConditionalOnProperty(prefix = "shenyu.sync.zookeeper", name = "url")
@EnableConfigurationProperties(ZookeeperProperties.class)
public class ZookeeperSyncDataConfiguration {

    private static final Logger LOGGER = LoggerFactory.getLogger(ZookeeperSyncDataConfiguration.class);

    /**
     * Sync data service.
     *
     * @param zookeeperClient  the zk client
     * @param pluginSubscriber the plugin subscriber
     * @param metaSubscribers  the meta subscribers
     * @param authSubscribers  the auth subscribers
     * @return the sync data service
     */
    @Bean
    public SyncDataService syncDataService(final ObjectProvider<ZookeeperClient> zookeeperClient, final ObjectProvider<PluginDataSubscriber> pluginSubscriber,
                                           final ObjectProvider<List<MetaDataSubscriber>> metaSubscribers, final ObjectProvider<List<AuthDataSubscriber>> authSubscribers) {
        LOGGER.info("you use zookeeper sync shenyu data.......");
        return new ZookeeperSyncDataService(zookeeperClient.getIfAvailable(), pluginSubscriber.getIfAvailable(),
                metaSubscribers.getIfAvailable(Collections::emptyList), authSubscribers.getIfAvailable(Collections::emptyList));
    }

    /**
     * register zkClient in spring ioc.
     *
     * @param zookeeperProps the zookeeper configuration
     * @return ZookeeperClient {@linkplain ZookeeperClient}
     */
    @Bean
    public ZookeeperClient zookeeperClient(final ZookeeperProperties zookeeperProps) {
        int sessionTimeout = Objects.isNull(zookeeperProps.getSessionTimeout()) ? 3000 : zookeeperProps.getSessionTimeout();
        int connectionTimeout = Objects.isNull(zookeeperProps.getConnectionTimeout()) ? 3000 : zookeeperProps.getConnectionTimeout();
        ZookeeperConfig zkConfig = new ZookeeperConfig(zookeeperProps.getUrl());
        zkConfig.setSessionTimeoutMilliseconds(sessionTimeout)
                .setConnectionTimeoutMilliseconds(connectionTimeout);
        ZookeeperClient client = new ZookeeperClient(zkConfig);
        client.start();
        return client;
    }
}
