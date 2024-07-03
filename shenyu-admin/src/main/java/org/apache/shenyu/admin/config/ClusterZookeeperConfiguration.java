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

import org.apache.shenyu.admin.config.properties.ClusterProperties;
import org.apache.shenyu.admin.config.properties.ClusterZookeeperProperties;
import org.apache.shenyu.admin.mode.cluster.impl.zookeeper.ClusterSelectMasterServiceZookeeperImpl;
import org.apache.shenyu.admin.mode.cluster.impl.zookeeper.ClusterZookeeperClient;
import org.apache.shenyu.admin.mode.cluster.impl.zookeeper.ClusterZookeeperConfig;
import org.apache.shenyu.admin.mode.cluster.service.ClusterSelectMasterService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.integration.zookeeper.lock.ZookeeperLockRegistry;

/**
 * The type Cluster zookeeper configuration.
 */
@Configuration(proxyBeanMethods = false)
@EnableConfigurationProperties({ClusterProperties.class, ClusterZookeeperProperties.class})
@ConditionalOnProperty(value = {"shenyu.cluster.type"}, havingValue = "zookeeper", matchIfMissing = false)
public class ClusterZookeeperConfiguration {
    
    private static final Logger LOGGER = LoggerFactory.getLogger(ClusterZookeeperConfiguration.class);
    
    private static final String LOCK_PATH = "/shenyu-cluster-lock";
    
    /**
     * Shenyu Admin distributed lock by spring-integration-zookeeper.
     *
     * @param clusterZookeeperClient the cluster zookeeper client
     * @return the shenyu Admin zookeeper lock registry
     */
    @Bean
    public ZookeeperLockRegistry zookeeperLockRegistry(final ClusterZookeeperClient clusterZookeeperClient) {
        return new ZookeeperLockRegistry(clusterZookeeperClient.getClient(), LOCK_PATH);
    }
    
    /**
     * Shenyu cluster select master service.
     *
     * @param clusterProperties the cluster properties
     * @param zookeeperLockRegistry the zookeeper lock registry
     * @param clusterZookeeperClient cluster zookeeper client
     * @return the shenyu cluster select master service
     */
    @Bean
    public ClusterSelectMasterService clusterSelectMasterZookeeperService(final ClusterProperties clusterProperties,
                                                                          final ZookeeperLockRegistry zookeeperLockRegistry,
                                                                          final ClusterZookeeperClient clusterZookeeperClient) {
        return new ClusterSelectMasterServiceZookeeperImpl(clusterProperties, zookeeperLockRegistry, clusterZookeeperClient);
    }
    
    /**
     * register zkClient in spring ioc.
     *
     * @param clusterZookeeperProperties the zookeeper configuration
     * @return ClusterZookeeperClient {@linkplain ClusterZookeeperClient}
     */
    @Bean
    public ClusterZookeeperClient clusterZookeeperClient(final ClusterZookeeperProperties clusterZookeeperProperties) {
        int sessionTimeout = clusterZookeeperProperties.getSessionTimeout();
        int connectionTimeout = clusterZookeeperProperties.getConnectionTimeout();
        ClusterZookeeperConfig zkConfig = new ClusterZookeeperConfig(clusterZookeeperProperties.getUrl());
        zkConfig.setSessionTimeoutMilliseconds(sessionTimeout)
                .setConnectionTimeoutMilliseconds(connectionTimeout);
        ClusterZookeeperClient client = new ClusterZookeeperClient(zkConfig);
        client.start();
        return client;
    }
    
}
