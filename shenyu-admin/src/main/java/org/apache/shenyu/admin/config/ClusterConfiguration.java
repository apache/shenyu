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
import org.apache.shenyu.admin.mode.ShenyuRunningModeService;
import org.apache.shenyu.admin.mode.cluster.filter.ClusterForwardFilter;
import org.apache.shenyu.admin.mode.cluster.impl.jdbc.ClusterSelectMasterServiceJdbcImpl;
import org.apache.shenyu.admin.mode.cluster.impl.zookeeper.ClusterSelectMasterServiceZookeeperImpl;
import org.apache.shenyu.admin.mode.cluster.impl.zookeeper.ClusterZookeeperClient;
import org.apache.shenyu.admin.mode.cluster.impl.zookeeper.ClusterZookeeperConfig;
import org.apache.shenyu.admin.mode.cluster.mapper.ClusterMasterMapper;
import org.apache.shenyu.admin.mode.cluster.service.ClusterSelectMasterService;
import org.apache.shenyu.admin.mode.cluster.service.ShenyuClusterService;
import org.apache.shenyu.admin.service.impl.UpstreamCheckService;
import org.apache.shenyu.admin.service.manager.LoadServiceDocEntry;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.integration.jdbc.lock.JdbcLockRegistry;
import org.springframework.integration.zookeeper.lock.ZookeeperLockRegistry;

import java.util.Objects;

/**
 * The type Cluster configuration.
 */
@Configuration(proxyBeanMethods = false)
@EnableConfigurationProperties({ClusterProperties.class, ClusterZookeeperProperties.class})
public class ClusterConfiguration {
    
    private static final Logger LOGGER = LoggerFactory.getLogger(ClusterConfiguration.class);
    
    /**
     * Shenyu running mode cluster service.
     *
     * @param shenyuClusterSelectMasterService shenyu cluster select master service
     * @param upstreamCheckService upstream check service
     * @param loadServiceDocEntry load service doc entry
     * @param clusterProperties cluster properties
     * @return Shenyu cluster service
     */
    @Bean(destroyMethod = "shutdown")
    @ConditionalOnProperty(value = {"shenyu.cluster.enabled"}, havingValue = "true", matchIfMissing = true)
    @ConditionalOnMissingBean
    public ShenyuRunningModeService shenyuRunningModeService(final ClusterSelectMasterService shenyuClusterSelectMasterService,
                                                             final UpstreamCheckService upstreamCheckService,
                                                             final LoadServiceDocEntry loadServiceDocEntry,
                                                             final ClusterProperties clusterProperties) {
        LOGGER.info("starting in cluster mode ...");
        return new ShenyuClusterService(shenyuClusterSelectMasterService,
                upstreamCheckService,
                loadServiceDocEntry,
                clusterProperties
        );
    }
    
    /**
     * Shenyu cluster forward filter.
     *
     * @return the Shenyu cluster forward filter
     */
    @Bean
    @ConditionalOnProperty(value = {"shenyu.cluster.enabled"}, havingValue = "true", matchIfMissing = false)
    public ClusterForwardFilter clusterForwardFilter() {
        return new ClusterForwardFilter();
    }
    
    /**
     * Shenyu select master service.
     *
     * @param clusterProperties the cluster properties
     * @param jdbcLockRegistry the jdbc lock registry
     * @param clusterMasterMapper the cluster master mapper
     * @return the shenyu select master service
     */
    @Bean
    @ConditionalOnProperty(value = {"shenyu.cluster.select-type"}, havingValue = "jdbc", matchIfMissing = true)
    public ClusterSelectMasterService clusterSelectMasterJdbcService(final ClusterProperties clusterProperties,
                                                                     final JdbcLockRegistry jdbcLockRegistry,
                                                                     final ClusterMasterMapper clusterMasterMapper) {
        return new ClusterSelectMasterServiceJdbcImpl(clusterProperties, jdbcLockRegistry, clusterMasterMapper);
    }
    
    /**
     * Shenyu cluster select master service.
     *
     * @param clusterProperties the cluster properties
     * @param zookeeperLockRegistry the zookeeper lock registry
     * @return the shenyu cluster select master service
     */
    @Bean
    @ConditionalOnProperty(value = {"shenyu.cluster.select-type"}, havingValue = "zookeeper", matchIfMissing = false)
    public ClusterSelectMasterService clusterSelectMasterZookeeperService(final ClusterProperties clusterProperties,
                                                                          final ZookeeperLockRegistry zookeeperLockRegistry) {
        return new ClusterSelectMasterServiceZookeeperImpl(clusterProperties, zookeeperLockRegistry);
    }
    
    /**
     * register zkClient in spring ioc.
     *
     * @param clusterZookeeperProperties the zookeeper configuration
     * @return ClusterZookeeperClient {@linkplain ClusterZookeeperClient}
     */
    @Bean
    @ConditionalOnProperty(value = {"shenyu.cluster.select-type"}, havingValue = "zookeeper", matchIfMissing = false)
    public ClusterZookeeperClient clusterZookeeperClient(final ClusterZookeeperProperties clusterZookeeperProperties) {
        int sessionTimeout = Objects.isNull(clusterZookeeperProperties.getSessionTimeout()) ? 3000 : clusterZookeeperProperties.getSessionTimeout();
        int connectionTimeout = Objects.isNull(clusterZookeeperProperties.getConnectionTimeout()) ? 3000 : clusterZookeeperProperties.getConnectionTimeout();
        ClusterZookeeperConfig zkConfig = new ClusterZookeeperConfig(clusterZookeeperProperties.getUrl());
        zkConfig.setSessionTimeoutMilliseconds(sessionTimeout)
                .setConnectionTimeoutMilliseconds(connectionTimeout);
        ClusterZookeeperClient client = new ClusterZookeeperClient(zkConfig);
        client.start();
        return client;
    }
    
}
