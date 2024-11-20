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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.config.properties.ClusterProperties;
import org.apache.shenyu.admin.config.properties.ClusterZookeeperProperties;
import org.apache.shenyu.admin.mode.cluster.impl.jdbc.ClusterSelectMasterServiceJdbcImpl;
import org.apache.shenyu.admin.mode.cluster.impl.jdbc.mapper.ClusterMasterMapper;
import org.apache.shenyu.admin.mode.cluster.service.ClusterSelectMasterService;
import org.apache.shenyu.common.utils.IpUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.integration.jdbc.lock.DefaultLockRepository;
import org.springframework.integration.jdbc.lock.JdbcLockRegistry;
import org.springframework.integration.jdbc.lock.LockRepository;

import javax.sql.DataSource;
import java.util.concurrent.TimeUnit;

/**
 * The type Cluster jdbc configuration.
 */
@Configuration(proxyBeanMethods = false)
@EnableConfigurationProperties({ClusterProperties.class, ClusterZookeeperProperties.class})
@ConditionalOnProperty(value = {"shenyu.cluster.type"}, havingValue = "jdbc", matchIfMissing = true)
public class ClusterJdbcConfiguration {
    
    private static final Logger LOGGER = LoggerFactory.getLogger(ClusterJdbcConfiguration.class);
    
    @Value("${server.servlet.context-path:}")
    private String contextPath;
    
    @Value("${server.port:}")
    private String port;
    
    /**
     * Shenyu Admin distributed lock by spring-integration-jdbc.
     *
     * @param dataSource the dataSource
     * @param clusterProperties the cluster properties
     * @return  defaultLockRepository
     */
    @Bean
    public DefaultLockRepository defaultLockRepository(final DataSource dataSource,
                                                       final ClusterProperties clusterProperties) {
        final String host = IpUtils.getHost();
        String fullPath = host + ":" + port;
        if (StringUtils.isNoneBlank(contextPath)) {
            fullPath += contextPath;
        }
        DefaultLockRepository defaultLockRepository = new DefaultLockRepository(dataSource, fullPath);
        defaultLockRepository.setPrefix("SHENYU_");
        // set lock ttl
        long millis = TimeUnit.SECONDS.toMillis(clusterProperties.getLockTtl());
        defaultLockRepository.setTimeToLive(Long.valueOf(millis).intValue());
        return defaultLockRepository;
    }
    
    /**
     * Shenyu Admin distributed lock by spring-integration-jdbc.
     *
     * @param lockRepository the lockRepository
     * @return the shenyu Admin register repository
     */
    @Bean
    public JdbcLockRegistry jdbcLockRegistry(final LockRepository lockRepository) {
        return new JdbcLockRegistry(lockRepository);
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
    public ClusterSelectMasterService clusterSelectMasterJdbcService(final ClusterProperties clusterProperties,
                                                                     final JdbcLockRegistry jdbcLockRegistry,
                                                                     final ClusterMasterMapper clusterMasterMapper) {
        return new ClusterSelectMasterServiceJdbcImpl(clusterProperties, jdbcLockRegistry, clusterMasterMapper);
    }
    
}
