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
import org.springframework.http.client.SimpleClientHttpRequestFactory;
import org.springframework.web.client.RestTemplate;

/**
 * The type Cluster configuration.
 */
@Configuration(proxyBeanMethods = false)
@EnableConfigurationProperties({ClusterProperties.class, ClusterZookeeperProperties.class})
@ConditionalOnProperty(value = {"shenyu.cluster.enabled"}, havingValue = "true", matchIfMissing = false)
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
     * @param clusterProperties cluster properties
     * @return the Shenyu cluster forward filter
     */
    @Bean
    public ClusterForwardFilter clusterForwardFilter(final ClusterProperties clusterProperties) {
        SimpleClientHttpRequestFactory factory = new SimpleClientHttpRequestFactory();
        factory.setConnectTimeout(clusterProperties.getConnectionTimeout());
        factory.setReadTimeout(clusterProperties.getReadTimeout());
        return new ClusterForwardFilter(new RestTemplate(factory));
    }
    
}
