/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.web.config;

import org.dromara.soul.web.cache.ZookeeperCacheManager;
import org.dromara.soul.web.plugin.SoulPlugin;
import org.dromara.soul.web.plugin.function.SpringCloudPlugin;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.AutoConfigureAfter;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.cloud.client.discovery.EnableDiscoveryClient;
import org.springframework.cloud.client.loadbalancer.LoadBalancerClient;
import org.springframework.cloud.netflix.ribbon.RibbonAutoConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.reactive.DispatcherHandler;

/**
 * SpringCloudAutoConfiguration.
 *
 * @author xiaoyu(Myth)
 */
@Configuration
@ConditionalOnClass({LoadBalancerClient.class, RibbonAutoConfiguration.class, DispatcherHandler.class})
@AutoConfigureAfter(RibbonAutoConfiguration.class)
@ConditionalOnProperty(prefix = "eureka.client", name = "serviceUrl")
@EnableDiscoveryClient
public class SpringCloudAutoConfiguration {

    private final LoadBalancerClient loadBalancerClient;

    private final ZookeeperCacheManager zookeeperCacheManager;

    @Autowired(required = false)
    public SpringCloudAutoConfiguration(final LoadBalancerClient loadBalancerClient, final ZookeeperCacheManager zookeeperCacheManager) {
        this.loadBalancerClient = loadBalancerClient;
        this.zookeeperCacheManager = zookeeperCacheManager;
    }

    /**
     * init springCloud plugin.
     *
     * @return {@linkplain SpringCloudPlugin}
     */
    @Bean
    public SoulPlugin springCloudPlugin() {
        return new SpringCloudPlugin(zookeeperCacheManager, loadBalancerClient);
    }
}
