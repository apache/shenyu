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

package org.apache.shenyu.springboot.starter.sdk.springcloud;

import org.apache.shenyu.common.utils.VersionUtils;
import org.apache.shenyu.registry.api.ShenyuInstanceRegisterRepository;
import org.apache.shenyu.registry.api.config.RegisterConfig;
import org.apache.shenyu.registry.core.ShenyuInstanceRegisterRepositoryFactory;
import org.apache.shenyu.sdk.springcloud.EnableShenyuClients;
import org.apache.shenyu.sdk.springcloud.ShenyuDiscoveryClient;
import org.apache.shenyu.sdk.springcloud.ShenyuServiceInstanceLoadBalancer;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.autoconfigure.AutoConfigureAfter;
import org.springframework.boot.autoconfigure.AutoConfigureBefore;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnExpression;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.cloud.loadbalancer.annotation.LoadBalancerClient;
import org.springframework.cloud.loadbalancer.config.LoadBalancerAutoConfiguration;
import org.springframework.cloud.loadbalancer.core.ReactorServiceInstanceLoadBalancer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * The type Shenyu sdk autoConfiguration.
 */
@Configuration(proxyBeanMethods = false)
@ConditionalOnProperty(value = "shenyu.sdk.enabled", havingValue = "true", matchIfMissing = true)
public class ShenyuSdkAutoConfiguration {

    static {
        VersionUtils.checkDuplicate(ShenyuSdkAutoConfiguration.class);
    }

    /**
     * ShenYu Instance Register Repository.
     *
     * @param config the config
     * @return ShenYu Instance Register Repository
     */
    @Bean
    @ConditionalOnExpression("!\"local\".equals(\"${shenyu.sdk.registerType}\")")
    public ShenyuInstanceRegisterRepository shenyuInstanceRegisterRepository(final RegisterConfig config) {
        return ShenyuInstanceRegisterRepositoryFactory.newAndInitInstance(config);
    }

    /**
     * shenyu config.
     *
     * @return the shenyu config
     */
    @Bean
    @ConfigurationProperties(prefix = "shenyu.sdk")
    public RegisterConfig shenyuConfig() {
        return new RegisterConfig();
    }

    @Bean
    @ConditionalOnProperty(value = "shenyu.sdk.enabled", havingValue = "true", matchIfMissing = true)
    public ShenyuDiscoveryClient shenyuDiscoveryClient(final RegisterConfig registerConfig) {
        return new ShenyuDiscoveryClient(registerConfig);
    }

    @Bean
    @ConditionalOnBean(ShenyuInstanceRegisterRepository.class)
    public ShenyuDiscoveryClient shenyuDiscoveryClient(final ShenyuInstanceRegisterRepository registerRepository) {
        return new ShenyuDiscoveryClient(registerRepository);
    }

    @Bean
    @ConditionalOnBean(ShenyuDiscoveryClient.class)
    public ReactorServiceInstanceLoadBalancer loadBalancer(@Qualifier("shenyuDiscoveryClient") final ShenyuDiscoveryClient shenyuDiscoveryClient) {
        return new ShenyuServiceInstanceLoadBalancer(shenyuDiscoveryClient);
    }
}
