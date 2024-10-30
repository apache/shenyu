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

package org.apache.shenyu.springboot.starter.client.springmvc;

import jakarta.annotation.Resource;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.client.core.constant.ShenyuClientConstants;
import org.apache.shenyu.client.core.register.ClientDiscoveryConfigRefreshedEventListener;
import org.apache.shenyu.client.core.register.ClientRegisterConfig;
import org.apache.shenyu.client.core.register.InstanceRegisterListener;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.register.client.http.HttpClientRegisterRepository;
import org.apache.shenyu.register.common.config.ShenyuClientConfig;
import org.apache.shenyu.register.common.config.ShenyuDiscoveryConfig;
import org.apache.shenyu.springboot.starter.client.common.config.ShenyuClientCommonBeanConfiguration;
import org.springframework.boot.autoconfigure.ImportAutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.core.env.Environment;

import java.util.Optional;

@Configuration
@ConditionalOnBean(ClientRegisterConfig.class)
@ImportAutoConfiguration(ShenyuClientCommonBeanConfiguration.class)
public class ShenyuSpringMvcDiscoveryConfiguration {

    @Resource
    private Environment environment;

    /**
     * clientDiscoveryConfigRefreshedEventListener Bean.
     *
     * @param shenyuDiscoveryConfig        shenyuDiscoveryConfig
     * @param httpClientRegisterRepository httpClientRegisterRepository
     * @param clientRegisterConfig         clientRegisterConfig
     * @param shenyuClientConfig           shenyuClientConfig
     * @return ClientDiscoveryConfigRefreshedEventListener
     */
    @Bean("SpringMvcClientDiscoveryConfigRefreshedEventListener")
    @ConditionalOnProperty(prefix = "shenyu.discovery", name = "serverList", matchIfMissing = false)
    @ConditionalOnBean(ShenyuDiscoveryConfig.class)
    public ClientDiscoveryConfigRefreshedEventListener clientDiscoveryConfigRefreshedEventListener(final ShenyuDiscoveryConfig shenyuDiscoveryConfig,
                                                                                                   final HttpClientRegisterRepository httpClientRegisterRepository,
                                                                                                   final ClientRegisterConfig clientRegisterConfig,
                                                                                                   final ShenyuClientConfig shenyuClientConfig) {
        return new ClientDiscoveryConfigRefreshedEventListener(shenyuDiscoveryConfig, httpClientRegisterRepository, clientRegisterConfig, PluginEnum.DIVIDE, shenyuClientConfig);
    }

    /**
     * InstanceRegisterListener.
     *
     * @param clientRegisterConfig  clientRegisterConfig
     * @param shenyuDiscoveryConfig shenyuDiscoveryConfig
     * @param shenyuClientConfig    shenyuClientConfig
     * @return InstanceRegisterListener
     */
    @Bean("springmvcInstanceRegisterListener")
    @ConditionalOnProperty(prefix = "shenyu.discovery", name = "register", matchIfMissing = false)
    @ConditionalOnBean(ShenyuDiscoveryConfig.class)
    @Primary
    public InstanceRegisterListener instanceRegisterListener(final ClientRegisterConfig clientRegisterConfig,
                                                             final ShenyuDiscoveryConfig shenyuDiscoveryConfig,
                                                             final ShenyuClientConfig shenyuClientConfig) {
        DiscoveryUpstreamData discoveryUpstreamData = new DiscoveryUpstreamData();
        discoveryUpstreamData.setUrl(clientRegisterConfig.getHost() + ":" + clientRegisterConfig.getPort());
        discoveryUpstreamData.setStatus(0);
        discoveryUpstreamData.setWeight(50);
        discoveryUpstreamData.setProtocol(Optional.ofNullable(shenyuDiscoveryConfig.getProtocol()).orElse(ShenyuClientConstants.HTTP));
        discoveryUpstreamData.setNamespaceId(shenyuClientConfig.getNamespace());
        final String appName = environment.getProperty("spring.application.name");
        if (StringUtils.isEmpty(shenyuDiscoveryConfig.getProps().getProperty("name")) && appName != null) {
            shenyuDiscoveryConfig.getProps().put("name", appName);
        }
        return new InstanceRegisterListener(discoveryUpstreamData, shenyuDiscoveryConfig);
    }

}
