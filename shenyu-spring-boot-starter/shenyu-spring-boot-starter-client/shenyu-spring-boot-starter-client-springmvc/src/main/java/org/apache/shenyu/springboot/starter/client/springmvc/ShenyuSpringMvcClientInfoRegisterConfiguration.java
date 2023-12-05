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

import org.apache.shenyu.client.auto.config.ClientRegisterConfiguration;
import org.apache.shenyu.client.core.constant.ShenyuClientConstants;
import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.core.register.ClientDiscoveryConfigRefreshedEventListener;
import org.apache.shenyu.client.core.register.ClientRegisterConfig;
import org.apache.shenyu.client.core.register.ClientRegisterConfigImpl;
import org.apache.shenyu.client.core.register.InstanceRegisterListener;
import org.apache.shenyu.client.core.register.matcher.ExtractorProcessor;
import org.apache.shenyu.client.core.register.registrar.AbstractApiDocRegistrar;
import org.apache.shenyu.client.core.register.registrar.AbstractApiMetaRegistrar;
import org.apache.shenyu.client.core.register.registrar.HttpApiDocRegistrar;
import org.apache.shenyu.client.springmvc.proceeor.register.ShenyuSpringMvcClientProcessorImpl;
import org.apache.shenyu.client.springmvc.register.SpringMvcApiBeansExtractor;
import org.apache.shenyu.client.springmvc.register.SpringMvcApiMetaRegister;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.register.client.http.HttpClientRegisterRepository;
import org.apache.shenyu.register.common.config.ShenyuClientConfig;
import org.apache.shenyu.register.common.config.ShenyuDiscoveryConfig;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.Environment;

import java.util.List;

@Configuration(proxyBeanMethods = false)
@ConditionalOnBean(ClientRegisterConfiguration.class)
public class ShenyuSpringMvcClientInfoRegisterConfiguration {

    public ShenyuSpringMvcClientInfoRegisterConfiguration() {
    }

    /**
     * ApiBeansExtractor Bean.
     *
     * @param extractorProcessorList extractorProcessorList
     * @return apiBeansExtractor
     */
    @Bean
    @ConditionalOnMissingBean
    public SpringMvcApiBeansExtractor springMvcApiBeansExtractor(final List<ExtractorProcessor> extractorProcessorList) {
        final SpringMvcApiBeansExtractor extractor = SpringMvcApiBeansExtractor.buildDefaultSpringMvcApiBeansExtractor();
        for (ExtractorProcessor processor : extractorProcessorList) {
            extractor.addExtractorProcessor(processor);
        }
        return extractor;
    }

    /**
     * shenyuSpringMvcClientProcessor.
     *
     * @return shenyuSpringMvcClientProcessor
     */
    @Bean
    public ShenyuSpringMvcClientProcessorImpl shenyuSpringMvcClientProcessor() {
        return new ShenyuSpringMvcClientProcessorImpl();
    }

    /**
     * Builds ApiMetaRegistrar Bean.
     *
     * @param publisher            publisher
     * @param clientRegisterConfig clientRegisterConfig
     * @return ApiMetaRegistrar
     */
//    @Bean(name = "ApiMetaRegistrar")
//    @ConditionalOnProperty(value = "shenyu.register.api.meta.enabled", matchIfMissing = true, havingValue = "true")
    public AbstractApiMetaRegistrar buildApiMetaRegistrar(final ShenyuClientRegisterEventPublisher publisher,
                                                          final ClientRegisterConfig clientRegisterConfig) {

        return new SpringMvcApiMetaRegister(publisher, clientRegisterConfig);
    }

    /**
     * Builds ApiDocRegistrar  Bean.
     *
     * @param publisher            publisher
     * @param clientRegisterConfig clientRegisterConfig
     * @return ApiDocRegistrar
     */
//    @Bean(name = "ApiDocRegistrar")
//    @ConditionalOnProperty(value = "shenyu.register.api.data.enabled", matchIfMissing = true, havingValue = "true")
    public AbstractApiDocRegistrar buildApiDocRegistrar(final ShenyuClientRegisterEventPublisher publisher,
                                                        final ClientRegisterConfig clientRegisterConfig) {
        return new HttpApiDocRegistrar(publisher, clientRegisterConfig);
    }

    /**
     * ClientRegisterConfig Bean.
     *
     * @param shenyuClientConfig shenyuClientConfig
     * @param applicationContext applicationContext
     * @param env                env
     * @return clientRegisterConfig
     */
    @Bean
    public ClientRegisterConfig clientRegisterConfig(final ShenyuClientConfig shenyuClientConfig,
                                                     final ApplicationContext applicationContext,
                                                     final Environment env) {
        return new ClientRegisterConfigImpl(shenyuClientConfig, RpcTypeEnum.HTTP, applicationContext, env);
    }

    /**
     * InstanceRegisterListener.
     *
     * @param clientRegisterConfig  clientRegisterConfig
     * @param shenyuDiscoveryConfig shenyuDiscoveryConfig
     * @return InstanceRegisterListener
     */
    @Bean("springmvcInstanceRegisterListener")
    @ConditionalOnBean(ShenyuDiscoveryConfig.class)
    @ConditionalOnMissingBean(name = "websocketInstanceRegisterListener")
    public InstanceRegisterListener instanceRegisterListener(final ClientRegisterConfig clientRegisterConfig, final ShenyuDiscoveryConfig shenyuDiscoveryConfig) {
        DiscoveryUpstreamData discoveryUpstreamData = new DiscoveryUpstreamData();
        discoveryUpstreamData.setUrl(clientRegisterConfig.getHost() + ":" + clientRegisterConfig.getPort());
        discoveryUpstreamData.setStatus(0);
        String weight = shenyuDiscoveryConfig.getProps().getOrDefault("discoveryUpstream.weight", "10").toString();
        discoveryUpstreamData.setWeight(Integer.parseInt(weight));
        discoveryUpstreamData.setProtocol(shenyuDiscoveryConfig.getProps().getOrDefault("discoveryUpstream.protocol", ShenyuClientConstants.HTTP).toString());
        return new InstanceRegisterListener(discoveryUpstreamData, shenyuDiscoveryConfig);
    }

    /**
     * clientDiscoveryConfigRefreshedEventListener Bean.
     *
     * @param shenyuDiscoveryConfig        shenyuDiscoveryConfig
     * @param httpClientRegisterRepository httpClientRegisterRepository
     * @param clientRegisterConfig         clientRegisterConfig
     * @return ClientDiscoveryConfigRefreshedEventListener
     */
    @Bean
    @ConditionalOnProperty(prefix = "shenyu.discovery", name = "serverList", matchIfMissing = false)
    @ConditionalOnBean(ShenyuDiscoveryConfig.class)
    public ClientDiscoveryConfigRefreshedEventListener clientDiscoveryConfigRefreshedEventListener(final ShenyuDiscoveryConfig shenyuDiscoveryConfig,
                                                                                                   final HttpClientRegisterRepository httpClientRegisterRepository,
                                                                                                   final ClientRegisterConfig clientRegisterConfig) {
        return new ClientDiscoveryConfigRefreshedEventListener(shenyuDiscoveryConfig, httpClientRegisterRepository, clientRegisterConfig);
    }

}
