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

package org.apache.shenyu.client.auto.config;

import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.core.register.ClientApiRefreshedEventListener;
import org.apache.shenyu.client.core.register.ClientRegisterConfig;
import org.apache.shenyu.client.core.register.extractor.MultiClientApiBeansExtractorImpl;
import org.apache.shenyu.client.core.register.extractor.RpcApiBeansExtractor;
import org.apache.shenyu.client.core.register.matcher.ApiDocProcessorImpl;
import org.apache.shenyu.client.core.register.matcher.ApiRegisterProcessor;
import org.apache.shenyu.client.core.register.registrar.ApiDocRegistrarImpl;
import org.apache.shenyu.client.core.register.registrar.ApiRegistrar;
import org.apache.shenyu.client.core.register.registrar.MateDataApiRegistrarImpl;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.List;

@Configuration(proxyBeanMethods = false)
public class ClientRegisterConfiguration {
    
    /**
     * Gets ContextApiRefreshedEventListener Bean.
     *
     * @param rpcApiBeansExtractors rpcApiBeansExtractors
     * @param apiRegistrars         apiRegistrars
     * @return contextApiRefreshedEventListener
     */
    @Bean
    public ClientApiRefreshedEventListener apiListener(final List<RpcApiBeansExtractor> rpcApiBeansExtractors,
                                                       final List<ApiRegistrar> apiRegistrars) {
        return new ClientApiRefreshedEventListener(apiRegistrars, new MultiClientApiBeansExtractorImpl(rpcApiBeansExtractors));
    }
    
    /**
     * mate date register.
     *
     * @param clientRegisterConfig clientRegisterConfig
     * @param processor            processor
     * @return register
     */
    @Bean
    public MateDataApiRegistrarImpl mateDataApiRegistrarImpl(final ClientRegisterConfig clientRegisterConfig, final List<ApiRegisterProcessor> processor) {
        final MateDataApiRegistrarImpl apiRegistrarImpl = new MateDataApiRegistrarImpl(clientRegisterConfig);
        processor.forEach(apiRegistrarImpl::addApiProcessor);
        return apiRegistrarImpl;
    }

    /**
     * api doc register.
     *
     * @param clientRegisterConfig clientRegisterConfig
     * @param processor            processor
     * @return register
     */
    @Bean
    public ApiDocRegistrarImpl apiDocRegistrarImpl(final ClientRegisterConfig clientRegisterConfig, final List<ApiRegisterProcessor> processor) {
        final ApiDocRegistrarImpl apiRegistrarImpl = new ApiDocRegistrarImpl(clientRegisterConfig);
        processor.forEach(apiRegistrarImpl::addApiProcessor);
        return apiRegistrarImpl;
    }

    
    /**
     * apiDocProcessor.
     *
     * @return apiDocProcessor
     */
    @Bean
    @ConditionalOnMissingBean
    public ApiDocProcessorImpl apiDocProcessor() {
        return new ApiDocProcessorImpl();
    }
    
    /**
     * Gets ShenyuClientRegisterEventPublisher Bean that is initialized .
     *
     * @param shenyuClientRegisterRepository shenyuClientRegisterRepository.
     * @return publisher
     */
    @Bean
    public ShenyuClientRegisterEventPublisher publisher(final ShenyuClientRegisterRepository shenyuClientRegisterRepository) {
        ShenyuClientRegisterEventPublisher publisher = ShenyuClientRegisterEventPublisher.getInstance();
        publisher.start(shenyuClientRegisterRepository);
        return publisher;
    }
}
