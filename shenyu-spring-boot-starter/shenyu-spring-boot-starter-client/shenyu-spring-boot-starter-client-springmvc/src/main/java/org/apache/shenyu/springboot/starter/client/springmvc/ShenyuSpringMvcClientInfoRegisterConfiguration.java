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
import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.core.register.ClientRegisterConfig;
import org.apache.shenyu.client.core.register.matcher.ExtractorProcessor;
import org.apache.shenyu.client.core.register.registrar.AbstractApiDocRegistrar;
import org.apache.shenyu.client.core.register.registrar.AbstractApiMetaRegistrar;
import org.apache.shenyu.client.core.register.registrar.HttpApiDocRegistrar;
import org.apache.shenyu.client.springmvc.proceeor.register.ShenyuSpringMvcClientProcessorImpl;
import org.apache.shenyu.client.springmvc.register.SpringMvcApiBeansExtractor;
import org.apache.shenyu.client.springmvc.register.SpringMvcApiMetaRegister;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

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
    public AbstractApiDocRegistrar buildApiDocRegistrar(final ShenyuClientRegisterEventPublisher publisher,
                                                        final ClientRegisterConfig clientRegisterConfig) {
        return new HttpApiDocRegistrar(publisher, clientRegisterConfig);
    }

}
