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

package org.apache.shenyu.springboot.starter.client.springcloud;

import org.apache.shenyu.client.auto.config.ClientRegisterConfiguration;
import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.core.register.ClientInfoRefreshedEventListener;
import org.apache.shenyu.client.core.register.ClientRegisterConfig;
import org.apache.shenyu.client.core.register.ClientRegisterConfigImpl;
import org.apache.shenyu.client.core.register.extractor.ApiBeansExtractor;
import org.apache.shenyu.client.core.register.registrar.AbstractApiDocRegistrar;
import org.apache.shenyu.client.core.register.registrar.AbstractApiMetaRegistrar;
import org.apache.shenyu.client.core.register.registrar.HttpApiDocRegistrar;
import org.apache.shenyu.client.springcloud.register.SpringCloudApiBeansExtractor;
import org.apache.shenyu.client.springcloud.register.SpringCloudApiMetaRegister;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.register.common.config.ShenyuClientConfig;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.Environment;

@Configuration(proxyBeanMethods = false)
@ConditionalOnBean(ClientRegisterConfiguration.class)
public class ShenyuSpringCloudClientInfoRegisterConfiguration {
    
    public ShenyuSpringCloudClientInfoRegisterConfiguration() {
    }
    
    /**
     * ClientInfoRefreshedEventListener Bean.
     *
     * @param clientRegisterConfig clientRegisterConfig
     * @param publisher            publisher
     * @return clientInfoRefreshedEventListener
     */
    @Bean
    public ClientInfoRefreshedEventListener clientInfoEventListener(final ClientRegisterConfig clientRegisterConfig,
                                                                    final ShenyuClientRegisterEventPublisher publisher) {
        return new ClientInfoRefreshedEventListener(clientRegisterConfig, publisher);
    }
    
    /**
     * ApiBeansExtractor Bean.
     *
     * @return apiBeansExtractor
     */
    @Bean
    @ConditionalOnMissingBean
    public ApiBeansExtractor apiBeansExtractor() {
        return new SpringCloudApiBeansExtractor();
    }
    
    /**
     * Builds ApiMetaRegistrar Bean.
     *
     * @param publisher            publisher
     * @param clientRegisterConfig clientRegisterConfig
     * @return ApiMetaRegistrar
     */
    @Bean(name = "ApiMetaRegistrar")
    @ConditionalOnProperty(value = "shenyu.register.api.meta.enabled", matchIfMissing = true, havingValue = "true")
    public AbstractApiMetaRegistrar buildApiMetaRegistrar(final ShenyuClientRegisterEventPublisher publisher,
                                                          final ClientRegisterConfig clientRegisterConfig) {
        
        return new SpringCloudApiMetaRegister(publisher, clientRegisterConfig);
    }
    
    /**
     * Builds ApiDocRegistrar  Bean.
     *
     * @param publisher            publisher
     * @param clientRegisterConfig clientRegisterConfig
     * @return ApiDocRegistrar
     */
    @Bean(name = "ApiDocRegistrar")
    @ConditionalOnProperty(value = "shenyu.register.api.data.enabled", matchIfMissing = true, havingValue = "true")
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
        return new ClientRegisterConfigImpl(shenyuClientConfig, RpcTypeEnum.SPRING_CLOUD, applicationContext, env);
    }
}
