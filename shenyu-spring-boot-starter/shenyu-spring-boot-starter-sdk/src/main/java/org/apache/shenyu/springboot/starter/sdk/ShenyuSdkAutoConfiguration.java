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

package org.apache.shenyu.springboot.starter.sdk;

import okhttp3.OkHttpClient;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.register.instance.api.ShenyuInstanceRegisterRepository;
import org.apache.shenyu.register.instance.core.ShenyuInstanceRegisterRepositoryFactory;
import org.apache.shenyu.sdk.core.client.ShenyuSdkClient;
import org.apache.shenyu.sdk.core.client.ShenyuSdkClientFactory;
import org.apache.shenyu.sdk.spring.annotation.CookieValueParameterProcessor;
import org.apache.shenyu.sdk.spring.annotation.PathVariableParameterProcessor;
import org.apache.shenyu.sdk.spring.annotation.RequestBodyParameterProcessor;
import org.apache.shenyu.sdk.spring.annotation.RequestHeaderParameterProcessor;
import org.apache.shenyu.sdk.spring.annotation.RequestParamParameterProcessor;
import org.apache.shenyu.sdk.spring.factory.AnnotatedParameterProcessor;
import org.apache.shenyu.sdk.spring.factory.Contract;
import org.apache.shenyu.sdk.spring.support.SpringMvcContract;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.beans.factory.support.BeanDefinitionRegistryPostProcessor;
import org.springframework.beans.factory.support.GenericBeanDefinition;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.ArrayList;
import java.util.List;

/**
 * The type Shenyu sdk autoConfiguration.
 */
@Configuration(proxyBeanMethods = false)
public class ShenyuSdkAutoConfiguration {
    
    /**
     * springMvcContract.
     *
     * @return {@link Contract}
     */
    @Bean
    @ConditionalOnMissingBean
    public Contract springMvcContract() {
        return new SpringMvcContract();
    }

    /**
     * okHttpShenyuSdkClient.
     *
     * @param config config
     * @param registerRepositoryObjectFactory registerRepositoryObjectFactory
     * @return {@link ShenyuSdkClient}
     */
    @Bean
    @ConditionalOnClass(OkHttpClient.class)
    @ConditionalOnProperty(name = "shenyu.sdk.props.clientType")
    public ShenyuSdkClient shenyuSdkClient(final ShenyuConfig config,
                                                 final ObjectProvider<ShenyuInstanceRegisterRepository> registerRepositoryObjectFactory) {
        final ShenyuSdkClient shenyuSdkClient = ShenyuSdkClientFactory.newInstance(config.getSdk().getProps().getProperty("clientType"));
        shenyuSdkClient.init(config.getSdk(), registerRepositoryObjectFactory);
        return shenyuSdkClient;
    }
    
    /**
     * ShenYu Instance Register Repository.
     *
     * @param config the config
     * @return ShenYu Instance Register Repository
     */
    @Bean
    @ConditionalOnProperty(name = "shenyu.sdk.registerType")
    public ShenyuInstanceRegisterRepository shenyuInstanceRegisterRepository(final ShenyuConfig config) {
        final String registerType = config.getSdk().getRegisterType();
        if ("local".equals(registerType)) {
            return null;
        }
        ShenyuInstanceRegisterRepository repository = ShenyuInstanceRegisterRepositoryFactory.newInstance(config.getSdk().getRegisterType());
        repository.init(config.getSdk());
        return repository;
    }

    /**
     * shenyu config.
     *
     * @return the shenyu config
     */
    @Bean
    @ConditionalOnMissingBean
    @ConfigurationProperties(prefix = "shenyu")
    public ShenyuConfig shenyuConfig() {
        return new ShenyuConfig();
    }
    
    /**
     * The type Parameter processor registry post processor.
     */
    @Configuration(proxyBeanMethods = false)
    public static class ParameterProcessorRegistryPostProcessor implements BeanDefinitionRegistryPostProcessor {
        @Override
        public void postProcessBeanDefinitionRegistry(final BeanDefinitionRegistry registry) throws BeansException {
            List<AnnotatedParameterProcessor> annotatedParameterProcessors = new ArrayList<>();
            annotatedParameterProcessors.add(new CookieValueParameterProcessor());
            annotatedParameterProcessors.add(new PathVariableParameterProcessor());
            annotatedParameterProcessors.add(new RequestHeaderParameterProcessor());
            annotatedParameterProcessors.add(new RequestParamParameterProcessor());
            annotatedParameterProcessors.add(new RequestBodyParameterProcessor());

            for (AnnotatedParameterProcessor annotatedParameterProcessor : annotatedParameterProcessors) {
                GenericBeanDefinition beanDefinition = new GenericBeanDefinition();
                beanDefinition.setBeanClass(annotatedParameterProcessor.getClass());
                beanDefinition.setAutowireCandidate(true);
                beanDefinition.setRole(BeanDefinition.ROLE_INFRASTRUCTURE);
                registry.registerBeanDefinition(annotatedParameterProcessor.getClass().getSimpleName(), beanDefinition);
            }
        }

        @Override
        public void postProcessBeanFactory(final ConfigurableListableBeanFactory beanFactory) throws BeansException {

        }
    }
}
