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

package org.dromara.soul.client.springcloud.init;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.beans.factory.support.BeanDefinitionRegistryPostProcessor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Test Case for SpringCloudClientBeanPostProcessor.
 *
 * @author DaveModl (davemo-coderpersonal@hotmail.com)
 */
@Configuration
public class SpringCloudClientBeanPostProcessorTestConfig {

    /**
     *  return bean springCloudClientBeanPostProcessorRegistrar.
     *
     * @return SpringCloudClientBeanPostProcessorRegistrar
     */
    @Bean
    public SpringCloudClientBeanPostProcessorRegistrar springCloudClientBeanPostProcessorRegistrar() {
        return new SpringCloudClientBeanPostProcessorRegistrar();
    }

    public class SpringCloudClientBeanPostProcessorRegistrar implements BeanDefinitionRegistryPostProcessor {

        @Override
        public void postProcessBeanDefinitionRegistry(final BeanDefinitionRegistry beanDefinitionRegistry) throws BeansException {
            BeanDefinitionBuilder bean = BeanDefinitionBuilder.genericBeanDefinition(SpringCloudClientBeanPostProcessor.class)
                    .addConstructorArgValue(SpringCloudClientBeanPostProcessorTest.CONFIG)
                    .addConstructorArgValue(SpringCloudClientBeanPostProcessorTest.ENVIRONMENT);
            beanDefinitionRegistry.registerBeanDefinition("springCloudClientBeanPostProcessor", bean.getBeanDefinition());
        }

        @Override
        public void postProcessBeanFactory(final ConfigurableListableBeanFactory configurableListableBeanFactory) throws BeansException { }

    }
}
