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

package org.apache.shenyu.springboot.starter.client.apache.dubbo;

import org.apache.shenyu.client.apache.dubbo.ApacheDubboServiceBeanListener;
import org.junit.Test;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.autoconfigure.ImportAutoConfiguration;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.ImportResource;
import org.springframework.context.annotation.PropertySource;
import org.springframework.context.support.PropertySourcesPlaceholderConfigurer;
import org.springframework.beans.factory.xml.XmlBeanDefinitionReader;

import static org.junit.Assert.assertNotNull;

/**
 * Test case for {@link ShenyuApacheDubboClientConfiguration}.
 */
@ImportAutoConfiguration(ShenyuApacheDubboClientConfigurationTest.EnvironmentSetting.class)
public final class ShenyuApacheDubboClientConfigurationTest {

    @Test
    public void testShenyuApacheDubboClientConfiguration() {
        new ApplicationContextRunner()
            .withConfiguration(AutoConfigurations.of(ShenyuApacheDubboClientConfiguration.class))
            .withBean(ShenyuApacheDubboClientConfigurationTest.class)
            .withPropertyValues("debug=true")
            .run(
                context -> {
                    ApacheDubboServiceBeanListener listener = context.getBean("apacheDubboServiceBeanListener", ApacheDubboServiceBeanListener.class);
                    assertNotNull(listener);
                }
            );
    }

    @Configuration
    @EnableConfigurationProperties
    class EnvironmentSetting {
        @Configuration
        @PropertySource(value = "classpath:application.properties")
        public class ApplicationResource {

        }

        /**
         * takes precedence over spring-dubbo.xml load properties.
         */
        @Configuration
        @ImportAutoConfiguration(PropertySourcesPlaceholderConfigurer.class)
        @ImportResource(value = "classpath:spring-dubbo.xml", reader = XmlBeanDefinitionReader.class)
        public class SpringDubboResource {

        }

        @Configuration
        @ComponentScan(value = "org.apache.shenyu.springboot.starter.client.apache.dubbo.service.impl")
        public class ScanDubboImpl {

        }
    }
}
