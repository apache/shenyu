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

package org.dromara.soul.client.apache.dubbo;

import org.apache.dubbo.config.ApplicationConfig;
import org.apache.dubbo.config.RegistryConfig;
import org.apache.dubbo.config.annotation.Service;
import org.apache.dubbo.config.spring.ServiceBean;
import org.dromara.soul.client.dubbo.common.annotation.SoulDubboClient;
import org.dromara.soul.client.dubbo.common.config.DubboConfig;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.MethodSorters;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.aop.framework.ProxyFactoryBean;
import org.springframework.aop.interceptor.PerformanceMonitorInterceptor;
import org.springframework.aop.support.NameMatchMethodPointcutAdvisor;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.event.ContextRefreshedEvent;

import java.util.Collections;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link ApacheDubboServiceBeanPostProcessor}.
 *
 * @author HoldDie
 */
@RunWith(MockitoJUnitRunner.class)
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public final class ApacheDubboServiceBeanPostProcessorTest {

    private static ApacheDubboServiceBeanPostProcessor apacheDubboServiceBeanPostProcessor;

    @Test
    public void testOnApplicationEventWithNonDubboConfigContextPath() {
        DubboConfig mockDubboConfig = new DubboConfig();
        mockDubboConfig.setAppName("dubbo");
        try {
            mockDubboConfig.setAdminUrl("http://127.0.0.1:28080");
            mockDubboConfig.setContextPath(null);
            apacheDubboServiceBeanPostProcessor = new ApacheDubboServiceBeanPostProcessor(mockDubboConfig);
        } catch (RuntimeException e) {
            assertThat("apache dubbo client must config the contextPath, adminUrl", is(e.getMessage()));
        }

        try {
            mockDubboConfig.setAdminUrl(null);
            mockDubboConfig.setContextPath("/dubbo");
            apacheDubboServiceBeanPostProcessor = new ApacheDubboServiceBeanPostProcessor(mockDubboConfig);
        } catch (RuntimeException e) {
            assertThat("apache dubbo client must config the contextPath, adminUrl", is(e.getMessage()));
        }
    }

    @Test
    public void testOnApplicationEventNormally() {
        DubboConfig mockDubboConfig = new DubboConfig();
        mockDubboConfig.setAdminUrl("http://127.0.0.1:28080");
        mockDubboConfig.setContextPath("/dubbo");
        apacheDubboServiceBeanPostProcessor = new ApacheDubboServiceBeanPostProcessor(mockDubboConfig);
        ApplicationContext applicationContext = new AnnotationConfigApplicationContext(MockApplicationConfiguration.class);
        ContextRefreshedEvent contextRefreshedEvent = new ContextRefreshedEvent(applicationContext);
        apacheDubboServiceBeanPostProcessor.onApplicationEvent(contextRefreshedEvent);
    }

    @Test
    public void testOnApplicationEventWithNonNullContextRefreshedEventParent() {
        DubboConfig mockDubboConfig = new DubboConfig();
        mockDubboConfig.setAdminUrl("http://127.0.0.1:28080");
        mockDubboConfig.setContextPath("/dubbo");
        apacheDubboServiceBeanPostProcessor = new ApacheDubboServiceBeanPostProcessor(mockDubboConfig);

        ApplicationContext mockApplicationContext = mock(ApplicationContext.class);
        when(mockApplicationContext.getParent()).thenReturn(new AnnotationConfigApplicationContext());
        ContextRefreshedEvent contextRefreshedEvent = new ContextRefreshedEvent(mockApplicationContext);
        apacheDubboServiceBeanPostProcessor.onApplicationEvent(contextRefreshedEvent);
    }

    interface MockDubboService {

        String foo();
    }

    static class MockDubboServiceImpl implements MockDubboService {

        @Override
        @SoulDubboClient(path = "/mock/foo")
        public String foo() {
            return "bar";
        }
    }

    static class MockApplicationConfiguration {

        @Bean
        public MockDubboService dubboService() {
            NameMatchMethodPointcutAdvisor advisor = new NameMatchMethodPointcutAdvisor();
            advisor.setAdvice(new PerformanceMonitorInterceptor());
            advisor.setMappedName("foo");

            ProxyFactoryBean proxyFactoryBean = new ProxyFactoryBean();
            proxyFactoryBean.setProxyTargetClass(true);
            proxyFactoryBean.setTarget(new MockDubboServiceImpl());
            return (MockDubboService) proxyFactoryBean.getObject();
        }

        @Bean
        public ServiceBean<Object> mockServiceBean() {
            Service service = mock(Service.class);
            when(service.interfaceName()).thenReturn(MockDubboService.class.getName());
            ServiceBean<Object> serviceBean = new ServiceBean<>(service);
            serviceBean.setRef(this.dubboService());
            ApplicationConfig applicationConfig = new ApplicationConfig();
            applicationConfig.setName("ApacheDubboServiceBeanPostProcessorTest");
            serviceBean.setApplication(applicationConfig);
            RegistryConfig registryConfig = new RegistryConfig();
            registryConfig.setAddress(RegistryConfig.NO_AVAILABLE);
            serviceBean.setRegistries(Collections.singletonList(registryConfig));
            return serviceBean;
        }
    }
}
