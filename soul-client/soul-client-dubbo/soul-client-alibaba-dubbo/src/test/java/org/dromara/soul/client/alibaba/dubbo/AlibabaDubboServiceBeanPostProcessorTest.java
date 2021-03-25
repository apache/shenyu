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

package org.dromara.soul.client.alibaba.dubbo;

/**
 * Test case for {@link AlibabaDubboServiceBeanListener}.
 *
 * @author HoldDie
 */
public final class AlibabaDubboServiceBeanPostProcessorTest {

    private static AlibabaDubboServiceBeanListener alibabaDubboServiceBeanListener;

//    @Test
//    public void testOnApplicationEventWithNonDubboConfigContextPath() {
//        Properties properties = new Properties();
//        properties.setProperty("appName", "dubbo");
//        SoulRegisterCenterConfig mockRegisterCenter = new SoulRegisterCenterConfig();
//        mockRegisterCenter.setServerLists("http://127.0.0.1:28080");
//        mockRegisterCenter.setRegisterType("http");
//        mockRegisterCenter.setProps(properties);
//        try {
//            alibabaDubboServiceBeanListener = new AlibabaDubboServiceBeanListener(mockRegisterCenter);
//        } catch (RuntimeException e) {
//            assertThat("Alibaba dubbo client must config the contextPath, adminUrl", is(e.getMessage()));
//        }
//
//        try {
//            mockRegisterCenter.setServerLists(null);
//            properties.setProperty("/contextPath", "dubbo");
//            alibabaDubboServiceBeanListener = new AlibabaDubboServiceBeanListener(mockRegisterCenter);
//        } catch (RuntimeException e) {
//            assertThat("Alibaba dubbo client must config the contextPath, adminUrl", is(e.getMessage()));
//        }
//    }
//
//    @Test
//    public void testOnApplicationEventNormally() {
//        Properties properties = new Properties();
//        properties.setProperty("appName", "dubbo");
//        properties.setProperty("/contextPath", "dubbo");
//        SoulRegisterCenterConfig mockRegisterCenter = new SoulRegisterCenterConfig();
//        mockRegisterCenter.setServerLists("http://127.0.0.1:28080");
//        mockRegisterCenter.setRegisterType("http");
//        mockRegisterCenter.setProps(properties);
//        alibabaDubboServiceBeanListener = new AlibabaDubboServiceBeanListener(mockRegisterCenter);
//        ApplicationContext applicationContext = new AnnotationConfigApplicationContext(MockApplicationConfiguration.class);
//        ContextRefreshedEvent contextRefreshedEvent = new ContextRefreshedEvent(applicationContext);
//        alibabaDubboServiceBeanListener.onApplicationEvent(contextRefreshedEvent);
//    }
//
//    @Test
//    public void testOnApplicationEventWithNonNullContextRefreshedEventParent() {
//        Properties properties = new Properties();
//        properties.setProperty("appName", "dubbo");
//        properties.setProperty("/contextPath", "dubbo");
//        SoulRegisterCenterConfig mockRegisterCenter = new SoulRegisterCenterConfig();
//        mockRegisterCenter.setServerLists("http://127.0.0.1:28080");
//        mockRegisterCenter.setRegisterType("http");
//        mockRegisterCenter.setProps(properties);
//        alibabaDubboServiceBeanListener = new AlibabaDubboServiceBeanListener(mockRegisterCenter);
//
//        ApplicationContext mockApplicationContext = mock(ApplicationContext.class);
//        when(mockApplicationContext.getParent()).thenReturn(new AnnotationConfigApplicationContext());
//        ContextRefreshedEvent contextRefreshedEvent = new ContextRefreshedEvent(mockApplicationContext);
//        alibabaDubboServiceBeanListener.onApplicationEvent(contextRefreshedEvent);
//    }
//
//    interface MockDubboService {
//        String foo();
//    }
//
//    static class MockDubboServiceImpl implements MockDubboService {
//
//        @Override
//        @SoulDubboClient(path = "/mock/foo")
//        public String foo() {
//            return "bar";
//        }
//    }
//
//    static class MockApplicationConfiguration {
//
//        @Bean
//        public MockDubboService dubboService() {
//            NameMatchMethodPointcutAdvisor advisor = new NameMatchMethodPointcutAdvisor();
//            advisor.setAdvice(new PerformanceMonitorInterceptor());
//            advisor.setMappedName("foo");
//
//            ProxyFactoryBean proxyFactoryBean = new ProxyFactoryBean();
//            proxyFactoryBean.setProxyTargetClass(true);
//            proxyFactoryBean.setTarget(new MockDubboServiceImpl());
//            return (MockDubboService) proxyFactoryBean.getObject();
//        }
//
//        @Bean
//        public ServiceBean<Object> mockServiceBean() {
//            Service service = mock(Service.class);
//            when(service.interfaceName()).thenReturn(MockDubboService.class.getName());
//            ServiceBean<Object> serviceBean = new ServiceBean<>(service);
//            serviceBean.setRef(this.dubboService());
//            ApplicationConfig mockDubboApplicationCfg = mock(ApplicationConfig.class);
//            when(mockDubboApplicationCfg.getName()).thenReturn("AlibabaDubboServiceBeanPostProcessorTest");
//            serviceBean.setApplication(mockDubboApplicationCfg);
//            RegistryConfig registryConfig = new RegistryConfig();
//            registryConfig.setAddress(RegistryConfig.NO_AVAILABLE);
//            serviceBean.setRegistries(Collections.singletonList(registryConfig));
//            return serviceBean;
//        }
//    }
}
