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

package org.apache.shenyu.sdk.spring;

import org.apache.shenyu.sdk.core.client.ShenyuSdkClient;
import org.apache.shenyu.sdk.spring.support.SpringMvcContract;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import org.junit.jupiter.api.Test;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.beans.factory.support.DefaultListableBeanFactory;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.test.util.ReflectionTestUtils;

/**
 * {@link ShenyuClientsRegistrar} test.
 */
public class ShenyuClientsRegistrarTest {

    @Test
    public void registerBeanDefinitionsTest() {
        AnnotationConfigApplicationContext context = new AnnotationConfigApplicationContext();
        final ShenyuSdkClient client = spy(ShenyuSdkClient.class);
        ((DefaultListableBeanFactory) context.getBeanFactory()).setAllowBeanDefinitionOverriding(false);
        context.register(TopLevelSubLevelTestConfig.class);
        context.register(SpringMvcContract.class);
        context.registerBean("shenyuSdkClient", ShenyuSdkClient.class, () -> client);

        context.refresh();

        final TopLevelClient topClient = context.getBean(TopLevelClient.class);
        final TopLevelSubLevelTestConfig.SubLevelClient subClient = context.getBean(TopLevelSubLevelTestConfig.SubLevelClient.class);
        assertNotNull(topClient);
        assertNotNull(subClient);
    }

    @Test
    public void nullUrlTest() {
        AnnotationConfigApplicationContext context = new AnnotationConfigApplicationContext();
        final ShenyuSdkClient client = spy(ShenyuSdkClient.class);
        ((DefaultListableBeanFactory) context.getBeanFactory()).setAllowBeanDefinitionOverriding(false);
        context.register(NullUrlTestConfig.class);
        context.register(SpringMvcContract.class);
        context.registerBean("shenyuSdkClient", ShenyuSdkClient.class, () -> client);

        context.refresh();
        final NullUrlShenClient topClient = context.getBean(NullUrlShenClient.class);

        Object invocationHandler = ReflectionTestUtils.getField(topClient, "h");
        assertNotNull(invocationHandler);
        ShenyuClientFactoryBean factoryBean = (ShenyuClientFactoryBean) ReflectionTestUtils.getField(invocationHandler, "shenyuClientFactoryBean");
        assertNotNull(factoryBean);
        assertEquals(factoryBean.getName(), "nullUrlShenClient");
        assertEquals(factoryBean.getUrl(), "http://nullUrlShenClient");
        assertNull(factoryBean.getPath());
    }

    @Test
    public void notNullUrlTest() {
        AnnotationConfigApplicationContext context = new AnnotationConfigApplicationContext();
        final ShenyuSdkClient client = spy(ShenyuSdkClient.class);
        ((DefaultListableBeanFactory) context.getBeanFactory()).setAllowBeanDefinitionOverriding(false);
        context.register(NotNullUrlTestConfig.class);
        context.register(SpringMvcContract.class);
        context.registerBean("shenyuSdkClient", ShenyuSdkClient.class, () -> client);

        final AnnotationConfigApplicationContext contextSpy = spy(context);
        final ConfigurableListableBeanFactory beanFactorySpy = spy(context.getBeanFactory());
        when(beanFactorySpy.resolveEmbeddedValue("#{test.url}")).thenReturn("localhost");
        when(beanFactorySpy.resolveEmbeddedValue("#{test.path}")).thenReturn("/dev/null");

        when(contextSpy.getBeanFactory()).thenReturn(beanFactorySpy);
        context = contextSpy;
        context.refresh();

        final NotNullUrlShenClient topClient = context.getBean(NotNullUrlShenClient.class);

        Object invocationHandler = ReflectionTestUtils.getField(topClient, "h");
        assertNotNull(invocationHandler);
        ShenyuClientFactoryBean factoryBean = (ShenyuClientFactoryBean) ReflectionTestUtils.getField(invocationHandler, "shenyuClientFactoryBean");
        assertNotNull(factoryBean);
        assertEquals(factoryBean.getName(), "notNullUrlShenClient");
        assertEquals(factoryBean.getUrl(), "http://localhost");
        assertEquals(factoryBean.getPath(), "/dev/null");
    }

    @EnableShenyuClients(basePackageClasses = {TopLevelClient.class, TopLevelSubLevelTestConfig.SubLevelClient.class})
    protected static class TopLevelSubLevelTestConfig {

        @ShenyuClient("sub-level")
        public interface SubLevelClient {

        }

    }

    @EnableShenyuClients(basePackageClasses = {NullUrlShenClient.class})
    protected static class NullUrlTestConfig {

    }

    @EnableShenyuClients(basePackageClasses = {NotNullUrlShenClient.class})
    protected static class NotNullUrlTestConfig {

    }

    @ShenyuClient(name = "nullUrlShenClient", url = "${test.url:#{null}}", path = "${test.path:#{null}}")
    protected interface NullUrlShenClient {

    }

    @ShenyuClient(name = "notNullUrlShenClient", url = "#{test.url}", path = "#{test.path}")
    protected interface NotNullUrlShenClient {

    }

    @ShenyuClient("top-level")
    public interface TopLevelClient {

    }

}
