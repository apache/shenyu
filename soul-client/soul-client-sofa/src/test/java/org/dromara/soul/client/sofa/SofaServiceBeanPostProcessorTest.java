/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.client.sofa;

import com.alipay.sofa.runtime.service.component.impl.ServiceImpl;
import com.alipay.sofa.runtime.spring.factory.ServiceFactoryBean;
import org.dromara.soul.client.sofa.common.annotation.SoulSofaClient;
import org.dromara.soul.client.sofa.common.config.SofaConfig;
import org.junit.BeforeClass;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.MethodSorters;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.stereotype.Service;
import org.springframework.web.bind.annotation.RequestBody;

import java.lang.reflect.Field;

/**
 * Test case for {@link SofaServiceBeanPostProcessor}.
 *
 * @author HoldDie
 */
@RunWith(MockitoJUnitRunner.class)
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public final class SofaServiceBeanPostProcessorTest {
    private static SofaServiceBeanPostProcessor sofaServiceBeanPostProcessorUnderTest;

    @BeforeClass
    public static void init() {
        SofaConfig mockSofaConfig = new SofaConfig();
        mockSofaConfig.setAdminUrl("http://localhost:58080");
        mockSofaConfig.setAppName("sofa");
        mockSofaConfig.setContextPath("/sofa");
        sofaServiceBeanPostProcessorUnderTest = new SofaServiceBeanPostProcessor(mockSofaConfig);
    }

    @Test
    public void testPostProcessAfterInitialization() throws Exception {
        ServiceFactoryBean serviceFactoryBean = new ServiceFactoryBean();
        Class<?> serviceFactoryBeanClass = serviceFactoryBean.getClass();
        Field serviceField = serviceFactoryBeanClass.getDeclaredField("service");
        serviceField.setAccessible(true);
        serviceField.set(serviceFactoryBean,
                new ServiceImpl("uniqueId", SofaService.class, new SoulSofaServiceImpl()));
        Field interfaceClassField = serviceFactoryBeanClass.getSuperclass().getDeclaredField("interfaceClass");
        interfaceClassField.setAccessible(true);
        interfaceClassField.set(serviceFactoryBean, SoulSofaServiceImpl.class);
        sofaServiceBeanPostProcessorUnderTest
                .postProcessAfterInitialization(serviceFactoryBean, "soulSofaServiceImpl");
    }

    @Test
    public void testPostProcessAfterInitializationWithNormalBean() {
        sofaServiceBeanPostProcessorUnderTest
                .postProcessAfterInitialization(new SofaServiceImpl(), "sofaServiceImpl");
    }

    interface SofaService {
        String save(String body);
    }

    @Service("soulSofaServiceImpl")
    static class SoulSofaServiceImpl implements SofaService {
        @Override
        @SoulSofaClient(path = "/save")
        public String save(@RequestBody final String body) {
            return "" + body;
        }
    }

    @Service("sofaServiceImpl")
    static class SofaServiceImpl implements SofaService {
        @Override
        public String save(@RequestBody final String body) {
            return "" + body;
        }
    }
}
