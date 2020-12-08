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
import io.undertow.Undertow;
import org.dromara.soul.client.sofa.common.annotation.SoulSofaClient;
import org.dromara.soul.client.sofa.common.config.SofaConfig;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.MethodSorters;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.stereotype.Service;
import org.springframework.web.bind.annotation.RequestBody;

import java.lang.reflect.Field;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import static io.undertow.Handlers.path;

/**
 * Test case for SofaServiceBeanPostProcessor.
 *
 * @author HoldDie
 */
@RunWith(MockitoJUnitRunner.class)
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public final class SofaServiceBeanPostProcessorTest {

    private static Undertow server;

    private static long registerNum;

    private static CountDownLatch countDownLatch;

    private static SofaServiceBeanPostProcessor sofaServiceBeanPostProcessorUnderTest;

    @BeforeClass
    public static void init() {
        SofaConfig mockSofaConfig = new SofaConfig();
        mockSofaConfig.setAdminUrl("http://localhost:9095");
        mockSofaConfig.setAppName("sofa");
        mockSofaConfig.setContextPath("/sofa");
        sofaServiceBeanPostProcessorUnderTest = new SofaServiceBeanPostProcessor(mockSofaConfig);
        // config server
        server = Undertow.builder()
                .addHttpListener(9095, "localhost")
                .setHandler(path().addPrefixPath("/soul-client/sofa-register", httpServerExchange -> {
                    registerNum++;
                    countDownLatch.countDown();
                }))
                .build();
        server.start();
    }

    @AfterClass
    public static void after() {
        server.stop();
    }

    @Before
    public void before() {
        countDownLatch = new CountDownLatch(1);
        registerNum = 0;
    }

    @Test
    public void testPostProcessAfterInitialization() throws Exception {
        ServiceFactoryBean serviceFactoryBean = new ServiceFactoryBean();
        Class<?> c = serviceFactoryBean.getClass();
        Field serviceField = c.getDeclaredField("service");
        serviceField.setAccessible(true);
        serviceField.set(serviceFactoryBean,
                new ServiceImpl("uniqueId", SofaService.class, new SoulSofaServiceImpl()));

        Field interfaceClassField = c.getSuperclass().getDeclaredField("interfaceClass");
        interfaceClassField.setAccessible(true);
        interfaceClassField.set(serviceFactoryBean, SoulSofaServiceImpl.class);
        sofaServiceBeanPostProcessorUnderTest
                .postProcessAfterInitialization(serviceFactoryBean, "soulSofaServiceImpl");
        countDownLatch.await(500L, TimeUnit.MILLISECONDS);
        Assert.assertEquals(1L, registerNum);
    }

    @Test
    public void testPostProcessAfterInitializationWithNormalBean() throws Exception {
        sofaServiceBeanPostProcessorUnderTest
                .postProcessAfterInitialization(new SofaServiceImpl(), "SofaServiceImpl");
        countDownLatch.await(500L, TimeUnit.MILLISECONDS);
        Assert.assertEquals(0L, registerNum);
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
