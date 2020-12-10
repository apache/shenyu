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

package org.dromara.soul.client.springcloud;

import io.undertow.Undertow;
import org.dromara.soul.client.springcloud.config.SoulSpringCloudConfig;
import org.dromara.soul.client.springcloud.init.ContextRegisterListener;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.MethodSorters;
import org.mockito.Mock;
import org.junit.Assert;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.core.env.Environment;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import static io.undertow.Handlers.path;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * ContextRegisterListenerTest.
 *
 * @author kaitoShy
 */
@RunWith(MockitoJUnitRunner.class)
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public final class ContextRegisterListenerTest {
    private static boolean isRegister;

    private static Undertow server;

    private static CountDownLatch countDownLatch;

    @Mock
    private static Environment env;

    @BeforeClass
    public static void init() {
        server = Undertow.builder()
                .addHttpListener(58888, "localhost")
                .setHandler(path()
                        .addPrefixPath("/soul-client/springcloud-register", httpServerExchange -> {
                            isRegister = true;
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
    }

    @Test
    public void testNotFullRegister() throws InterruptedException {
        isRegister = false;
        SoulSpringCloudConfig soulSpringCloudConfig = new SoulSpringCloudConfig();
        soulSpringCloudConfig.setAdminUrl("http://127.0.0.1:58888");
        soulSpringCloudConfig.setContextPath("test");
        when(env.getProperty("spring.application.name")).thenReturn("spring-cloud-test");
        ContextRegisterListener contextRegisterListener = new ContextRegisterListener(soulSpringCloudConfig, env);
        ContextRefreshedEvent contextRefreshedEvent = mock(ContextRefreshedEvent.class);
        contextRegisterListener.onApplicationEvent(contextRefreshedEvent);
        countDownLatch.await(500L, TimeUnit.MILLISECONDS);
        Assert.assertFalse(isRegister);
    }

    @Test
    public void testFullRegister() throws InterruptedException {
        SoulSpringCloudConfig soulSpringMvcConfig = new SoulSpringCloudConfig();
        soulSpringMvcConfig.setAdminUrl("http://127.0.0.1:58888");
        soulSpringMvcConfig.setContextPath("test");
        soulSpringMvcConfig.setFull(true);
        when(env.getProperty("spring.application.name")).thenReturn("spring-cloud-test");
        ContextRegisterListener contextRegisterListener = new ContextRegisterListener(soulSpringMvcConfig, env);
        ContextRefreshedEvent contextRefreshedEvent = mock(ContextRefreshedEvent.class);
        contextRegisterListener.onApplicationEvent(contextRefreshedEvent);
        countDownLatch.await(500L, TimeUnit.MILLISECONDS);
        Assert.assertTrue(isRegister);
    }
}
