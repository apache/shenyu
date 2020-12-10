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

package org.dromara.soul.client.springmvc.init;

import io.undertow.Undertow;
import org.dromara.soul.client.springmvc.config.SoulSpringMvcConfig;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.Assert;
import org.junit.runner.RunWith;
import org.junit.runners.MethodSorters;
import org.mockito.junit.MockitoJUnitRunner;

import static io.undertow.Handlers.path;
import static org.mockito.Mockito.mock;

import org.springframework.context.event.ContextRefreshedEvent;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

/**
 * ContextRegisterListenerTest.
 *
 * @author tydhot
 */
@RunWith(MockitoJUnitRunner.class)
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public final class ContextRegisterListenerTest {

    private static boolean isRegister;

    private static Undertow server;

    private static CountDownLatch countDownLatch;

    @BeforeClass
    public static void init() {
        server = Undertow.builder()
                .addHttpListener(58888, "localhost")
                .setHandler(path()
                        .addPrefixPath("/soul-client/springmvc-register", httpServerExchange -> {
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
        String port = server.getListenerInfo().get(0).getAddress().toString().split(":")[1];
        SoulSpringMvcConfig soulSpringMvcConfig = new SoulSpringMvcConfig();
        soulSpringMvcConfig.setAdminUrl("http://127.0.0.1:" + port);
        soulSpringMvcConfig.setAppName("test-mvc");
        soulSpringMvcConfig.setContextPath("test");
        soulSpringMvcConfig.setPort(58889);
        ContextRegisterListener contextRegisterListener = new ContextRegisterListener(soulSpringMvcConfig);
        ContextRefreshedEvent contextRefreshedEvent = mock(ContextRefreshedEvent.class);
        contextRegisterListener.onApplicationEvent(contextRefreshedEvent);
        countDownLatch.await(5, TimeUnit.SECONDS);
        Assert.assertFalse(isRegister);
    }

    @Test
    public void testFullRegister() throws InterruptedException {
        String port = server.getListenerInfo().get(0).getAddress().toString().split(":")[1];
        SoulSpringMvcConfig soulSpringMvcConfig = new SoulSpringMvcConfig();
        soulSpringMvcConfig.setAdminUrl("http://127.0.0.1:" + port);
        soulSpringMvcConfig.setAppName("test-mvc");
        soulSpringMvcConfig.setContextPath("test");
        soulSpringMvcConfig.setFull(true);
        soulSpringMvcConfig.setPort(58889);
        ContextRegisterListener contextRegisterListener = new ContextRegisterListener(soulSpringMvcConfig);
        ContextRefreshedEvent contextRefreshedEvent = mock(ContextRefreshedEvent.class);
        contextRegisterListener.onApplicationEvent(contextRefreshedEvent);
        countDownLatch.await(5, TimeUnit.SECONDS);
        Assert.assertTrue(isRegister);
    }
}
