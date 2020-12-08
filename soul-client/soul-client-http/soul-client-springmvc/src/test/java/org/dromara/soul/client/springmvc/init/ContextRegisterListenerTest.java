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
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;

import static io.undertow.Handlers.path;
import static org.mockito.Mockito.mock;

import org.springframework.context.event.ContextRefreshedEvent;

/**
 * ContextRegisterListenerTest.
 *
 * @author tydhot
 */
@RunWith(MockitoJUnitRunner.class)
public final class ContextRegisterListenerTest {

    private static boolean isRegister;

    private static Undertow server;

    @BeforeClass
    public static void init() {
        server = Undertow.builder()
                .addHttpListener(58888, "localhost")
                .setHandler(path()
                        .addPrefixPath("/soul-client/springmvc-register", httpServerExchange -> isRegister = true))
                .build();
        server.start();
    }

    @AfterClass
    public static void after() {
        server.stop();
    }

    @Test
    public void testNotFullRegister() throws InterruptedException {
        isRegister = false;
        SoulSpringMvcConfig soulSpringMvcConfig = new SoulSpringMvcConfig();
        soulSpringMvcConfig.setAdminUrl("http://127.0.0.1:58888");
        soulSpringMvcConfig.setAppName("test-mvc");
        soulSpringMvcConfig.setContextPath("test");
        soulSpringMvcConfig.setPort(58889);
        ContextRegisterListener contextRegisterListener = new ContextRegisterListener(soulSpringMvcConfig);
        ContextRefreshedEvent contextRefreshedEvent = mock(ContextRefreshedEvent.class);
        contextRegisterListener.onApplicationEvent(contextRefreshedEvent);
        Thread.sleep(500L);
        Assert.assertFalse(isRegister);
    }

    @Test
    public void testFullRegister() throws InterruptedException {
        isRegister = false;
        SoulSpringMvcConfig soulSpringMvcConfig = new SoulSpringMvcConfig();
        soulSpringMvcConfig.setAdminUrl("http://127.0.0.1:58888");
        soulSpringMvcConfig.setAppName("test-mvc");
        soulSpringMvcConfig.setContextPath("test");
        soulSpringMvcConfig.setFull(true);
        soulSpringMvcConfig.setPort(58889);
        ContextRegisterListener contextRegisterListener = new ContextRegisterListener(soulSpringMvcConfig);
        ContextRefreshedEvent contextRefreshedEvent = mock(ContextRefreshedEvent.class);
        contextRegisterListener.onApplicationEvent(contextRefreshedEvent);
        Thread.sleep(500L);
        Assert.assertTrue(isRegister);
    }
}
