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
import org.dromara.soul.client.springcloud.annotation.SoulSpringCloudClient;
import org.dromara.soul.client.springcloud.config.SoulSpringCloudConfig;
import org.dromara.soul.client.springcloud.init.SpringCloudClientBeanPostProcessor;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.MethodSorters;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.core.env.Environment;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import static io.undertow.Handlers.path;
import static org.mockito.Mockito.when;

/**
 * SpringMvcClientBeanPostProcessorTest.
 *
 * @author kaitoShy
 */
@RunWith(MockitoJUnitRunner.class)
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public final class SpringMvcClientBeanPostProcessorTest {
    @Mock
    private static Environment env;

    private static Undertow server;

    private static boolean isRegister;

    private static SpringCloudClientBeanPostProcessor springCloudClientBeanPostProcessor;

    private static CountDownLatch countDownLatch;

    private static String port;

    private final SpringMvcClientTestBean springMvcClientTestBean = new SpringMvcClientTestBean();

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
        port = server.getListenerInfo().get(0).getAddress().toString().split(":")[1];
    }

    @AfterClass
    public static void after() {
        server.stop();
    }

    @Before
    public void before() {
        countDownLatch = new CountDownLatch(1);
        isRegister = false;
    }

    @Test
    public void testSoulBeanProcess() throws InterruptedException {
        SoulSpringCloudConfig soulSpringCloudConfig = new SoulSpringCloudConfig();
        soulSpringCloudConfig.setAdminUrl("http://127.0.0.1:" + port);
        soulSpringCloudConfig.setContextPath("test");
        when(env.getProperty("spring.application.name")).thenReturn("spring-cloud-test");
        springCloudClientBeanPostProcessor = new SpringCloudClientBeanPostProcessor(soulSpringCloudConfig, env);
        springCloudClientBeanPostProcessor.postProcessAfterInitialization(springMvcClientTestBean, "springMvcClientTestBean");
        countDownLatch.await(5, TimeUnit.SECONDS);
        Assert.assertTrue(isRegister);
    }

    @Test
    public void testNormalBeanProcess() throws InterruptedException {
        SoulSpringCloudConfig soulSpringCloudConfig = new SoulSpringCloudConfig();
        soulSpringCloudConfig.setAdminUrl("http://127.0.0.1:" + port);
        soulSpringCloudConfig.setContextPath("test");
        when(env.getProperty("spring.application.name")).thenReturn("spring-cloud-test");
        springCloudClientBeanPostProcessor = new SpringCloudClientBeanPostProcessor(soulSpringCloudConfig, env);
        springCloudClientBeanPostProcessor.postProcessAfterInitialization(new Object(), "normalBean");
        countDownLatch.await(5, TimeUnit.SECONDS);
        Assert.assertFalse(isRegister);
    }

    @RestController
    @RequestMapping("/order")
    @SoulSpringCloudClient(path = "/order")
    static class SpringMvcClientTestBean {
        @PostMapping("/save")
        @SoulSpringCloudClient(path = "/save")
        public String save(@RequestBody final String body) {
            return "" + body;
        }
    }
}
