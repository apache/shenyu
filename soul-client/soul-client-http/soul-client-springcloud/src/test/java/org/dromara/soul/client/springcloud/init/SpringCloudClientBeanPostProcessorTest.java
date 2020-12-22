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

package org.dromara.soul.client.springcloud.init;

import org.dromara.soul.client.common.utils.RegisterUtils;
import org.dromara.soul.client.springcloud.annotation.SoulSpringCloudClient;
import org.dromara.soul.client.springcloud.config.SoulSpringCloudConfig;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.MockedStatic;
import org.mockito.stubbing.Answer;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.mock.env.MockEnvironment;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mockStatic;

/**
 * Test Case for SpringCloudClientBeanPostProcessor.
 *
 * @author DaveModl (davemo-coderpersonal@hotmail.com)
 */
public final class SpringCloudClientBeanPostProcessorTest {

    static final MockEnvironment ENVIRONMENT = new MockEnvironment();

    static final SoulSpringCloudConfig CONFIG = new SoulSpringCloudConfig();

    /**
     * From SpringMvcClientBeanPostProcessorTest.
     */
    static final SpringMvcClientTestBean SPRING_MVC_CLIENT_TEST_BEAN = new SpringMvcClientTestBean();

    @Before
    public void setUp() {
        CONFIG.setAdminUrl("http://127.0.0.1:8080");
        CONFIG.setContextPath("/test");
        CONFIG.setFull(true);
        ENVIRONMENT.setProperty("spring.application.name", "testApp");

    }

    @Test
    public void test() {
        AnnotationConfigApplicationContext context = new AnnotationConfigApplicationContext(SpringCloudClientBeanPostProcessorTestConfig.class);
        SpringCloudClientBeanPostProcessor beanPostProcessor = context.getBean(SpringCloudClientBeanPostProcessor.class, CONFIG, ENVIRONMENT);
        Assert.assertNotNull(beanPostProcessor);
        //full
        Assert.assertEquals(beanPostProcessor, beanPostProcessor.postProcessAfterInitialization(beanPostProcessor, "springCloudClientBeanPostProcessor"));
        CONFIG.setFull(false);
        //not full
        SpringCloudClientBeanPostProcessor other = context.getBean(SpringCloudClientBeanPostProcessor.class, CONFIG, ENVIRONMENT);
        //from SpringMvcClientBeanPostProcessorTest
        Assert.assertEquals(other, other.postProcessAfterInitialization(other, "springCloudClientBeanPostProcessor"));
        try (MockedStatic mocked = mockStatic(RegisterUtils.class)) {
            mocked.when(() -> RegisterUtils.doRegister(any(), any(), any()))
                    .thenAnswer((Answer<Void>) invocation -> null);
            ReflectionTestUtils.setField(other, "executorService", new ThreadPoolExecutor(1,
                    1, 0L, TimeUnit.MILLISECONDS, new LinkedBlockingQueue<>()) {
                @Override
                public void execute(final Runnable command) {
                    command.run();
                }
            });
            assertEquals(SPRING_MVC_CLIENT_TEST_BEAN, other.postProcessAfterInitialization(SPRING_MVC_CLIENT_TEST_BEAN, "normalBean"));
        }
    }

    /**
     * From SpringMvcClientBeanPostProcessorTest.
     */
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

