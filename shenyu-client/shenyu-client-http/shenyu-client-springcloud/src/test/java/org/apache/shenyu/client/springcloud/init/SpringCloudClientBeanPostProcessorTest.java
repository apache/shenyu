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

package org.apache.shenyu.client.springcloud.init;

import org.apache.shenyu.client.springcloud.annotation.ShenyuSpringCloudClient;
import org.apache.shenyu.client.core.register.ShenyuClientRegisterRepositoryFactory;
import org.apache.shenyu.register.client.http.utils.RegisterUtils;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.junit.Before;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.MethodSorters;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.MockitoJUnitRunner;
import org.mockito.stubbing.Answer;
import org.springframework.core.env.Environment;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Properties;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import static org.hamcrest.Matchers.equalTo;
import static org.junit.Assert.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

/**
 * Test for {@link SpringCloudClientBeanPostProcessor}.
 */
@RunWith(MockitoJUnitRunner.class)
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public final class SpringCloudClientBeanPostProcessorTest {
    @Mock
    private static Environment env;

    private final SpringCloudClientTestBean springCloudClientTestBean = new SpringCloudClientTestBean();

    @Before
    public void init() {
        when(env.getProperty("spring.application.name")).thenReturn("spring-cloud-test");
    }

    @Test
    public void testShenyuBeanProcess() {
        // config with full
        SpringCloudClientBeanPostProcessor springCloudClientBeanPostProcessor = buildSpringCloudClientBeanPostProcessor(true);
        assertThat(springCloudClientTestBean, equalTo(springCloudClientBeanPostProcessor.postProcessAfterInitialization(springCloudClientTestBean, "springCloudClientTestBean")));
    }

    @Test
    public void testNormalBeanProcess() {
        SpringCloudClientBeanPostProcessor springCloudClientBeanPostProcessor = buildSpringCloudClientBeanPostProcessor(false);
        Object normalBean = new Object();

        assertThat(normalBean, equalTo(springCloudClientBeanPostProcessor.postProcessAfterInitialization(normalBean, "normalBean")));
    }

    @Test
    public void testWithShenyuClientAnnotation() {
        try (MockedStatic mocked = mockStatic(RegisterUtils.class)) {
            mocked.when(() -> RegisterUtils.doRegister(any(), any(), any()))
                    .thenAnswer((Answer<Void>) invocation -> null);
            SpringCloudClientBeanPostProcessor springCloudClientBeanPostProcessor = buildSpringCloudClientBeanPostProcessor(false);
            ReflectionTestUtils.setField(springCloudClientBeanPostProcessor, "executorService", new ThreadPoolExecutor(1,
                    1, 0L, TimeUnit.MILLISECONDS, new LinkedBlockingQueue<>()) {
                @Override
                public void execute(final Runnable command) {
                    command.run();
                }
            });
            assertThat(springCloudClientTestBean, equalTo(springCloudClientBeanPostProcessor.postProcessAfterInitialization(springCloudClientTestBean, "normalBean")));
        }
    }

    private SpringCloudClientBeanPostProcessor buildSpringCloudClientBeanPostProcessor(final boolean full) {
        Properties properties = new Properties();
        properties.setProperty("contextPath", "/test");
        properties.setProperty("isFull", full + "");
        properties.setProperty("ip", "127.0.0.1");
        properties.setProperty("port", "8081");
        ShenyuRegisterCenterConfig mockRegisterCenter = new ShenyuRegisterCenterConfig();
        mockRegisterCenter.setServerLists("http://127.0.0.1:8080");
        mockRegisterCenter.setRegisterType("http");
        mockRegisterCenter.setProps(properties);
        return new SpringCloudClientBeanPostProcessor(mockRegisterCenter, env, ShenyuClientRegisterRepositoryFactory.newInstance(mockRegisterCenter));
    }

    @RestController
    @RequestMapping("/order")
    @ShenyuSpringCloudClient(path = "/order")
    static class SpringCloudClientTestBean {
        @PostMapping("/save")
        @ShenyuSpringCloudClient(path = "/save")
        public String save(@RequestBody final String body) {
            return "" + body;
        }
    }
}
