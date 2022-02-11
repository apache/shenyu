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

import org.apache.shenyu.client.core.register.ShenyuClientRegisterRepositoryFactory;
import org.apache.shenyu.client.springcloud.annotation.ShenyuSpringCloudClient;
import org.apache.shenyu.register.client.http.utils.RegisterUtils;
import org.apache.shenyu.register.common.config.PropertiesConfig;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.stubbing.Answer;
import org.springframework.core.env.Environment;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Optional;
import java.util.Properties;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

/**
 * Test for {@link SpringCloudClientBeanPostProcessor}.
 */
@ExtendWith(MockitoExtension.class)
@TestMethodOrder(MethodOrderer.Alphanumeric.class)
public final class SpringCloudClientBeanPostProcessorTest {

    @Mock
    private static Environment env;

    private final MockedStatic<RegisterUtils> registerUtilsMockedStatic = mockStatic(RegisterUtils.class);

    private final SpringCloudClientTestBean springCloudClientTestBean = new SpringCloudClientTestBean();

    @BeforeEach
    public void init() {
        when(env.getProperty("spring.application.name")).thenReturn("spring-cloud-test");
    }

    @Test
    public void testShenyuBeanProcess() {
        registerUtilsMockedStatic.when(() -> RegisterUtils.doLogin(any(), any(), any())).thenReturn(Optional.of("token"));
        // config with full
        SpringCloudClientBeanPostProcessor springCloudClientBeanPostProcessor = buildSpringCloudClientBeanPostProcessor(true);
        assertThat(springCloudClientTestBean, equalTo(springCloudClientBeanPostProcessor.postProcessAfterInitialization(springCloudClientTestBean, "springCloudClientTestBean")));
        registerUtilsMockedStatic.close();
    }

    @Test
    public void testNormalBeanProcess() {
        registerUtilsMockedStatic.when(() -> RegisterUtils.doLogin(any(), any(), any())).thenReturn(Optional.of("token"));
        SpringCloudClientBeanPostProcessor springCloudClientBeanPostProcessor = buildSpringCloudClientBeanPostProcessor(false);
        Object normalBean = new Object();

        assertThat(normalBean, equalTo(springCloudClientBeanPostProcessor.postProcessAfterInitialization(normalBean, "normalBean")));
        registerUtilsMockedStatic.close();
    }

    @Test
    public void testWithShenyuClientAnnotation() {
        registerUtilsMockedStatic.when(() -> RegisterUtils.doLogin(any(), any(), any())).thenReturn(Optional.of("token"));
        registerUtilsMockedStatic.when(() -> RegisterUtils.doRegister(any(), any(), any()))
                .thenAnswer((Answer<Void>) invocation -> null);
        SpringCloudClientBeanPostProcessor springCloudClientBeanPostProcessor = buildSpringCloudClientBeanPostProcessor(false);
        assertThat(springCloudClientTestBean, equalTo(springCloudClientBeanPostProcessor.postProcessAfterInitialization(springCloudClientTestBean, "normalBean")));
        registerUtilsMockedStatic.close();
    }

    private SpringCloudClientBeanPostProcessor buildSpringCloudClientBeanPostProcessor(final boolean full) {
        Properties properties = new Properties();
        properties.setProperty("contextPath", "/test");
        properties.setProperty("isFull", full + "");
        properties.setProperty("ip", "127.0.0.1");
        properties.setProperty("port", "8081");
        properties.setProperty("username", "admin");
        properties.setProperty("password", "123456");
        PropertiesConfig config = new PropertiesConfig();
        config.setProps(properties);
        ShenyuRegisterCenterConfig mockRegisterCenter = new ShenyuRegisterCenterConfig();
        mockRegisterCenter.setServerLists("http://127.0.0.1:8080");
        mockRegisterCenter.setRegisterType("http");
        mockRegisterCenter.setProps(properties);
        return new SpringCloudClientBeanPostProcessor(config, ShenyuClientRegisterRepositoryFactory.newInstance(mockRegisterCenter), env);
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
