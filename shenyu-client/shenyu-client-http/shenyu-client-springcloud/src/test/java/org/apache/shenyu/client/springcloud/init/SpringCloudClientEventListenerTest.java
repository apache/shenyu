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

import org.apache.shenyu.client.core.exception.ShenyuClientIllegalArgumentException;
import org.apache.shenyu.client.core.register.ShenyuClientRegisterRepositoryFactory;
import org.apache.shenyu.client.springcloud.annotation.ShenyuSpringCloudClient;
import org.apache.shenyu.register.client.http.utils.RegisterUtils;
import org.apache.shenyu.register.common.config.PropertiesConfig;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.junit.Assert;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.stubbing.Answer;
import org.springframework.context.ApplicationContext;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.core.env.Environment;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;
import java.util.Properties;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test for {@link SpringCloudClientEventListener}.
 */
@ExtendWith(MockitoExtension.class)
@TestMethodOrder(MethodOrderer.Alphanumeric.class)
public final class SpringCloudClientEventListenerTest {

    @Mock
    private static Environment env;

    private final MockedStatic<RegisterUtils> registerUtilsMockedStatic = mockStatic(RegisterUtils.class);

    private final SpringCloudClientTestBean springCloudClientTestBean = new SpringCloudClientTestBean();

    @Mock
    private ApplicationContext applicationContext;

    private ContextRefreshedEvent contextRefreshedEvent;

    @BeforeEach
    public void beforeEach() {
        when(env.getProperty("spring.application.name")).thenReturn("spring-cloud-test");
    }

    private void init() {
        Map<String, Object> results = new LinkedHashMap<>();
        results.put("springCloudClientTestBean", springCloudClientTestBean);
        when(applicationContext.getBeansWithAnnotation(any())).thenReturn(results);
        contextRefreshedEvent = new ContextRefreshedEvent(applicationContext);
    }

    @Test
    public void testShenyuBeanProcess() {
        registerUtilsMockedStatic.when(() -> RegisterUtils.doLogin(any(), any(), any())).thenReturn(Optional.of("token"));
        // config with full
        SpringCloudClientEventListener springCloudClientEventListener = buildSpringCloudClientEventListener(true);
        springCloudClientEventListener.onApplicationEvent(new ContextRefreshedEvent(applicationContext));
        verify(applicationContext, never()).getBeansWithAnnotation(any());
        registerUtilsMockedStatic.close();
    }

    @Test
    public void testNormalBeanProcess() {
        init();
        registerUtilsMockedStatic.when(() -> RegisterUtils.doLogin(any(), any(), any())).thenReturn(Optional.of("token"));
        SpringCloudClientEventListener springCloudClientEventListener = buildSpringCloudClientEventListener(false);
        springCloudClientEventListener.onApplicationEvent(contextRefreshedEvent);
        verify(applicationContext, times(2)).getBeansWithAnnotation(any());
        registerUtilsMockedStatic.close();
    }

    @Test
    public void testWithShenyuClientAnnotation() {
        init();
        registerUtilsMockedStatic.when(() -> RegisterUtils.doLogin(any(), any(), any())).thenReturn(Optional.of("token"));
        registerUtilsMockedStatic.when(() -> RegisterUtils.doRegister(any(), any(), any()))
                .thenAnswer((Answer<Void>) invocation -> null);
        SpringCloudClientEventListener springCloudClientEventListener = buildSpringCloudClientEventListener(false);
        springCloudClientEventListener.onApplicationEvent(contextRefreshedEvent);
        verify(applicationContext, times(2)).getBeansWithAnnotation(any());
        registerUtilsMockedStatic.close();
    }

    private SpringCloudClientEventListener buildSpringCloudClientEventListener(final boolean full) {
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
        // hit error
        when(env.getProperty("spring.application.name")).thenReturn("");
        Assert.assertThrows(ShenyuClientIllegalArgumentException.class, () -> new SpringCloudClientEventListener(config, ShenyuClientRegisterRepositoryFactory.newInstance(mockRegisterCenter), env));
        when(env.getProperty("spring.application.name")).thenReturn("spring-cloud-test");
        return new SpringCloudClientEventListener(config, ShenyuClientRegisterRepositoryFactory.newInstance(mockRegisterCenter), env);
    }

    @RestController
    @RequestMapping("/order")
    static class SpringCloudClientTestBean {
        @PostMapping("/save")
        @ShenyuSpringCloudClient(path = "/order/save")
        public String save(@RequestBody final String body) {
            return "" + body;
        }

        @PostMapping("/update")
        @ShenyuSpringCloudClient(path = "")
        public String update(@RequestBody final String body) {
            return "" + body;
        }
    }

}
