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

package org.apache.shenyu.client.tars;

import org.apache.shenyu.client.core.exception.ShenyuClientIllegalArgumentException;
import org.apache.shenyu.client.core.register.ShenyuClientRegisterRepositoryFactory;
import org.apache.shenyu.client.tars.common.annotation.ShenyuTarsClient;
import org.apache.shenyu.client.tars.common.annotation.ShenyuTarsService;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
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
import org.springframework.context.ApplicationContext;
import org.springframework.context.event.ContextRefreshedEvent;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;
import java.util.Properties;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.times;

/**
 * Test case for {@link TarsServiceBeanEventListener}.
 */
@ExtendWith(MockitoExtension.class)
@TestMethodOrder(MethodOrderer.Alphanumeric.class)
public final class TarsServiceBeanPostProcessorTest {
    private final MockedStatic<RegisterUtils> registerUtilsMockedStatic = mockStatic(RegisterUtils.class);

    private final TarsDemoService tarsDemoService = new TarsDemoService();

    private final TarsDemoService2 tarsDemoService2 = new TarsDemoService2();

    private final TarsDemoService3 tarsDemoService3 = new TarsDemoService3();

    @Mock
    private ApplicationContext applicationContext;

    private ContextRefreshedEvent contextRefreshedEvent;

    @BeforeEach
    public void init() {
        Map<String, Object> results = new LinkedHashMap();
        results.put("tarsDemoService", tarsDemoService);
        results.put("tarsDemoService2", tarsDemoService2);
        results.put("tarsDemoService3", tarsDemoService3);
        when(applicationContext.getBeansWithAnnotation(any())).thenReturn(results);
        contextRefreshedEvent = new ContextRefreshedEvent(applicationContext);

        Properties properties = mock(Properties.class);
        PropertiesConfig clientConfig = mock(PropertiesConfig.class);
        when(clientConfig.getProps()).thenReturn(properties);
        Assert.assertThrows(ShenyuClientIllegalArgumentException.class, () -> new TarsServiceBeanEventListener(clientConfig, mock(ShenyuClientRegisterRepository.class)));
    }

    @Test
    public void testPostProcessAfterInitialization() {
        registerUtilsMockedStatic.when(() -> RegisterUtils.doLogin(any(), any(), any())).thenReturn(Optional.of("token"));
        TarsServiceBeanEventListener tarsServiceBeanEventListener = buildTarsServiceBeanEventListener(true);
        tarsServiceBeanEventListener.onApplicationEvent(contextRefreshedEvent);
        verify(applicationContext, times(1)).getBeansWithAnnotation(any());
        registerUtilsMockedStatic.close();
    }

    @Test
    public void testPostProcessNormalBean() {
        registerUtilsMockedStatic.when(() -> RegisterUtils.doLogin(any(), any(), any())).thenReturn(Optional.of("token"));
        TarsServiceBeanEventListener tarsServiceBeanEventListener = buildTarsServiceBeanEventListener(false);
        tarsServiceBeanEventListener.onApplicationEvent(contextRefreshedEvent);
        verify(applicationContext, times(1)).getBeansWithAnnotation(any());
        registerUtilsMockedStatic.close();
    }

    private TarsServiceBeanEventListener buildTarsServiceBeanEventListener(final boolean full) {
        Properties properties = new Properties();
        properties.setProperty("contextPath", "/tars");
        properties.setProperty("port", "8080");
        properties.setProperty("host", "localhost");
        properties.setProperty("username", "admin");
        properties.setProperty("password", "123456");
        PropertiesConfig config = new PropertiesConfig();
        config.setProps(properties);

        ShenyuRegisterCenterConfig mockRegisterCenter = new ShenyuRegisterCenterConfig();
        mockRegisterCenter.setServerLists("http://localhost:58080");
        mockRegisterCenter.setRegisterType("http");
        mockRegisterCenter.setProps(properties);

        return new TarsServiceBeanEventListener(config, ShenyuClientRegisterRepositoryFactory.newInstance(mockRegisterCenter));
    }

    @ShenyuTarsService(serviceName = "testObj")
    static class TarsDemoService {
        @ShenyuTarsClient("hello")
        public String test(final String hello) {
            return hello + "";
        }

        @ShenyuTarsClient("hello2/*")
        public String test2(final String hello) {
            return hello + "";
        }
    }

    @ShenyuTarsService(serviceName = "testObj2")
    @ShenyuTarsClient("hello2/*")
    static class TarsDemoService2 {
        public String test(final String hello) {
            return hello + "";
        }
    }

    @ShenyuTarsService(serviceName = "testObj3")
    @ShenyuTarsClient
    static class TarsDemoService3 {
        public String test(final String hello) {
            return hello + "";
        }
    }
}
