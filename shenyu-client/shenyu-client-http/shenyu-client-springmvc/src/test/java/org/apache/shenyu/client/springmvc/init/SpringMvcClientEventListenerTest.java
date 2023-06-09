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

package org.apache.shenyu.client.springmvc.init;

import org.apache.shenyu.client.core.exception.ShenyuClientIllegalArgumentException;
import org.apache.shenyu.client.core.register.ShenyuClientRegisterRepositoryFactory;
import org.apache.shenyu.client.springmvc.annotation.ShenyuSpringMvcClient;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.client.core.utils.PortUtils;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.client.http.utils.RegisterUtils;
import org.apache.shenyu.register.common.config.PropertiesConfig;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.junit.Assert;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.stubbing.Answer;
import org.springframework.beans.factory.config.AutowireCapableBeanFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;
import java.util.Properties;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test for {@link SpringMvcClientEventListener}.
 */
@ExtendWith(MockitoExtension.class)
public class SpringMvcClientEventListenerTest {

    private final MockedStatic<RegisterUtils> registerUtilsMockedStatic = mockStatic(RegisterUtils.class);

    private final SpringMvcClientTestBean springMvcClientTestBean = new SpringMvcClientTestBean();

    private final SpringMvcClientTestBean2 springMvcClientTestBean2 = new SpringMvcClientTestBean2();

    private final SpringMvcClientTestBean3 springMvcClientTestBean3 = new SpringMvcClientTestBean3();

    private final SpringMvcClientTestBean4 springMvcClientTestBean4 = new SpringMvcClientTestBean4();

    @Mock
    private ApplicationContext applicationContext;
    
    @Mock
    private AutowireCapableBeanFactory beanFactory;

    private ContextRefreshedEvent contextRefreshedEvent;

    private void init() {
        Map<String, Object> results = new LinkedHashMap<>();
        results.put("springMvcClientTestBean", springMvcClientTestBean);
        results.put("springMvcClientTestBean2", springMvcClientTestBean2);
        results.put("springMvcClientTestBean3", springMvcClientTestBean3);
        results.put("springMvcClientTestBean4", springMvcClientTestBean4);
        when(applicationContext.getBeansWithAnnotation(any())).thenReturn(results);
        contextRefreshedEvent = new ContextRefreshedEvent(applicationContext);
        Properties properties = mock(Properties.class);

        PropertiesConfig clientConfig = mock(PropertiesConfig.class);
        when(clientConfig.getProps()).thenReturn(properties);
        Assert.assertThrows(ShenyuClientIllegalArgumentException.class, () -> new SpringMvcClientEventListener(clientConfig, mock(ShenyuClientRegisterRepository.class)));
    }

    @Test
    public void testShenyuBeanProcess() {
        registerUtilsMockedStatic.when(() -> RegisterUtils.doLogin(any(), any(), any())).thenReturn(Optional.of("token"));
        // config with full
        SpringMvcClientEventListener springMvcClientEventListener = buildSpringMvcClientEventListener(true, true);
        springMvcClientEventListener.onApplicationEvent(new ContextRefreshedEvent(applicationContext));
        verify(applicationContext, never()).getBeansWithAnnotation(any());
        registerUtilsMockedStatic.close();
    }

    @Test
    public void testNormalBeanProcess() {
        init();
        registerUtilsMockedStatic.when(() -> RegisterUtils.doLogin(any(), any(), any())).thenReturn(Optional.of("token"));
        SpringMvcClientEventListener springMvcClientEventListener = buildSpringMvcClientEventListener(false, true);
        springMvcClientEventListener.onApplicationEvent(contextRefreshedEvent);
        verify(applicationContext, times(2)).getBeansWithAnnotation(any());
        registerUtilsMockedStatic.close();
    }

    @Test
    public void testWithShenyuClientAnnotation() {
        init();
        registerUtilsMockedStatic.when(() -> RegisterUtils.doLogin(any(), any(), any())).thenReturn(Optional.of("token"));
        registerUtilsMockedStatic.when(() -> RegisterUtils.doRegister(any(), any(), any()))
                .thenAnswer((Answer<Void>) invocation -> null);
        SpringMvcClientEventListener springMvcClientEventListener = buildSpringMvcClientEventListener(false, true);
        springMvcClientEventListener.onApplicationEvent(contextRefreshedEvent);
        verify(applicationContext, times(2)).getBeansWithAnnotation(any());
        registerUtilsMockedStatic.close();
    }

    private SpringMvcClientEventListener buildSpringMvcClientEventListener(final boolean full, final boolean port) {
        Properties properties = new Properties();
        properties.setProperty("contextPath", "/mvc");
        properties.setProperty("isFull", full + "");
        properties.setProperty("ip", "127.0.0.1");
        if (port) {
            properties.setProperty("port", "8289");
        }
        properties.setProperty("username", "admin");
        properties.setProperty("password", "123456");
        PropertiesConfig config = new PropertiesConfig();
        config.setProps(properties);
        ShenyuRegisterCenterConfig mockRegisterCenter = new ShenyuRegisterCenterConfig();
        mockRegisterCenter.setServerLists("http://127.0.0.1:9095");
        mockRegisterCenter.setRegisterType("http");
        mockRegisterCenter.setProps(properties);
        return new SpringMvcClientEventListener(config, ShenyuClientRegisterRepositoryFactory.newInstance(mockRegisterCenter));
    }
    
    @Test
    public void testOnApplicationEvent() {
        init();
        SpringMvcClientEventListener springMvcClientEventListener = buildSpringMvcClientEventListener(false, false);
        when(applicationContext.getAutowireCapableBeanFactory()).thenReturn(beanFactory);
        MockedStatic<PortUtils> portUtilsMockedStatic = mockStatic(PortUtils.class);
        portUtilsMockedStatic.when(() -> PortUtils.findPort(beanFactory)).thenReturn(8080);
        springMvcClientEventListener.onApplicationEvent(contextRefreshedEvent);
    
        // hit `!registered.compareAndSet(false, true)`
        springMvcClientEventListener.onApplicationEvent(contextRefreshedEvent);
        portUtilsMockedStatic.close();
        registerUtilsMockedStatic.close();
    }
    
    @Test
    public void testOnApplicationEventError() {
        init();
        SpringMvcClientEventListener springMvcClientEventListener = buildSpringMvcClientEventListener(false, false);
        Assert.assertThrows(ShenyuException.class, () -> springMvcClientEventListener.onApplicationEvent(contextRefreshedEvent));
        registerUtilsMockedStatic.close();
    }
    
    @RestController
    @RequestMapping("/order")
    @ShenyuSpringMvcClient(path = "/order")
    static class SpringMvcClientTestBean {

        @GetMapping("/hello")
        @ShenyuSpringMvcClient(path = "/hello")
        public String hello(@RequestBody final String input) {
            return "hello:" + input;
        }

        @GetMapping("/hello2")
        @ShenyuSpringMvcClient(path = "")
        public String hello2(@RequestBody final String input) {
            return "hello:" + input;
        }

        @GetMapping("")
        public String hello3(@RequestBody final String input) {
            return "hello:" + input;
        }
    }

    @RestController
    @RequestMapping("/hello2/*")
    static class SpringMvcClientTestBean2 {
        public String test(final String hello) {
            return hello + "";
        }
    }

    @RestController
    static class SpringMvcClientTestBean3 {
        public String test(final String hello) {
            return hello + "";
        }
    }

    @RestController
    @ShenyuSpringMvcClient(path = "/order/*")
    static class SpringMvcClientTestBean4 {
        public String test(final String hello) {
            return hello + "";
        }
    }

}
