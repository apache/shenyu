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

package org.apache.shenyu.client.brpc;

import com.baidu.cloud.starlight.springcloud.server.annotation.RpcService;
import org.apache.shenyu.client.brpc.common.annotation.ShenyuBrpcClient;
import org.apache.shenyu.client.core.register.ShenyuClientRegisterRepositoryFactory;
import org.apache.shenyu.register.client.http.utils.RegisterUtils;
import org.apache.shenyu.register.common.config.PropertiesConfig;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.stubbing.Answer;
import org.springframework.context.ApplicationContext;
import org.springframework.context.event.ContextRefreshedEvent;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;
import java.util.Properties;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test for {@link BrpcContextRefreshedEventListener}.
 */
@ExtendWith(MockitoExtension.class)
public class BrpcClientEventListenerTest {
    
    private final MockedStatic<RegisterUtils> registerUtilsMockedStatic = mockStatic(RegisterUtils.class);
    
    private final BrpcClientTestBean brpcClientTestBean = new BrpcClientTestBean();
    
    @Mock
    private ApplicationContext applicationContext;
    
    private ContextRefreshedEvent contextRefreshedEvent;
    
    @BeforeEach
    public void init() {
        Map<String, Object> results = new LinkedHashMap<>();
        results.put("brpcClientTestBean", brpcClientTestBean);
        when(applicationContext.getBeansWithAnnotation(any())).thenReturn(results);
        contextRefreshedEvent = new ContextRefreshedEvent(applicationContext);
    }
    
    @Test
    public void testNormalBeanProcess() {
        registerUtilsMockedStatic.when(() -> RegisterUtils.doLogin(any(), any(), any())).thenReturn(Optional.of("token"));
        BrpcContextRefreshedEventListener springMvcClientEventListener = buildBrpcClientEventListener();
        springMvcClientEventListener.onApplicationEvent(contextRefreshedEvent);
        verify(applicationContext, times(2)).getBeansWithAnnotation(any());
        registerUtilsMockedStatic.close();
    }
    
    @Test
    public void testWithShenyuClientAnnotation() {
        registerUtilsMockedStatic.when(() -> RegisterUtils.doLogin(any(), any(), any())).thenReturn(Optional.of("token"));
        registerUtilsMockedStatic.when(() -> RegisterUtils.doRegister(any(), any(), any()))
                .thenAnswer((Answer<Void>) invocation -> null);
        BrpcContextRefreshedEventListener springMvcClientEventListener = buildBrpcClientEventListener();
        springMvcClientEventListener.onApplicationEvent(contextRefreshedEvent);
        verify(applicationContext, times(2)).getBeansWithAnnotation(any());
        registerUtilsMockedStatic.close();
    }
    
    private BrpcContextRefreshedEventListener buildBrpcClientEventListener() {
        Properties properties = new Properties();
        properties.setProperty("appName", "brpc");
        properties.setProperty("contextPath", "/brpc");
        properties.setProperty("ipAndPort", "127.0.0.1:21715");
        properties.setProperty("host", "127.0.0.1");
        properties.setProperty("port", "21715");
        properties.setProperty("username", "admin");
        properties.setProperty("password", "123456");
        PropertiesConfig config = new PropertiesConfig();
        config.setProps(properties);
        ShenyuRegisterCenterConfig mockRegisterCenter = new ShenyuRegisterCenterConfig();
        mockRegisterCenter.setServerLists("http://127.0.0.1:9095");
        mockRegisterCenter.setRegisterType("http");
        mockRegisterCenter.setProps(properties);
        return new BrpcContextRefreshedEventListener(config, ShenyuClientRegisterRepositoryFactory.newInstance(mockRegisterCenter));
    }

    @RpcService
    static class BrpcClientTestBean {
        
        @ShenyuBrpcClient("/hello")
        public String hello(final String input) {
            return "hello:" + input;
        }
    }
    
}
