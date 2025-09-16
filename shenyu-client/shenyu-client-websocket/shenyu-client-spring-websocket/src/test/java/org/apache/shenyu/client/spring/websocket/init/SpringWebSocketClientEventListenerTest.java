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

package org.apache.shenyu.client.spring.websocket.init;

import org.apache.shenyu.client.core.constant.ShenyuClientConstants;
import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.spring.websocket.annotation.ShenyuSpringWebSocketClient;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.config.PropertiesConfig;
import org.apache.shenyu.register.common.config.ShenyuClientConfig;
import org.apache.shenyu.register.common.config.ShenyuClientConfig.ClientPropertiesConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.context.ApplicationContext;
import org.springframework.util.ReflectionUtils;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test for {@link SpringWebSocketClientEventListener}.
 */
@ExtendWith(MockitoExtension.class)
public class SpringWebSocketClientEventListenerTest {

    private static final String SUPER_PATH = "/superPath";

    @Mock
    private PropertiesConfig propertiesConfig;

    @Mock
    private ShenyuClientRegisterRepository registerRepository;

    @Mock
    private ShenyuClientRegisterEventPublisher publisher;

    @Mock
    private ApplicationContext applicationContext;

    @Mock
    private SpringWebSocketClientEventListener eventListener;

    @Mock
    private ShenyuSpringWebSocketClient annotation;

    @Mock
    private MockClass mockClass;

    @BeforeEach
    public void setUp() {
        MockitoAnnotations.openMocks(this);
        Properties properties = mock(Properties.class);
        when(properties.getProperty("appName")).thenReturn("appName");
        when(properties.getProperty("contextPath")).thenReturn("contextPath");
        when(properties.getProperty(ShenyuClientConstants.PORT)).thenReturn("8080");
        when(properties.getProperty(ShenyuClientConstants.HOST)).thenReturn("127.0.0.1");
        when(properties.getProperty(ShenyuClientConstants.IP_PORT)).thenReturn("127.0.0.1:8080");
        
        ShenyuClientConfig clientConfig = mock(ShenyuClientConfig.class);
        Map<String, ClientPropertiesConfig> client = new HashMap<>();
        ClientPropertiesConfig clientPropertiesConfig = new ClientPropertiesConfig();
        clientPropertiesConfig.setProps(properties);
        client.put(RpcTypeEnum.WEB_SOCKET.getName(), clientPropertiesConfig);
        when(clientConfig.getClient()).thenReturn(client);
        eventListener = new SpringWebSocketClientEventListener(clientConfig, registerRepository);
    }

    @Test
    public void testBuildApiDocSextet() throws NoSuchMethodException {
        Method method = MockClass.class.getDeclaredMethod("mockMethod");
        ReflectionUtils.makeAccessible(method);
        assertNull(eventListener.buildApiDocSextet(method, mock(Annotation.class), Collections.emptyMap()));
    }

    @Test
    public void testGetBeans() {
        Map<String, Object> beans = eventListener.getBeans(applicationContext);
        assertNotNull(beans);
        verify(publisher, never()).publishEvent(any());
    }

    @Test
    public void testBuildURIRegisterDTO() {
        URIRegisterDTO uriRegisterDTO = eventListener.buildURIRegisterDTO(applicationContext, Collections.emptyMap(), Constants.SYS_DEFAULT_NAMESPACE_ID);
        assertNotNull(uriRegisterDTO);
        assertEquals("/contextPath", uriRegisterDTO.getContextPath());
        assertEquals("appName", uriRegisterDTO.getAppName());
        assertEquals("127.0.0.1", uriRegisterDTO.getHost());
        assertEquals(8080, uriRegisterDTO.getPort());
    }

    @Test
    public void testHandle() {
        eventListener.handle("mock", mockClass);
    }

    @Test
    public void testBuildApiSuperPath() {
        String annotationPath = "/path";
        when(annotation.path()).thenReturn(annotationPath);
        String path = eventListener.buildApiSuperPath(MockClass.class, annotation);
        assertNotNull(path);
        assertEquals(annotationPath, path);
    }

    @Test
    public void testHandleClass() {
        Class<MockClass> clazz = MockClass.class;
        eventListener.handleClass(clazz, mockClass, annotation, SUPER_PATH);
    }

    @Test
    public void testHandleMethod() throws NoSuchMethodException {
        Method method = mockClass.getClass().getMethod("mockMethod");
        eventListener.handleMethod(mockClass, MockClass.class, annotation, method, SUPER_PATH);
    }

    @Test
    public void testGetAnnotationType() {
        Class<ShenyuSpringWebSocketClient> annotationType = eventListener.getAnnotationType();
        assertEquals(annotationType, ShenyuSpringWebSocketClient.class);
    }

    @Test
    public void testBuildApiPath() throws NoSuchMethodException {
        Method method = mockClass.getClass().getMethod("mockMethod");
        String apiPath = eventListener.buildApiPath(method, SUPER_PATH, annotation);
        assertNotNull(apiPath);
    }

    @Test
    public void testBuildMetaDataDTO() throws NoSuchMethodException {
        Method method = mockClass.getClass().getMethod("mockMethod");
        MetaDataRegisterDTO metaDataRegisterDTO = eventListener.buildMetaDataDTO(mockClass, annotation, SUPER_PATH, MockClass.class, method, Constants.SYS_DEFAULT_NAMESPACE_ID);
        assertNotNull(metaDataRegisterDTO);
    }

    @Test
    public void testGetPort() {
        String port = eventListener.getPort();
        assertNotNull(port);
        assertEquals(port, "8080");
    }

    /**
     * class for mock.
     */
    @ShenyuSpringWebSocketClient
    private static class MockClass {
        public void mockMethod() {
        }
    }

}
