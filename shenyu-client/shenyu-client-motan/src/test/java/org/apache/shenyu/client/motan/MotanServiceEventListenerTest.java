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

package org.apache.shenyu.client.motan;

import com.weibo.api.motan.config.springsupport.BasicServiceConfigBean;
import com.weibo.api.motan.config.springsupport.annotation.MotanService;
import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Properties;
import org.apache.shenyu.client.core.register.ShenyuClientRegisterRepositoryFactory;
import org.apache.shenyu.client.motan.common.annotation.ShenyuMotanClient;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.register.common.config.ShenyuClientConfig;
import org.apache.shenyu.register.common.config.ShenyuClientConfig.ClientPropertiesConfig;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.enums.EventType;
import org.jetbrains.annotations.NotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import static org.mockito.BDDMockito.given;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.context.ApplicationContext;
import org.springframework.util.ReflectionUtils;

/**
 * Test for {@link MotanServiceEventListener}.
 */
@ExtendWith(MockitoExtension.class)
public final class MotanServiceEventListenerTest {

    private static final String CONTEXT_PATH = "/motan";

    private static final String PORT = "8080";

    private static final String HOST = "127.0.0.1";

    private static final String USERNAME = "admin";

    private static final String PASSWORD = "123456";

    private static final String PATH = "path";

    private static final String APP_NAME = "appName";

    private static final String SUPER_PATH_CONTAINS_STAR = "/demo/**";

    private static final String SUPER_PATH_NOT_CONTAINS_STAR = "/findByIdsAndName";

    private static final String METHOD_NAME = "buildURIRegisterDTO";

    private static final String SERVICE_NAME = "java.lang.Comparable";

    private static final String DESC = "desc";

    private static final String CONFIG_RULE_NAME = "configRuleName";

    private static final boolean ENABLED = true;

    @InjectMocks
    private MotanServiceEventListener motanServiceEventListener = buildMotanServiceEventListener();

    @Mock
    private ApplicationContext applicationContext;

    @Mock
    private ShenyuMotanClient shenyuMotanClient;

    @Mock
    private Method method;

    @Mock
    private BasicServiceConfigBean basicServiceConfigBean;

    @Test
    public void testGetBeans() {
        given(applicationContext.getBean(MotanServiceEventListener.BASE_SERVICE_CONFIG)).willReturn(basicServiceConfigBean);
        given(basicServiceConfigBean.getGroup()).willReturn("testGroup");
        Map<String, Object> mockBeans = new HashMap<>();
        mockBeans.put("bean1", new Object());
        given(applicationContext.getBeansWithAnnotation(ShenyuMotanClient.class)).willReturn(mockBeans);
        Map<String, Object> result = motanServiceEventListener.getBeans(applicationContext);

        assertEquals(mockBeans, result);
        verify(applicationContext, times(1)).getBean(MotanServiceEventListener.BASE_SERVICE_CONFIG);
        verify(applicationContext, times(1)).getBeansWithAnnotation(ShenyuMotanClient.class);
    }

    @Test
    public void testBuildURIRegisterDTO() {
        URIRegisterDTO expectedURIRegisterDTO = URIRegisterDTO.builder()
                .contextPath(CONTEXT_PATH)
                .appName(APP_NAME)
                .rpcType(RpcTypeEnum.MOTAN.getName())
                .eventType(EventType.REGISTER)
                .host(HOST)
                .port(Integer.parseInt(PORT))
                .namespaceId(Constants.SYS_DEFAULT_NAMESPACE_ID)
                .build();
        Map<String, Object> beans = new HashMap<>();
        URIRegisterDTO realURIRegisterDTO = motanServiceEventListener.buildURIRegisterDTO(applicationContext, beans, Constants.SYS_DEFAULT_NAMESPACE_ID);

        assertEquals(expectedURIRegisterDTO, realURIRegisterDTO);
    }

    @Test
    public void testBuildApiSuperPathWhenShenyuMotanClientIsNull() {
        Class<?> clazz = Class.class;
        String realSuperPath = motanServiceEventListener.buildApiSuperPath(clazz, null);

        verify(shenyuMotanClient, times(0)).path();
        assertEquals("", realSuperPath);
    }

    @Test
    public void testBuildApiSuperPathWhenShenyuMotanClientPathIsEmpty() {
        Class<?> clazz = Class.class;
        given(shenyuMotanClient.path()).willReturn("");
        String realSuperPath = motanServiceEventListener.buildApiSuperPath(clazz, shenyuMotanClient);

        verify(shenyuMotanClient, times(1)).path();
        assertEquals("", realSuperPath);
    }

    @Test
    public void testBuildApiSuperPath() {
        Class<?> clazz = Class.class;
        given(shenyuMotanClient.path()).willReturn(PATH);
        String realSuperPath = motanServiceEventListener.buildApiSuperPath(clazz, shenyuMotanClient);

        verify(shenyuMotanClient, times(2)).path();
        assertEquals(PATH, realSuperPath);
    }

    @Test
    public void testGetAnnotationType() {
        Class<?> clazz = motanServiceEventListener.getAnnotationType();

        assertEquals(ShenyuMotanClient.class, clazz);
    }

    @Test
    public void testBuildMetaDataDTOForMotan() throws NoSuchMethodException {
        given(shenyuMotanClient.desc()).willReturn(DESC);
        given(shenyuMotanClient.ruleName()).willReturn(CONFIG_RULE_NAME);
        given(shenyuMotanClient.enabled()).willReturn(ENABLED);
        given(basicServiceConfigBean.getRequestTimeout()).willReturn(1000);
        given(applicationContext.getBean(MotanServiceEventListener.BASE_SERVICE_CONFIG)).willReturn(basicServiceConfigBean);

        final String expectedParameterTypes = "org.springframework.context.ApplicationContext,java.util.Map,java.lang.String";
        final String expectedRpcExt = "{\"methodInfo\":[{\"methodName\":\"buildURIRegisterDTO\","
                + "\"params\":[{\"left\":\"org.springframework.context.ApplicationContext\",\"right\":\"context\"},"
                + "{\"left\":\"java.util.Map\",\"right\":\"beans\"},{\"left\":\"java.lang.String\",\"right\":\"namespaceId\"}]}],\"timeout\":1000,\"rpcProtocol\":\"motan2\"}";
        Method method = MotanServiceEventListener.class.getDeclaredMethod(METHOD_NAME, ApplicationContext.class, Map.class, String.class);

        MetaDataRegisterDTO realMetaDataRegisterDTO = motanServiceEventListener.buildMetaDataDTO(
                null,
                shenyuMotanClient,
                SUPER_PATH_NOT_CONTAINS_STAR,
                MockMotanServiceClass.class,
                method,
                Constants.SYS_DEFAULT_NAMESPACE_ID);
    
        MetaDataRegisterDTO expectedMetaDataRegisterDTO = MetaDataRegisterDTO.builder()
            .appName(APP_NAME)
            .serviceName(SERVICE_NAME)
            .methodName(METHOD_NAME)
            .contextPath(CONTEXT_PATH)
            .path(SUPER_PATH_NOT_CONTAINS_STAR)
            .port(Integer.parseInt(PORT))
            .host(HOST)
            .ruleName(CONFIG_RULE_NAME)
            .pathDesc(DESC)
            .parameterTypes(expectedParameterTypes)
            .rpcType(RpcTypeEnum.MOTAN.getName())
            .rpcExt(expectedRpcExt)
            .enabled(ENABLED)
            .namespaceId(Constants.SYS_DEFAULT_NAMESPACE_ID)
            .build();
    
        assertEquals(expectedMetaDataRegisterDTO, realMetaDataRegisterDTO);
    }
    


    @Test
    public void testBuildApiPathSuperPathContainsStar() {
        given(method.getName()).willReturn(METHOD_NAME);
        String realApiPath = motanServiceEventListener.buildApiPath(method, SUPER_PATH_CONTAINS_STAR, shenyuMotanClient);
        String expectedApiPath = "/motan/demo/buildURIRegisterDTO";

        assertEquals(expectedApiPath, realApiPath);
    }

    @Test
    public void testBuildApiPathSuperPathNotContainsStar() {
        given(shenyuMotanClient.path()).willReturn(PATH);
        String realApiPath = motanServiceEventListener.buildApiPath(method, SUPER_PATH_NOT_CONTAINS_STAR, shenyuMotanClient);
        String expectedApiPath = "/motan/findByIdsAndName/path";

        assertEquals(expectedApiPath, realApiPath);
    }

    

    private MotanServiceEventListener buildMotanServiceEventListener() {
        Properties properties = new Properties();
        properties.setProperty("contextPath", CONTEXT_PATH);
        properties.setProperty("port", PORT);
        properties.setProperty("host", HOST);
        properties.setProperty("username", USERNAME);
        properties.setProperty("password", PASSWORD);
        properties.setProperty("appName", APP_NAME);
        ClientPropertiesConfig config = new ClientPropertiesConfig();
        config.setProps(properties);

        ShenyuRegisterCenterConfig shenyuRegisterCenterConfig = new ShenyuRegisterCenterConfig();
        shenyuRegisterCenterConfig.setServerLists("http://localhost:58080");
        shenyuRegisterCenterConfig.setRegisterType("http");
        shenyuRegisterCenterConfig.setProps(properties);
        ShenyuClientConfig clientConfig = new ShenyuClientConfig();
        Map<String, ShenyuClientConfig.ClientPropertiesConfig> client = new LinkedHashMap<>();
        client.put(RpcTypeEnum.MOTAN.getName(), config);
        clientConfig.setClient(client);
        return new MotanServiceEventListener(clientConfig, ShenyuClientRegisterRepositoryFactory.newInstance(shenyuRegisterCenterConfig));
    }

    @Test
    public void testBuildApiDocSextet() throws NoSuchMethodException {
        Method method = MockShenyuMotanClientClass.class.getDeclaredMethod("mockMethod");
        ReflectionUtils.makeAccessible(method);
        assertNull(motanServiceEventListener.buildApiDocSextet(method, mock(Annotation.class), Collections.emptyMap()));
    }

    @ShenyuMotanClient
    private static class MockShenyuMotanClientClass {
        public void mockMethod() {

        }
    }


    @MotanService(interfaceClass = Comparable.class)
    private class MockMotanServiceClass implements Comparable {
        @Override
        public int compareTo(@NotNull final Object o) {
            return 0;
        }
    }
}
