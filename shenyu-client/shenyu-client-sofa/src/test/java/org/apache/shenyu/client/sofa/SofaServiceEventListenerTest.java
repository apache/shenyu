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

package org.apache.shenyu.client.sofa;

import com.alipay.sofa.runtime.spring.factory.ServiceFactoryBean;
import org.apache.shenyu.client.core.register.ShenyuClientRegisterRepositoryFactory;
import org.apache.shenyu.client.sofa.common.annotation.ShenyuSofaClient;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.register.common.config.ShenyuClientConfig;
import org.apache.shenyu.register.common.config.ShenyuClientConfig.ClientPropertiesConfig;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.enums.EventType;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.context.ApplicationContext;
import org.springframework.context.event.ContextRefreshedEvent;

import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import static org.mockito.BDDMockito.given;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

/**
 * Test for {@link SofaServiceEventListener}.
 */
@ExtendWith(MockitoExtension.class)
public class SofaServiceEventListenerTest {

    private static final String CONTEXT_PATH = "/sofa";

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

    private static final String LOAD_BALANCE = "loadBalance";

    private static final int RETRY_TIME = 0;

    private static final int TIME_OUT = 0;

    private static final boolean ENABLED = true;

    @InjectMocks
    private SofaServiceEventListener sofaServiceEventListener = buildSofaServiceEventListener();

    @Mock
    private ApplicationContext applicationContext;

    @Mock
    private ShenyuSofaClient shenyuSofaClient;

    @Mock
    private Method method;

    @Mock
    private ServiceFactoryBean serviceFactoryBean;

    @Mock
    private ContextRefreshedEvent contextRefreshedEvent;

    @Test
    public void testGetBeans() {
        sofaServiceEventListener.getBeans(applicationContext);

        verify(applicationContext, times(1)).getBeansOfType(ServiceFactoryBean.class);
    }

    @Test
    public void testBuildURIRegisterDTO() {
        URIRegisterDTO expectedURIRegisterDTO = URIRegisterDTO.builder()
                .contextPath(CONTEXT_PATH)
                .appName(APP_NAME)
                .rpcType(RpcTypeEnum.SOFA.getName())
                .eventType(EventType.REGISTER)
                .host(HOST)
                .port(Integer.parseInt(PORT))
                .namespaceId(Constants.SYS_DEFAULT_NAMESPACE_ID)
                .build();
        Map<String, ServiceFactoryBean> beans = new HashMap<>();
        URIRegisterDTO realURIRegisterDTO = sofaServiceEventListener.buildURIRegisterDTO(applicationContext, beans, Constants.SYS_DEFAULT_NAMESPACE_ID);

        assertEquals(expectedURIRegisterDTO, realURIRegisterDTO);
    }

    @Test
    public void testBuildApiSuperPathWhenBeanShenyuClientIsNull() {
        Class<?> clazz = Class.class;
        String realSuperPath = sofaServiceEventListener.buildApiSuperPath(clazz, null);

        verify(shenyuSofaClient, times(0)).path();
        assertEquals("", realSuperPath);
    }

    @Test
    public void testBuildApiSuperPathWhenBeanShenyuClientPathIsEmpty() {
        Class<?> clazz = Class.class;
        given(shenyuSofaClient.path()).willReturn("");
        String realSuperPath = sofaServiceEventListener.buildApiSuperPath(clazz, shenyuSofaClient);

        verify(shenyuSofaClient, times(1)).path();
        assertEquals("", realSuperPath);
    }

    @Test
    public void testBuildApiSuperPath() {
        Class<?> clazz = Class.class;
        given(shenyuSofaClient.path()).willReturn(PATH);
        String realSuperPath = sofaServiceEventListener.buildApiSuperPath(clazz, shenyuSofaClient);

        verify(shenyuSofaClient, times(2)).path();
        assertEquals(PATH, realSuperPath);
    }

    @Test
    public void testGetAnnotationType() {
        Class<?> clazz = sofaServiceEventListener.getAnnotationType();

        assertEquals(ShenyuSofaClient.class, clazz);
    }

    @Test
    public void testBuildApiPathSuperPathContainsStar() {
        given(method.getName()).willReturn(METHOD_NAME);
        String realApiPath = sofaServiceEventListener.buildApiPath(method, SUPER_PATH_CONTAINS_STAR, shenyuSofaClient);
        String expectedApiPath = "/sofa/demo/buildURIRegisterDTO";

        assertEquals(expectedApiPath, realApiPath);
    }

    @Test
    public void testBuildApiPathSuperPathNotContainsStar() {
        given(shenyuSofaClient.path()).willReturn(PATH);
        String realApiPath = sofaServiceEventListener.buildApiPath(method, SUPER_PATH_NOT_CONTAINS_STAR, shenyuSofaClient);
        String expectedApiPath = "/sofa/findByIdsAndName/path";

        assertEquals(expectedApiPath, realApiPath);
    }

    @Test
    public void testBuildMetaDataDTO() throws NoSuchMethodException {
        Method method = SofaServiceEventListener
                .class
                .getDeclaredMethod(METHOD_NAME, ApplicationContext.class, Map.class, String.class);
        given(shenyuSofaClient.path()).willReturn(PATH);
        given(shenyuSofaClient.desc()).willReturn(DESC);
        given(shenyuSofaClient.ruleName()).willReturn(CONFIG_RULE_NAME);
        given(shenyuSofaClient.loadBalance()).willReturn(LOAD_BALANCE);
        given(shenyuSofaClient.retries()).willReturn(RETRY_TIME);
        given(shenyuSofaClient.timeout()).willReturn(TIME_OUT);
        given(shenyuSofaClient.enabled()).willReturn(ENABLED);
        // The willReturn method cannot have the class<?> type as an input parameter.
        // Have raised the question in the Mockito community, will get back to this after getting answer
        doReturn(Comparable.class).when(serviceFactoryBean).getInterfaceClass();

        String expectedParameterTypes = "org.springframework.context.ApplicationContext,java.util.Map#java.lang.String#"
                + "com.alipay.sofa.runtime.spring.factory.ServiceFactoryBean,java.lang.String";
        String expectedPath = "/sofa/findByIdsAndName/path";
        String expectedRpcExt = "{\"loadbalance\":\"loadBalance\",\"retries\":0,\"timeout\":0}";

        MetaDataRegisterDTO realMetaDataRegisterDTO = sofaServiceEventListener
                .buildMetaDataDTO(
                        serviceFactoryBean,
                        shenyuSofaClient,
                        SUPER_PATH_NOT_CONTAINS_STAR,
                        SofaServiceEventListener.class,
                        method, Constants.SYS_DEFAULT_NAMESPACE_ID);
        MetaDataRegisterDTO expectedMetaDataRegisterDTO = MetaDataRegisterDTO
                .builder()
                .appName(APP_NAME)
                .serviceName(SERVICE_NAME)
                .methodName(METHOD_NAME)
                .contextPath(CONTEXT_PATH)
                .host(HOST)
                .port(Integer.parseInt(PORT))
                .path(expectedPath)
                .ruleName(CONFIG_RULE_NAME)
                .pathDesc(DESC)
                .parameterTypes(expectedParameterTypes)
                .rpcType(RpcTypeEnum.SOFA.getName())
                .rpcExt(expectedRpcExt)
                .enabled(ENABLED)
                .namespaceId(Constants.SYS_DEFAULT_NAMESPACE_ID)
                .build();

        assertEquals(expectedMetaDataRegisterDTO, realMetaDataRegisterDTO);
    }

    private SofaServiceEventListener buildSofaServiceEventListener() {
        Properties properties = new Properties();
        properties.setProperty("contextPath", CONTEXT_PATH);
        properties.setProperty("port", PORT);
        properties.setProperty("host", HOST);
        properties.setProperty("username", USERNAME);
        properties.setProperty("password", PASSWORD);
        properties.setProperty("appName", APP_NAME);
        ClientPropertiesConfig config = new ClientPropertiesConfig();
        config.setProps(properties);
        
        ShenyuClientConfig clientConfig = new ShenyuClientConfig();
        Map<String, ClientPropertiesConfig> client = new HashMap<>();
        client.put(RpcTypeEnum.SOFA.getName(), config);
        clientConfig.setClient(client);

        ShenyuRegisterCenterConfig mockRegisterCenter = new ShenyuRegisterCenterConfig();
        mockRegisterCenter.setServerLists("http://localhost:58080");
        mockRegisterCenter.setRegisterType("http");
        mockRegisterCenter.setProps(properties);

        return new SofaServiceEventListener(clientConfig, ShenyuClientRegisterRepositoryFactory.newInstance(mockRegisterCenter));
    }

}
