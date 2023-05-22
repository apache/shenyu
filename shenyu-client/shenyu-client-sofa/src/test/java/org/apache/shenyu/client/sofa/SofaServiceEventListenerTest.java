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
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.register.common.config.PropertiesConfig;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.context.ApplicationContext;

import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import static org.mockito.BDDMockito.given;
import static org.junit.jupiter.api.Assertions.assertEquals;
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

    @InjectMocks
    private SofaServiceEventListener sofaServiceEventListener = buildSofaServiceEventListener();

    @Mock
    private ApplicationContext applicationContext;

    @Mock
    private ShenyuSofaClient shenyuSofaClient;

    @Test
    public void testGetBeans() {
        sofaServiceEventListener.getBeans(applicationContext);

        verify(applicationContext, times(1)).getBeansOfType(ServiceFactoryBean.class);
    }

    @Test
    public void testBuildURIRegisterDTO() {
        URIRegisterDTO expectedURIRegisterDTO = URIRegisterDTO.builder()
                .contextPath(CONTEXT_PATH)
                .appName(null)
                .rpcType(RpcTypeEnum.SOFA.getName())
                .host(HOST)
                .port(Integer.parseInt(PORT))
                .build();
        Map<String, ServiceFactoryBean> beans = new HashMap<>();
        URIRegisterDTO realURIRegisterDTO = sofaServiceEventListener.buildURIRegisterDTO(applicationContext, beans);

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

    private SofaServiceEventListener buildSofaServiceEventListener() {
        Properties properties = new Properties();
        properties.setProperty("contextPath", CONTEXT_PATH);
        properties.setProperty("port", PORT);
        properties.setProperty("host", HOST);
        properties.setProperty("username", USERNAME);
        properties.setProperty("password", PASSWORD);
        PropertiesConfig config = new PropertiesConfig();
        config.setProps(properties);

        ShenyuRegisterCenterConfig mockRegisterCenter = new ShenyuRegisterCenterConfig();
        mockRegisterCenter.setServerLists("http://localhost:58080");
        mockRegisterCenter.setRegisterType("http");
        mockRegisterCenter.setProps(properties);

        return new SofaServiceEventListener(config, ShenyuClientRegisterRepositoryFactory.newInstance(mockRegisterCenter));
    }

}
