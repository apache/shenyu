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

package org.apache.shenyu.register.client.nacos;

import com.alibaba.nacos.api.PropertyKeyConst;
import com.alibaba.nacos.api.config.ConfigFactory;
import com.alibaba.nacos.api.config.ConfigService;
import com.alibaba.nacos.api.exception.NacosException;
import com.alibaba.nacos.api.naming.NamingFactory;
import com.alibaba.nacos.api.naming.NamingService;
import com.alibaba.nacos.api.naming.pojo.Instance;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import java.lang.reflect.Field;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

/**
 * Test for nacos register center client.
 */
public class NacosClientRegisterRepositoryTest {

    private NacosClientRegisterRepository repository;

    private final Map<String, Object> nacosBroker = new HashMap<>();

    private ConfigService configService;

    private NamingService namingService;

    @BeforeEach
    public void setUp() throws IllegalAccessException, NoSuchFieldException, NacosException {
        this.repository = new NacosClientRegisterRepository();
        this.configService = mockConfigService();
        this.namingService = mockNamingService();
        Class<? extends NacosClientRegisterRepository> clazz = this.repository.getClass();

        String configFiledStr = "configService";
        Field configFiled = clazz.getDeclaredField(configFiledStr);
        configFiled.setAccessible(true);
        configFiled.set(repository, this.configService);

        String namingFiledStr = "namingService";
        Field namingFiled = clazz.getDeclaredField(namingFiledStr);
        namingFiled.setAccessible(true);
        namingFiled.set(repository, this.namingService);

        nacosBroker.clear();
    }

    private ConfigService mockConfigService() throws NacosException {
        ConfigService configService = mock(ConfigService.class);

        doAnswer(invocationOnMock -> {
            String key = invocationOnMock.getArgument(0);
            String value = invocationOnMock.getArgument(2);
            nacosBroker.put(key, value);
            return true;
        }).when(configService).publishConfig(anyString(), anyString(), anyString(), anyString());

        return configService;
    }

    private NamingService mockNamingService() throws NacosException {
        NamingService namingService = mock(NamingService.class);

        doAnswer(invocationOnMock -> {
            String key = invocationOnMock.getArgument(0);
            Instance instance = invocationOnMock.getArgument(1);
            nacosBroker.put(key, instance);
            return null;
        }).when(namingService).registerInstance(anyString(), any());

        return namingService;
    }

    @Test
    public void testClose() throws NacosException {
        repository.close();
        verify(configService, times(1)).shutDown();
        verify(namingService, times(1)).shutDown();
    }

    @Test
    public void testPersistInterface() throws NacosException {
        final MetaDataRegisterDTO data = MetaDataRegisterDTO.builder()
                .rpcType("http")
                .host("host")
                .port(80)
                .contextPath("/context")
                .ruleName("ruleName")
                .build();
        repository.persistInterface(data);
        String configPath = "shenyu.register.service.http.context";
        assertTrue(nacosBroker.containsKey(configPath));
        String dataStr = GsonUtils.getInstance().toJson(data);
        assertEquals(nacosBroker.get(configPath), GsonUtils.getInstance().toJson(Collections.singletonList(dataStr)));

        doThrow(NacosException.class).when(configService).publishConfig(any(), any(), any(), any());
        Assertions.assertThrows(ShenyuException.class, () -> repository.persistInterface(data));

        doReturn(false).when(configService).publishConfig(any(), any(), any(), any());
        Assertions.assertThrows(ShenyuException.class, () -> repository.persistInterface(data));
    }

    @Test
    public void testPersistUri() throws NacosException {
        final URIRegisterDTO data = URIRegisterDTO.builder()
                .rpcType("http")
                .host("host")
                .port(80)
                .contextPath("/context")
                .build();
        repository.persistURI(data);
        String uriInstancePath = "shenyu.register.service.http";
        assertTrue(nacosBroker.containsKey(uriInstancePath));
        Instance instance = (Instance) nacosBroker.get(uriInstancePath);
        assertEquals(instance.getIp(), data.getHost());
        Map<String, String> metadataMap = instance.getMetadata();
        assertEquals(metadataMap.get("uriMetadata"), GsonUtils.getInstance().toJson(data));

        doThrow(NacosException.class).when(namingService).registerInstance(any(), any());
        Assertions.assertThrows(ShenyuException.class, () -> repository.persistURI(data));
    }

    @Test
    public void initTest() throws NacosException {
        ShenyuRegisterCenterConfig config = new ShenyuRegisterCenterConfig();
        config.setServerLists("http://localhost:8089");
        Properties configProps = config.getProps();
        configProps.setProperty("nacosNameSpace", PropertyKeyConst.NAMESPACE);
        configProps.setProperty(PropertyKeyConst.USERNAME, PropertyKeyConst.USERNAME);
        configProps.setProperty(PropertyKeyConst.PASSWORD, PropertyKeyConst.PASSWORD);
        configProps.setProperty(PropertyKeyConst.ACCESS_KEY, PropertyKeyConst.ACCESS_KEY);
        configProps.setProperty(PropertyKeyConst.SECRET_KEY, PropertyKeyConst.SECRET_KEY);

        MockedStatic<ConfigFactory> configFactoryMockedStatic = mockStatic(ConfigFactory.class);
        MockedStatic<NamingFactory> namingFactoryMockedStatic = mockStatic(NamingFactory.class);
        ConfigService configService = mock(ConfigService.class);
        configFactoryMockedStatic.when(() -> ConfigFactory.createConfigService(any(Properties.class))).thenReturn(configService);
        namingFactoryMockedStatic.when(() -> NamingFactory.createNamingService(any(Properties.class))).thenReturn(mock(NamingService.class));
        this.repository = new NacosClientRegisterRepository(config);
        Assertions.assertDoesNotThrow(() -> this.repository.init(config));
        Assertions.assertDoesNotThrow(() -> this.repository.close());
        doThrow(NacosException.class).when(configService).shutDown();
        configFactoryMockedStatic.when(() -> ConfigFactory.createConfigService(any(Properties.class))).thenThrow(NacosException.class);
        // hit error
        Assertions.assertDoesNotThrow(() -> this.repository.close());
        Assertions.assertThrows(ShenyuException.class, () -> this.repository.init(config));
        configFactoryMockedStatic.close();
        namingFactoryMockedStatic.close();
    }
}
