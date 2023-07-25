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

package org.apache.shenyu.register.client.polaris;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.tencent.polaris.api.config.Configuration;
import com.tencent.polaris.api.core.ProviderAPI;
import com.tencent.polaris.api.exception.PolarisException;
import com.tencent.polaris.api.pojo.DefaultInstance;
import com.tencent.polaris.api.pojo.Instance;
import com.tencent.polaris.api.rpc.InstanceRegisterRequest;
import com.tencent.polaris.client.api.SDKContext;
import com.tencent.polaris.configuration.api.core.ConfigFileMetadata;
import com.tencent.polaris.configuration.api.core.ConfigFilePublishService;
import com.tencent.polaris.configuration.api.core.ConfigFileService;
import com.tencent.polaris.configuration.factory.ConfigFileServiceFactory;
import com.tencent.polaris.configuration.factory.ConfigFileServicePublishFactory;
import com.tencent.polaris.factory.api.DiscoveryAPIFactory;
import java.io.IOException;
import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import static org.apache.shenyu.common.constant.PolarisPathConstants.FILE_GROUP;
import static org.apache.shenyu.common.constant.PolarisPathConstants.META_DATA_FILE_NAME;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.path.RegisterPathConstants;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import static org.mockito.ArgumentMatchers.any;
import org.mockito.MockedStatic;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;

/**
 * Test for polaris register center client.
 */
public class PolarisClientRegisterRepositoryTest {

    private PolarisClientRegisterRepository repository;

    private final Map<String, List<Instance>> storage = Maps.newHashMap();

    private ConfigFileService configFileService;

    private ConfigFileMetadata metaConfig;

    private ConfigFilePublishService configFilePublishService;

    private ProviderAPI providerAPI;

    @BeforeEach
    public void setUp() throws IllegalAccessException, NoSuchFieldException, PolarisException, IOException {
        this.repository = new PolarisClientRegisterRepository();
        final Class<? extends PolarisClientRegisterRepository> clazz = this.repository.getClass();
        this.providerAPI = mockProviderAPI();
        final PolarisMockConfigService polarisMockConfigService = new PolarisMockConfigService(new HashMap<>());
        this.configFileService = polarisMockConfigService;
        this.configFilePublishService = polarisMockConfigService;
        this.metaConfig = new PolarisMockConfigFile("default", FILE_GROUP, META_DATA_FILE_NAME);

        setField(clazz, "providerAPI", this.providerAPI);
        setField(clazz, "configFileService", this.configFileService);
        setField(clazz, "configFilePublishService", this.configFilePublishService);
        setField(clazz, "props", new Properties());
    }

    private <T> void setField(final Class<T> clazz, final String fieldName, final Object value) throws NoSuchFieldException, IllegalAccessException {
        Field field = clazz.getDeclaredField(fieldName);
        field.setAccessible(true);
        field.set(repository, value);
        field.setAccessible(false);
    }

    private ProviderAPI mockProviderAPI() throws PolarisException {
        ProviderAPI providerAPI = mock(ProviderAPI.class);

        doAnswer(invocationOnMock -> {
            InstanceRegisterRequest request = invocationOnMock.getArgument(0);
            String service = request.getService();
            String namespace = request.getNamespace();
            String host = request.getHost();
            Integer post = request.getPort();
            final String rpcType = request.getMetadata().getOrDefault("rpcType", "");
            final String uriInstancePath = RegisterPathConstants.buildServiceInstancePath(rpcType);
            DefaultInstance instance = (DefaultInstance) Instance.createDefaultInstance("1", namespace, service, host, post);
            instance.setMetadata(request.getMetadata());
            List<Instance> list = storage.getOrDefault(uriInstancePath, Lists.newArrayList());
            list.add(instance);
            storage.put(uriInstancePath, list);
            return null;
        }).when(providerAPI).registerInstance(any());

        doAnswer(invocationOnMock -> {
            storage.clear();
            return null;
        }).when(providerAPI).close();
        return providerAPI;
    }

    @Test
    public void testPersistInterface() throws PolarisException {
        final MetaDataRegisterDTO metadata = MetaDataRegisterDTO.builder()
                .rpcType("http")
                .host("host")
                .port(80)
                .contextPath("/context")
                .ruleName("ruleName")
                .build();
        repository.persistInterface(metadata);
        String configPath = RegisterPathConstants.buildMetaDataParentPath(metadata.getRpcType(), metadata.getContextPath());
        PolarisMockConfigFile configFileMetadata = new PolarisMockConfigFile(metaConfig);
        configFileMetadata.setFileName(configPath);
        assertTrue(configFileService.getConfigFile(configFileMetadata).hasContent());

        String dataStr = GsonUtils.getInstance().toJson(Lists.newArrayList(metadata));
        assertTrue(configFileService.getConfigFile(configFileMetadata).getContent().equals(dataStr));
    }

    @Test
    public void testPersistUri() throws PolarisException {
        final URIRegisterDTO data = URIRegisterDTO.builder()
                .rpcType("http")
                .host("host")
                .port(80)
                .contextPath("/context")
                .build();
        final String uriInstancePath = RegisterPathConstants.buildServiceInstancePath(data.getRpcType());
        repository.persistURI(data);
        assertTrue(storage.containsKey(uriInstancePath));
        Instance instance = storage.get(uriInstancePath).get(0);
        assertEquals(instance.getHost(), data.getHost());
        Map<String, String> metadataMap = instance.getMetadata();
        assertEquals(metadataMap.get("rpcType"), data.getRpcType());

        doThrow(ShenyuException.class).when(providerAPI).registerInstance(any());
        Assertions.assertThrows(ShenyuException.class, () -> repository.persistURI(data));
    }

    @Test
    public void initTest() throws PolarisException {
        ShenyuRegisterCenterConfig config = new ShenyuRegisterCenterConfig();
        config.setServerLists("http://localhost:8090");

        MockedStatic<DiscoveryAPIFactory> apiFactoryMockedStatic = mockStatic(DiscoveryAPIFactory.class);
        MockedStatic<SDKContext> sdkContextMockedStatic = mockStatic(SDKContext.class);
        MockedStatic<ConfigFileServiceFactory> configFileServiceFactoryMockedStatic = mockStatic(ConfigFileServiceFactory.class);
        MockedStatic<ConfigFileServicePublishFactory> configFileServicePublishFactoryMockedStatic = mockStatic(ConfigFileServicePublishFactory.class);
        SDKContext sdkContext = mock(SDKContext.class);
        ProviderAPI providerMock = mock(ProviderAPI.class);
        sdkContextMockedStatic.when(() -> SDKContext.initContextByConfig(any(Configuration.class))).thenReturn(sdkContext);
        apiFactoryMockedStatic.when(() -> DiscoveryAPIFactory.createProviderAPIByContext(any(SDKContext.class))).thenReturn(providerMock);
        configFileServiceFactoryMockedStatic.when(() -> ConfigFileServiceFactory.createConfigFileService(any(SDKContext.class))).thenReturn(this.configFileService);
        configFileServicePublishFactoryMockedStatic.when(() -> ConfigFileServicePublishFactory.createConfigFilePublishService(any(SDKContext.class))).thenReturn(this.configFilePublishService);
        this.repository = new PolarisClientRegisterRepository();
        Assertions.assertDoesNotThrow(() -> this.repository.init(config));
        Assertions.assertDoesNotThrow(() -> this.repository.closeRepository());
        apiFactoryMockedStatic.close();
        sdkContextMockedStatic.close();
        configFileServiceFactoryMockedStatic.close();
        configFileServicePublishFactoryMockedStatic.close();
    }

    @AfterEach
    public void close() {
        repository.closeRepository();
    }
}
