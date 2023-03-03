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
import com.tencent.polaris.api.plugin.configuration.ConfigFile;
import com.tencent.polaris.api.pojo.DefaultInstance;
import com.tencent.polaris.api.pojo.Instance;
import com.tencent.polaris.api.rpc.InstanceRegisterRequest;
import com.tencent.polaris.client.api.SDKContext;
import com.tencent.polaris.factory.api.DiscoveryAPIFactory;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.client.polaris.common.PolarisConfigClient;
import org.apache.shenyu.register.client.polaris.constant.OpenAPIStatusCode;
import org.apache.shenyu.register.client.polaris.model.ConfigFileRelease;
import org.apache.shenyu.register.client.polaris.model.ConfigFileTemp;
import org.apache.shenyu.register.client.polaris.model.ConfigFilesResponse;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.path.RegisterPathConstants;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import java.io.IOException;
import java.lang.reflect.Field;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.stream.Collectors;

import static org.apache.shenyu.common.constant.Constants.COLONS;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;

/**
 * Test for polaris register center client.
 */
public class PolarisClientRegisterRepositoryTest {

    private PolarisClientRegisterRepository repository;

    /**
     * key:    namespace:group:fileName.
     * value:  ConfigFiles.
     */
    private final Map<String, List<ConfigFile>> configStorage = Maps.newHashMap();

    private final Map<String, List<Instance>> storage = Maps.newHashMap();

    private ProviderAPI providerAPI;

    @BeforeEach
    public void setUp() throws IllegalAccessException, NoSuchFieldException, PolarisException, IOException {
        this.repository = new PolarisClientRegisterRepository();
        Class<? extends PolarisClientRegisterRepository> clazz = this.repository.getClass();
        this.providerAPI = mockProviderAPI();

        setField(clazz, "providerAPI", providerAPI);
        setField(clazz, "polarisConfigClient", mockConfigClient());
        setField(clazz, "props", new Properties());

        configStorage.clear();
    }

    private PolarisConfigClient mockConfigClient() throws PolarisException, IOException {
        PolarisConfigClient configClient = mock(PolarisConfigClient.class);

        doAnswer(invocationOnMock -> {
            ConfigFileRelease release = invocationOnMock.getArgument(0);
            final List<ConfigFile> files = configStorage.getOrDefault(String.join(COLONS, release.getNamespace(), release.getFileName()), Lists.newArrayList());

            return ConfigFilesResponse.builder()
                    .code(CollectionUtils.isEmpty(files) ? OpenAPIStatusCode.NOT_FOUND_RESOURCE : OpenAPIStatusCode.OK)
                    .files(files)
                    .build();
        }).when(configClient).getConfigFile(any());

        doAnswer(invocationOnMock -> {
            ConfigFileTemp temp = invocationOnMock.getArgument(0);
            final List<MetaDataRegisterDTO> list = GsonUtils.getInstance().fromList(temp.getContent(), MetaDataRegisterDTO.class);
            final String key = "default:shenyu:" + RegisterPathConstants.buildMetaDataParentPath(list.get(0).getRpcType(), list.get(0).getContextPath());
            final List<ConfigFile> files = configStorage.getOrDefault(key, Lists.newArrayList());
            files.addAll(list.stream().map(meta -> { 
                final ConfigFile configFile = new ConfigFile(temp.getNamespace(), temp.getGroup(), temp.getFileName());
                configFile.setContent(GsonUtils.getInstance().toJson(meta));
                return configFile;
            })
                    .collect(Collectors.toList()));
            configStorage.put(key, files);

            return ConfigFilesResponse.builder()
                    .code(OpenAPIStatusCode.OK)
                    .files(files)
                    .build();
        }).when(configClient).createConfigFile(any());

        doAnswer(invocationOnMock -> {
            ConfigFileRelease release = invocationOnMock.getArgument(0);
            final List<ConfigFile> files = configStorage.getOrDefault(String.join(COLONS, release.getNamespace(), release.getFileName()), Lists.newArrayList());

            return ConfigFilesResponse.builder()
                    .code(OpenAPIStatusCode.OK)
                    .files(files)
                    .build();
        }).when(configClient).releaseConfigFile(any());

        return configClient;
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
            configStorage.clear();
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
        String configPath = "default:shenyu:" + RegisterPathConstants.buildMetaDataParentPath(metadata.getRpcType(), metadata.getContextPath());
        assertTrue(configStorage.containsKey(configPath));

        String dataStr = GsonUtils.getInstance().toJson(metadata);
        assertTrue(configStorage.get(configPath).stream().map(ConfigFile::getContent).collect(Collectors.toList()).contains(dataStr));
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
        SDKContext sdkContext = mock(SDKContext.class);
        ProviderAPI providerMock = mock(ProviderAPI.class);
        sdkContextMockedStatic.when(() -> SDKContext.initContextByConfig(any(Configuration.class))).thenReturn(sdkContext);
        apiFactoryMockedStatic.when(() -> DiscoveryAPIFactory.createProviderAPIByContext(any(SDKContext.class))).thenReturn(providerMock);
        this.repository = new PolarisClientRegisterRepository();
        Assertions.assertDoesNotThrow(() -> this.repository.init(config));
        Assertions.assertDoesNotThrow(() -> this.repository.close());
        apiFactoryMockedStatic.close();
        sdkContextMockedStatic.close();
    }

    @AfterEach
    public void close() {
        repository.close();
        configStorage.clear();
    }
}
