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

package org.apache.shenyu.register.client.server.polaris;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.tencent.polaris.api.config.Configuration;
import com.tencent.polaris.api.core.ConsumerAPI;
import com.tencent.polaris.api.exception.PolarisException;
import com.tencent.polaris.api.listener.ServiceListener;
import com.tencent.polaris.api.pojo.Instance;
import com.tencent.polaris.api.rpc.InstancesResponse;
import com.tencent.polaris.api.rpc.WatchServiceRequest;
import com.tencent.polaris.api.rpc.WatchServiceResponse;
import com.tencent.polaris.client.api.SDKContext;
import com.tencent.polaris.configuration.api.core.ConfigFileService;
import com.tencent.polaris.configuration.client.internal.DefaultConfigFile;
import com.tencent.polaris.configuration.factory.ConfigFileServiceFactory;
import com.tencent.polaris.factory.ConfigAPIFactory;
import com.tencent.polaris.factory.api.DiscoveryAPIFactory;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.client.server.api.ShenyuClientServerRegisterPublisher;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.type.DataTypeParent;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test for polaris register server client.
 */
public class PolarisClientServerRegisterRepositoryTest {

    private final Map<String, List<Instance>> storage = Maps.newHashMap();

    private final Map<String, List<ServiceListener>> listenerStorage = Maps.newHashMap();

    private PolarisClientServerRegisterRepository repository;

    private ConsumerAPI consumerAPI;
    
    private ConfigFileService configFileService;

    private ShenyuClientServerRegisterPublisher publisher;

    @BeforeEach
    public void setUp() throws NoSuchFieldException, IllegalAccessException, PolarisException {
        this.publisher = mockPublish();
        this.repository = new PolarisClientServerRegisterRepository();
        Class<? extends PolarisClientServerRegisterRepository> clazz = this.repository.getClass();

        this.consumerAPI = mockConsumerAPI();
        this.configFileService = mockConfigService();
        final Properties props = new Properties();

        setField(clazz, "configFileService", configFileService);
        setField(clazz, "consumerAPI", consumerAPI);
        setField(clazz, "publisher", publisher);
        setField(clazz, "props", props);

        storage.clear();
    }

    private <T> void setField(final Class<T> clazz, final String fieldName, final Object value) throws NoSuchFieldException, IllegalAccessException {
        Field field = clazz.getDeclaredField(fieldName);
        field.setAccessible(true);
        field.set(repository, value);
        field.setAccessible(false);
    }

    private ConfigFileService mockConfigService() throws PolarisException {
        ConfigFileService configService = mock(ConfigFileService.class);

        doAnswer(invocationOnMock -> {
            DefaultConfigFile configFile = mock(DefaultConfigFile.class);
            MetaDataRegisterDTO metadata = MetaDataRegisterDTO.builder().build();
            String meta = GsonUtils.getInstance().toJson(URIRegisterDTO.transForm(metadata));
            when(configFile.getContent()).thenReturn(meta);
            return configFile;
        }).when(configService).getConfigFile(anyString(), anyString(), anyString());

        return configService;
    }

    private ConsumerAPI mockConsumerAPI() throws PolarisException {
        ConsumerAPI consumerAPI = mock(ConsumerAPI.class);

        doAnswer(invocationOnMock -> {
            WatchServiceRequest request = invocationOnMock.getArgument(0);
            String service = request.getService();
            String namespace = request.getNamespace();
            List<ServiceListener> listeners = request.getListeners();
            listenerStorage.put(service + namespace, listeners);

            WatchServiceResponse response = mock(WatchServiceResponse.class);
            InstancesResponse resp = mock(InstancesResponse.class);
            when(response.getResponse()).thenReturn(resp);
            List<Instance> instances = storage.getOrDefault(service + namespace, Lists.newArrayList());
            when(resp.getInstances()).thenReturn(instances.toArray(instances.toArray(new Instance[0])));
            return response;
        }).when(consumerAPI).watchService(any());

        doAnswer(invocationOnMock -> {
            listenerStorage.clear();
            return null;
        }).when(consumerAPI).close();
        return consumerAPI;
    }

    private ShenyuClientServerRegisterPublisher mockPublish() {
        ShenyuClientServerRegisterPublisher publisher = mock(ShenyuClientServerRegisterPublisher.class);
        doNothing().when(publisher).publish(localAny());
        return publisher;
    }

    @Test
    public void testSubscribeTypeOfNotSupportURI() throws NoSuchMethodException, InvocationTargetException,
            IllegalAccessException {
        Class<? extends PolarisClientServerRegisterRepository> clazz = this.repository.getClass();
        String methodString = "subscribe";
        Method method = clazz.getDeclaredMethod(methodString, RpcTypeEnum.class);
        method.setAccessible(true);
        method.invoke(repository, RpcTypeEnum.BRPC);

        verify(publisher, times(0)).publish(localAny());
        assertEquals(listenerStorage.values().size(), 1);
    }

    @Test
    public void initTest() {
        ShenyuRegisterCenterConfig config = new ShenyuRegisterCenterConfig();
        config.setServerLists("http://localhost:8089");

        try (
                MockedStatic<ConfigAPIFactory> configAPIFactoryMockedStatic = mockStatic(ConfigAPIFactory.class);
                MockedStatic<SDKContext> sdkContextMockedStatic = mockStatic(SDKContext.class);
                MockedStatic<DiscoveryAPIFactory> discoveryAPIFactoryMockedStatic = mockStatic(DiscoveryAPIFactory.class);
                MockedStatic<ConfigFileServiceFactory> configFileServiceFactoryMockedStatic = mockStatic(ConfigFileServiceFactory.class);
        ) {
            Configuration configService = mock(Configuration.class);
            SDKContext sdkContext = mock(SDKContext.class);
            
            configAPIFactoryMockedStatic.when(() -> ConfigAPIFactory.createConfigurationByAddress(anyString())).thenReturn(configService);
            sdkContextMockedStatic.when(() -> SDKContext.initContextByConfig(any(Configuration.class))).thenReturn(sdkContext);
            discoveryAPIFactoryMockedStatic.when(() -> DiscoveryAPIFactory.createConsumerAPIByContext(any(SDKContext.class))).thenReturn(consumerAPI);
            configFileServiceFactoryMockedStatic.when(() -> ConfigFileServiceFactory.createConfigFileService(any(SDKContext.class))).thenReturn(configFileService);
            
            ShenyuClientServerRegisterPublisher clientServerRegisterPublisher = mock(ShenyuClientServerRegisterPublisher.class);
            Assertions.assertDoesNotThrow(() -> this.repository.init(clientServerRegisterPublisher, config));
            Assertions.assertDoesNotThrow(() -> this.repository.close());
            doThrow(PolarisException.class).when(consumerAPI).close();
            // hit error
            Assertions.assertDoesNotThrow(() -> this.repository.close());
            Assertions.assertThrows(ShenyuException.class, () -> this.repository.init(clientServerRegisterPublisher, config));

        } catch (Exception e) {
            // ignore
        }
    }

    private List<DataTypeParent> localAny() {
        return any();
    }
}
