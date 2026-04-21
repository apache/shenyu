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

package org.apache.shenyu.admin.disruptor;

import org.apache.shenyu.admin.service.DiscoveryService;
import org.apache.shenyu.admin.service.register.ShenyuClientRegisterService;
import org.apache.shenyu.disruptor.DisruptorProviderManage;
import org.apache.shenyu.disruptor.provider.DisruptorProvider;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.type.DataTypeParent;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link RegisterClientServerDisruptorPublisher}.
 */
@ExtendWith(MockitoExtension.class)
class RegisterClientServerDisruptorPublisherTest {

    @Mock
    private ShenyuClientRegisterService shenyuClientRegisterService;

    @Mock
    private DiscoveryService discoveryService;

    @Mock
    private DisruptorProviderManage<Collection<DataTypeParent>> mockProviderManage;

    @Mock
    private DisruptorProvider<Collection<DataTypeParent>> mockProvider;

    private RegisterClientServerDisruptorPublisher publisher;

    private Map<String, ShenyuClientRegisterService> serviceMap;

    @BeforeEach
    void setUp() {
        publisher = RegisterClientServerDisruptorPublisher.getInstance();
        serviceMap = new HashMap<>();
        serviceMap.put("http", shenyuClientRegisterService);
    }

    @AfterEach
    void tearDown() throws Exception {
        Field providerManageField = RegisterClientServerDisruptorPublisher.class.getDeclaredField("providerManage");
        providerManageField.setAccessible(true);
        Object providerManage = providerManageField.get(publisher);
        if (Objects.nonNull(providerManage)) {
            try {
                publisher.close();
            } catch (Exception ignored) {
                // Ignore exceptions during cleanup to avoid test failures
            }
        }
        providerManageField.set(publisher, null);
    }

    @Test
    void testGetInstance() {
        RegisterClientServerDisruptorPublisher instance1 = RegisterClientServerDisruptorPublisher.getInstance();
        RegisterClientServerDisruptorPublisher instance2 = RegisterClientServerDisruptorPublisher.getInstance();

        assertNotNull(instance1);
        assertSame(instance1, instance2);
    }

    @Test
    void testStart() {
        publisher.start(serviceMap, discoveryService);

        assertNotNull(getProviderManage());
    }

    @Test
    void testPublishSingleData() throws Exception {
        publisher.start(serviceMap, discoveryService);
        setMockProviderManage();

        URIRegisterDTO uriDTO = URIRegisterDTO.builder()
                .contextPath("/test")
                .rpcType("http")
                .namespaceId("default")
                .build();

        publisher.publish(uriDTO);

        verify(mockProvider, times(1)).onData(any());
    }

    @Test
    void testPublishCollectionData() throws Exception {
        publisher.start(serviceMap, discoveryService);
        setMockProviderManage();

        List<DataTypeParent> dataList = new ArrayList<>();
        dataList.add(URIRegisterDTO.builder()
                .contextPath("/test1")
                .rpcType("http")
                .namespaceId("default")
                .build());
        dataList.add(URIRegisterDTO.builder()
                .contextPath("/test2")
                .rpcType("http")
                .namespaceId("default")
                .build());

        publisher.publish(dataList);

        verify(mockProvider, times(1)).onData(any());
    }

    @Test
    void testPublishMetaData() throws Exception {
        publisher.start(serviceMap, discoveryService);
        setMockProviderManage();

        MetaDataRegisterDTO metaDTO = MetaDataRegisterDTO.builder()
                .appName("testApp")
                .path("/test/path")
                .ruleName("testRule")
                .rpcType("http")
                .namespaceId("default")
                .build();

        publisher.publish(metaDTO);

        verify(mockProvider, times(1)).onData(any());
    }

    @Test
    void testClose() throws Exception {
        publisher.start(serviceMap, discoveryService);
        setMockProviderManage();

        publisher.close();

        verify(mockProvider, times(1)).shutdown();
    }

    @Test
    void testPublishEmptyCollection() throws Exception {
        publisher.start(serviceMap, discoveryService);
        setMockProviderManage();

        List<DataTypeParent> emptyList = new ArrayList<>();
        publisher.publish(emptyList);

        verify(mockProvider, times(1)).onData(any());
    }

    private DisruptorProviderManage<Collection<DataTypeParent>> getProviderManage() {
        try {
            Field field = RegisterClientServerDisruptorPublisher.class.getDeclaredField("providerManage");
            field.setAccessible(true);
            return (DisruptorProviderManage<Collection<DataTypeParent>>) field.get(publisher);
        } catch (Exception e) {
            return null;
        }
    }

    private void setMockProviderManage() throws Exception {
        when(mockProviderManage.getProvider()).thenReturn(mockProvider);

        Field field = RegisterClientServerDisruptorPublisher.class.getDeclaredField("providerManage");
        field.setAccessible(true);
        field.set(publisher, mockProviderManage);
    }
}
