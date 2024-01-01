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

package org.apache.shenyu.discovery.nacos;

import com.alibaba.nacos.api.exception.NacosException;
import com.alibaba.nacos.api.naming.NamingFactory;
import com.alibaba.nacos.api.naming.NamingService;
import com.alibaba.nacos.api.naming.listener.EventListener;
import com.alibaba.nacos.api.naming.listener.NamingEvent;
import com.alibaba.nacos.api.naming.pojo.Instance;
import com.google.gson.JsonObject;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.discovery.api.config.DiscoveryConfig;
import org.apache.shenyu.discovery.api.listener.DataChangedEventListener;
import org.apache.shenyu.discovery.api.listener.DiscoveryDataChangedEvent;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Properties;
import java.util.concurrent.ConcurrentMap;

import static org.junit.Assert.assertThrows;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * The test for  {@link NacosDiscoveryService} .
 */
class NacosDiscoveryServiceTest {

    private NacosDiscoveryService nacosDiscoveryServiceUnderTest;

    private NamingService namingService;

    @BeforeEach
    void setUp() throws NoSuchFieldException, IllegalAccessException {
        nacosDiscoveryServiceUnderTest = new NacosDiscoveryService();
        namingService = mock(NamingService.class);
        setField(nacosDiscoveryServiceUnderTest.getClass(), "namingService", namingService);
        setField(nacosDiscoveryServiceUnderTest.getClass(), "groupName", "SHENYU_GROUP");
    }

    @AfterEach
    void downTest() throws NacosException {
        nacosDiscoveryServiceUnderTest.shutdown();
        verify(namingService).shutDown();
    }

    private <T> void setField(final Class<T> clazz, final String fieldName, final Object value) throws NoSuchFieldException, IllegalAccessException {
        Field field = clazz.getDeclaredField(fieldName);
        field.setAccessible(true);
        field.set(nacosDiscoveryServiceUnderTest, value);
        field.setAccessible(false);
    }

    private <T> Object getField(final T object, final String fieldName) throws NoSuchFieldException, IllegalAccessException {
        Class<?> clazz = object.getClass();
        Field field = clazz.getDeclaredField(fieldName);
        field.setAccessible(true);
        Object value = field.get(object);
        field.setAccessible(false);
        return value;
    }

    @Test
    void testInit() throws NoSuchFieldException, IllegalAccessException {
        // Set the discovery config
        setField(nacosDiscoveryServiceUnderTest.getClass(), "namingService", null);
        DiscoveryConfig config = new DiscoveryConfig();
        Properties properties = new Properties();
        config.setServerList("127.0.0.1:8848");
        properties.setProperty("groupName", "SHENYU_GROUP");
        config.setProps(properties);

        try (MockedStatic<NamingFactory> mockedNamingFactory = mockStatic(NamingFactory.class)) {
            // Mock the successful creation of NamingService
            mockedNamingFactory.when(() -> NamingFactory.createNamingService(any(Properties.class)))
                    .thenReturn(namingService);
            nacosDiscoveryServiceUnderTest.init(config);
            mockedNamingFactory.verify(() -> NamingFactory.createNamingService(any(Properties.class)));
            assertEquals(namingService, getField(nacosDiscoveryServiceUnderTest, "namingService"));
            // Mock the situation where NamingService fails to be created and throws an exception
            mockedNamingFactory.when(() -> NamingFactory.createNamingService(any(Properties.class)))
                    .thenThrow(new NacosException());
            assertDoesNotThrow(() -> nacosDiscoveryServiceUnderTest.init(config));
        }
    }

    @Test
    void testWatch() throws NacosException, NoSuchFieldException, IllegalAccessException {
        final DataChangedEventListener mockListener = mock(DataChangedEventListener.class);
        // Construct instance information
        final String key = "test";
        final String key2 = "test2";
        Instance instance1 = new Instance();
        instance1.setIp("192.168.1.1");
        instance1.setPort(8080);
        instance1.setServiceName(key);
        instance1.setInstanceId(key);

        Instance instance2 = new Instance();
        instance2.setIp("192.168.1.2");
        instance2.setPort(8081);
        instance2.setServiceName(key2);
        instance2.setInstanceId(key2);

        List<Instance> initialInstances = new ArrayList<>();
        initialInstances.add(instance1);
        List<Instance> updatedInstances = Arrays.asList(instance1, instance2);

        // Mock the behavior of namingService.subscribe
        when(namingService.selectInstances(key, "SHENYU_GROUP", true))
                .thenReturn(initialInstances)
                .thenReturn(updatedInstances);
        doAnswer(invocation -> {
            EventListener nacosListener = invocation.getArgument(2);
            NamingEvent event = new NamingEvent(key, updatedInstances);
            nacosListener.onEvent(event);
            return null;
        }).when(namingService).subscribe(anyString(), anyString(), any(EventListener.class));
        nacosDiscoveryServiceUnderTest.watch(key, mockListener);
        // Verify that methods are called correctly
        verify(mockListener, atLeastOnce()).onChange(any(DiscoveryDataChangedEvent.class));
        verify(namingService).subscribe(anyString(), anyString(), any(EventListener.class));
        verify(namingService, times(2)).selectInstances(key, "SHENYU_GROUP", true);
        // Verify the field content of listenerMap：it should be non-empty
        ConcurrentMap<String, EventListener> listenerMap = (ConcurrentMap<String, EventListener>) getField(nacosDiscoveryServiceUnderTest, "listenerMap");
        assertNotNull(listenerMap.get(key));
    }

    @Test
    void testUnwatch() throws NoSuchFieldException, IllegalAccessException {
        final String key = "test";
        nacosDiscoveryServiceUnderTest.unwatch(key);
        ConcurrentMap<String, EventListener> listenerMap = (ConcurrentMap<String, EventListener>) getField(nacosDiscoveryServiceUnderTest, "listenerMap");
        assertFalse(listenerMap.containsKey(key));
    }

    @Test
    void testRegister() throws NacosException {
        final String key = "test";
        final String value = "{\"weight\":20,\"url\":\"127.0.0.1:8080\"}";

        doNothing().when(namingService).registerInstance(anyString(), anyString(), any(Instance.class));
        nacosDiscoveryServiceUnderTest.register(key, value);
        // Verify whether the method is called correctly
        verify(namingService).registerInstance(anyString(), anyString(), any(Instance.class));
        // Mock the wrong json format
        assertThrows(ShenyuException.class, () -> nacosDiscoveryServiceUnderTest.register(key, "test"));
        // Mock the throwing of registerInstance exception
        doThrow(new NacosException()).when(namingService).registerInstance(anyString(), anyString(), any(Instance.class));
        assertThrows(ShenyuException.class, () -> nacosDiscoveryServiceUnderTest.register(key, value));
    }

    @Test
    void testGetRegisterData() throws NacosException {
        final String key = "test";
        Instance instance1 = new Instance();
        instance1.setIp("192.168.1.1");
        instance1.setPort(8080);

        Instance instance2 = new Instance();
        instance2.setIp("192.168.1.2");
        instance2.setPort(8081);

        final List<Instance> mockInstances = Arrays.asList(instance1, instance2);
        when(namingService.selectInstances(key, "SHENYU_GROUP", true)).thenReturn(mockInstances);
        final List<String> result = nacosDiscoveryServiceUnderTest.getRegisterData(key);
        verify(namingService).selectInstances(key, "SHENYU_GROUP", true);

        // Verify that the data format is consistent
        assertEquals(mockInstances.size(), result.size());
        for (int i = 0; i < mockInstances.size(); i++) {
            Instance instance = mockInstances.get(i);
            String expectedJson = buildInstanceInfoJson(instance);
            assertEquals(expectedJson, result.get(i));
        }
    }

    @Test
    void testExists() throws NacosException {
        List<Instance> mockInstances = new ArrayList<>();
        mockInstances.add(mock(Instance.class));
        // Mock this service exists
        when(namingService.selectInstances(anyString(), anyString(), anyBoolean())).thenReturn(mockInstances);
        assertTrue(nacosDiscoveryServiceUnderTest.exists("key"));
        // Mock the service does not exist
        when(namingService.selectInstances(anyString(), anyString(), anyBoolean())).thenReturn(Collections.emptyList());
        assertFalse(nacosDiscoveryServiceUnderTest.exists("key"));
        // Mock the throwing of NacosException
        when(namingService.selectInstances(anyString(), anyString(), anyBoolean())).thenThrow(new NacosException());
        assertThrows(ShenyuException.class, () -> nacosDiscoveryServiceUnderTest.exists("key"));
    }

    /**
     * Same as in NacosDiscoveryService.
     *
     * @param instance Convert Nacos instance to Json string
     */
    private String buildInstanceInfoJson(final Instance instance) {
        JsonObject instanceJson = new JsonObject();
        instanceJson.addProperty("url", instance.getIp() + ":" + instance.getPort());
        // status 0:true, 1:false
        instanceJson.addProperty("status", instance.isHealthy() ? 0 : 1);
        instanceJson.addProperty("weight", instance.getWeight());

        return GsonUtils.getInstance().toJson(instanceJson);
    }

}

