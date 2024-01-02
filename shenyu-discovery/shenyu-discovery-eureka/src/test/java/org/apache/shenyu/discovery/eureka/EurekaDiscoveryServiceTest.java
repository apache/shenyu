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

package org.apache.shenyu.discovery.eureka;

import com.google.gson.JsonObject;
import com.netflix.appinfo.ApplicationInfoManager;
import com.netflix.appinfo.InstanceInfo;
import com.netflix.discovery.EurekaClient;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.discovery.api.listener.DataChangedEventListener;
import org.apache.shenyu.discovery.api.listener.DiscoveryDataChangedEvent;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

import static org.junit.Assert.assertThrows;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * The test for  {@link EurekaDiscoveryService} .
 */
class EurekaDiscoveryServiceTest {

    private EurekaDiscoveryService eurekaDiscoveryServiceUnderTest;

    private EurekaClient eurekaClient;

    private ApplicationInfoManager applicationInfoManager;

    @BeforeEach
    void setUp() throws NoSuchFieldException, IllegalAccessException {
        eurekaDiscoveryServiceUnderTest = new EurekaDiscoveryService();
        eurekaClient = mock(EurekaClient.class);
        applicationInfoManager = mock(ApplicationInfoManager.class);
        setField(eurekaDiscoveryServiceUnderTest.getClass(), "eurekaClient", eurekaClient);
        setField(eurekaDiscoveryServiceUnderTest.getClass(), "applicationInfoManager", applicationInfoManager);
    }

    @AfterEach
    void downTest() {
        when(eurekaClient.getApplicationInfoManager()).thenReturn(applicationInfoManager);
        eurekaDiscoveryServiceUnderTest.shutdown();
        verify(applicationInfoManager).setInstanceStatus(InstanceInfo.InstanceStatus.DOWN);
        verify(eurekaClient).shutdown();
    }

    private <T> void setField(final Class<T> clazz, final String fieldName, final Object value) throws NoSuchFieldException, IllegalAccessException {
        Field field = clazz.getDeclaredField(fieldName);
        field.setAccessible(true);
        field.set(eurekaDiscoveryServiceUnderTest, value);
        field.setAccessible(false);
    }

    @Test
    void testWatch() throws NoSuchFieldException, IllegalAccessException {
        final String key = "testService";
        final DataChangedEventListener mockListener = mock(DataChangedEventListener.class);
        final List<InstanceInfo> initialInstances = new ArrayList<>();
        initialInstances.add(mock(InstanceInfo.class));

        when(eurekaClient.getInstancesByVipAddressAndAppName(null, key, true)).thenReturn(initialInstances);
        final ScheduledExecutorService mockExecutorService = mock(ScheduledExecutorService.class);
        setField(eurekaDiscoveryServiceUnderTest.getClass(), "executorService", mockExecutorService);

        final ScheduledFuture<?> mockFuture = mock(ScheduledFuture.class);
        doReturn(mockFuture).when(mockExecutorService).scheduleAtFixedRate(
                any(Runnable.class),
                anyLong(),
                anyLong(),
                any(TimeUnit.class)
        );

        final ConcurrentMap<String, ScheduledFuture<?>> mockListenerThreadsMap = new ConcurrentHashMap<>();
        setField(eurekaDiscoveryServiceUnderTest.getClass(), "listenerThreadsMap", mockListenerThreadsMap);

        eurekaDiscoveryServiceUnderTest.watch(key, mockListener);

        verify(eurekaClient, times(1)).getInstancesByVipAddressAndAppName(null, key, true);
        verify(mockExecutorService, times(1)).scheduleAtFixedRate(any(Runnable.class), eq(0L), eq(1L), eq(TimeUnit.SECONDS));
        verify(mockListener, times(initialInstances.size())).onChange(any(DiscoveryDataChangedEvent.class));

        assertTrue(mockListenerThreadsMap.containsKey(key));
        assertEquals(mockFuture, mockListenerThreadsMap.get(key));
    }

    @Test
    void testUnwatch() throws NoSuchFieldException, IllegalAccessException {
        final String key = "testService";

        final ScheduledFuture<?> mockFuture = mock(ScheduledFuture.class);
        final ConcurrentMap<String, ScheduledFuture<?>> mockListenerThreadsMap = new ConcurrentHashMap<>();
        mockListenerThreadsMap.put(key, mockFuture);
        setField(eurekaDiscoveryServiceUnderTest.getClass(), "listenerThreadsMap", mockListenerThreadsMap);

        eurekaDiscoveryServiceUnderTest.unwatch(key);

        verify(mockFuture, times(1)).cancel(true);
        assertFalse(mockListenerThreadsMap.containsKey(key));
    }

    @Test
    void testGetRegisterData() {
        final String key = "testService";
        final List<InstanceInfo> instances = new ArrayList<>();
        instances.add(mock(InstanceInfo.class));
        when(eurekaClient.getInstancesByVipAddressAndAppName(null, key, true)).thenReturn(instances);

        final InstanceInfo instanceInfo = instances.get(0);
        when(instanceInfo.getIPAddr()).thenReturn("127.0.0.1");
        when(instanceInfo.getPort()).thenReturn(8080);
        final Map<String, String> metadata = new HashMap<>();
        metadata.put("key1", "value1");
        when(instanceInfo.getMetadata()).thenReturn(metadata);
        when(instanceInfo.getStatus()).thenReturn(InstanceInfo.InstanceStatus.UP);

        final List<String> registerDataList = eurekaDiscoveryServiceUnderTest.getRegisterData(key);

        assertNotNull(registerDataList);
        assertFalse(registerDataList.isEmpty());
        final String expectedJson = buildUpstreamJsonFromInstanceInfo(instanceInfo);
        assertEquals(expectedJson, registerDataList.get(0));
    }

    @Test
    void testExists() {
        final String key = "testService";
        final List<InstanceInfo> instances = new ArrayList<>();
        instances.add(mock(InstanceInfo.class));

        // Mock this service exists
        when(eurekaClient.getInstancesByVipAddressAndAppName(null, key, true)).thenReturn(instances);
        assertTrue(eurekaDiscoveryServiceUnderTest.exists(key));

        // Mock the service does not exist
        when(eurekaClient.getInstancesByVipAddressAndAppName(null, key, true)).thenReturn(Collections.emptyList());
        assertFalse(eurekaDiscoveryServiceUnderTest.exists(key));
        
        // Mock the throwing of Exception
        when(eurekaClient.getInstancesByVipAddressAndAppName(null, key, true)).thenThrow(new ShenyuException("test"));
        assertThrows(ShenyuException.class, () -> eurekaDiscoveryServiceUnderTest.exists(key));
    }

    /**
     * Same as in EurekaDiscoveryService.
     *
     * @param instanceInfo Convert eureka instance to Json string
     */
    private String buildUpstreamJsonFromInstanceInfo(final InstanceInfo instanceInfo) {
        JsonObject upstreamJson = new JsonObject();
        upstreamJson.addProperty("url", instanceInfo.getIPAddr() + ":" + instanceInfo.getPort());
        upstreamJson.addProperty("weight", instanceInfo.getMetadata().get("weight"));
        upstreamJson.addProperty("protocol", instanceInfo.getMetadata().get("protocol"));
        upstreamJson.addProperty("props", instanceInfo.getMetadata().get("props"));
        if (instanceInfo.getStatus() == InstanceInfo.InstanceStatus.UP) {
            upstreamJson.addProperty("status", 0);
        } else if (instanceInfo.getStatus() == InstanceInfo.InstanceStatus.DOWN) {
            upstreamJson.addProperty("status", 1);
        }
        return GsonUtils.getInstance().toJson(upstreamJson);
    }

}
