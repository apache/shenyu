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

package org.apache.shenyu.registry.nacos;

import com.alibaba.nacos.api.exception.NacosException;
import com.alibaba.nacos.api.naming.NamingService;
import com.alibaba.nacos.api.naming.listener.EventListener;
import com.alibaba.nacos.api.naming.listener.NamingEvent;
import com.alibaba.nacos.api.naming.pojo.Instance;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.registry.api.entity.InstanceEntity;
import org.apache.shenyu.registry.api.event.ChangedEventListener;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.ArgumentCaptor;

import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.clearInvocations;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

public final class NacosInstanceRegisterRepositoryTest {

    private NacosInstanceRegisterRepository repository;

    private NamingService namingService;

    private final Map<String, Instance> storage = new HashMap<>();

    @BeforeEach
    public void setUp() throws NoSuchFieldException, IllegalAccessException, NacosException {
        this.repository = new NacosInstanceRegisterRepository();
        Class<? extends NacosInstanceRegisterRepository> clazz = this.repository.getClass();

        Field field = clazz.getDeclaredField("namingService");
        field.setAccessible(true);
        namingService = mockNamingService();
        field.set(repository, namingService);

        field = clazz.getDeclaredField("groupName");
        field.setAccessible(true);
        field.set(repository, "group");

        storage.clear();
    }

    private NamingService mockNamingService() throws NacosException {
        NamingService namingService = mock(NamingService.class);

        doAnswer(invocationOnMock -> {
            String serviceName = invocationOnMock.getArgument(0);
            String groupName = invocationOnMock.getArgument(1);
            Instance value = invocationOnMock.getArgument(2);
            storage.put(serviceName + "-" + groupName, value);
            return null;
        }).when(namingService).registerInstance(anyString(), anyString(), any());

        doAnswer(invocationOnMock -> {
            storage.clear();
            return null;
        }).when(namingService).shutDown();
        return namingService;
    }

    @Test
    public void testPersistInstance() {
        InstanceEntity data = InstanceEntity.builder()
                .appName("shenyu-test")
                .host("shenyu-host")
                .port(9195)
                .build();

        final String key = "shenyu-test-group";
        repository.persistInstance(data);
        assertTrue(storage.containsKey(key));

        final Instance instance = storage.get(key);
        assertEquals(data.getHost(), instance.getIp());
        assertEquals(data.getPort(), instance.getPort());
        assertEquals(data.getAppName(), instance.getServiceName());
        repository.close();
    }

    @Test
    public void testSelectInstancesAndWatcher() {
        String selectKey = "shenyu-instances";
        repository.selectInstances(selectKey);
        repository.close();
    }

    @ParameterizedTest
    @NullAndEmptySource
    @ValueSource(strings = {"instance-1"})
    public void testWeightChangeOnlyPublishesUpdate(final String instanceId) throws NacosException {
        Instance previous = newInstance(instanceId, "127.0.0.1");
        Instance current = newInstance(instanceId, "127.0.0.1");
        current.setWeight(100);
        ChangedEventListener listener = notifyChange(Collections.singletonList(previous), Collections.singletonList(current));
        ArgumentCaptor<String> payload = ArgumentCaptor.forClass(String.class);
        verify(listener).onEvent(eq("service"), payload.capture(), eq(ChangedEventListener.Event.UPDATED));
        assertEquals(100.0, GsonUtils.getInstance().fromJson(payload.getValue(), Map.class).get("weight"));
        verifyNoMoreInteractions(listener);
        assertTrue(previous.isHealthy());
    }

    @Test
    public void testMetadataChangeOnlyPublishesUpdate() throws NacosException {
        Instance previous = newInstance("instance-1", "127.0.0.1");
        Instance current = newInstance("instance-1", "127.0.0.1");
        current.setMetadata(Collections.singletonMap("props", "version-v2"));
        ChangedEventListener listener = notifyChange(Collections.singletonList(previous), Collections.singletonList(current));
        ArgumentCaptor<String> payload = ArgumentCaptor.forClass(String.class);
        verify(listener).onEvent(eq("service"), payload.capture(), eq(ChangedEventListener.Event.UPDATED));
        assertEquals("version-v2", GsonUtils.getInstance().fromJson(payload.getValue(), Map.class).get("props"));
        verifyNoMoreInteractions(listener);
    }

    @Test
    public void testUnchangedInstanceDoesNotPublishEvent() throws NacosException {
        ChangedEventListener listener = notifyChange(Collections.singletonList(newInstance("instance-1", "127.0.0.1")),
                Collections.singletonList(newInstance("instance-1", "127.0.0.1")));
        verifyNoMoreInteractions(listener);
    }

    @Test
    public void testAddedAndDeletedInstances() throws NacosException {
        Instance unchanged = newInstance("instance-1", "127.0.0.1");
        ChangedEventListener listener = notifyChange(Arrays.asList(unchanged, newInstance("instance-2", "127.0.0.2")),
                Arrays.asList(unchanged, newInstance("instance-3", "127.0.0.3")));
        verify(listener).onEvent(eq("service"), anyString(), eq(ChangedEventListener.Event.ADDED));
        verify(listener).onEvent(eq("service"), anyString(), eq(ChangedEventListener.Event.DELETED));
        verifyNoMoreInteractions(listener);
    }

    @ParameterizedTest
    @ValueSource(strings = {"ip", "port", "cluster"})
    public void testDistinctInstancesWithoutIds(final String changedField) throws NacosException {
        Instance previous = newInstance(null, "127.0.0.1");
        Instance current = newInstance(null, "127.0.0.1");
        if ("ip".equals(changedField)) {
            current.setIp("127.0.0.2");
        } else if ("port".equals(changedField)) {
            current.setPort(8081);
        } else {
            current.setClusterName("another-cluster");
        }
        ChangedEventListener listener = notifyChange(Collections.singletonList(previous), Collections.singletonList(current));
        verify(listener).onEvent(eq("service"), anyString(), eq(ChangedEventListener.Event.ADDED));
        verify(listener).onEvent(eq("service"), anyString(), eq(ChangedEventListener.Event.DELETED));
        verifyNoMoreInteractions(listener);
    }

    private ChangedEventListener notifyChange(final List<Instance> previous, final List<Instance> current) throws NacosException {
        when(namingService.selectInstances("service", "group", true)).thenReturn(previous, current);
        ChangedEventListener listener = mock(ChangedEventListener.class);
        repository.watchInstances("service", listener);
        ArgumentCaptor<EventListener> callback = ArgumentCaptor.forClass(EventListener.class);
        verify(namingService).subscribe(eq("service"), eq("group"), callback.capture());
        clearInvocations(listener);
        callback.getValue().onEvent(new NamingEvent("service", current));
        return listener;
    }

    private Instance newInstance(final String instanceId, final String ip) {
        Instance instance = new Instance();
        instance.setInstanceId(instanceId);
        instance.setIp(ip);
        instance.setPort(8080);
        instance.setServiceName("service");
        instance.setWeight(50);
        return instance;
    }
}
