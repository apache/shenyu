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

package org.apache.shenyu.registry.eureka;

import com.netflix.appinfo.ApplicationInfoManager;
import com.netflix.appinfo.InstanceInfo;
import com.netflix.appinfo.providers.VipAddressResolver;
import com.netflix.discovery.DiscoveryClient;
import com.netflix.discovery.EurekaClient;
import com.netflix.discovery.EurekaEventListener;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.registry.api.config.RegisterConfig;
import org.apache.shenyu.registry.api.entity.InstanceEntity;
import org.apache.shenyu.registry.api.event.ChangedEventListener;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedConstruction;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.nullable;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

public final class EurekaInstanceRegisterRepositoryTest {

    private EurekaInstanceRegisterRepository repository;

    private final InstanceEntity instance = new InstanceEntity("shenyu-instances", "shenyu-host", 9195);

    private final Map<String, InstanceInfo> instanceStorage = new HashMap<>();

    private final Map<String, EurekaEventListener> eurekaEventStorage = new HashMap<>();

    private MockedConstruction<DiscoveryClient> discoveryClientMockedConstruction;

    @BeforeEach
    public void setUp() throws Exception {
        repository = new EurekaInstanceRegisterRepository();
        Class<? extends EurekaInstanceRegisterRepository> clazz = repository.getClass();

        Field eurekaClientField = clazz.getDeclaredField("eurekaClient");
        eurekaClientField.setAccessible(true);
        eurekaClientField.set(repository, mockEurekaClient());

        RegisterConfig registerConfig = new RegisterConfig();
        registerConfig.setServerLists("");
        repository.init(registerConfig);

        // mock the function discoveryClient#register().
        discoveryClientMockedConstruction = mockConstruction(DiscoveryClient.class, (mock, context) -> {
            InstanceInfo.Builder builder = repository.instanceInfoBuilder();
            builder.setAppName(instance.getAppName())
                    .setIPAddr(instance.getHost())
                    .setHostName(instance.getHost())
                    .setPort(instance.getPort())
                    .setStatus(InstanceInfo.InstanceStatus.UP);
            InstanceInfo instanceInfo = builder.build();
            instanceStorage.put(instanceInfo.getAppName(), instanceInfo);
        });
    }

    private EurekaClient mockEurekaClient() {
        DiscoveryClient discoveryClient = mock(DiscoveryClient.class);

        doAnswer(invocationOnMock -> {
            eurekaEventStorage.clear();
            return null;
        }).when(discoveryClient).shutdown();

        return discoveryClient;
    }

    @Test
    public void persistInstance() {
        repository.persistInstance(instance);
        assertTrue(instanceStorage.containsKey(instance.getAppName().toUpperCase()));
        InstanceInfo instanceInfo = instanceStorage.get(instance.getAppName().toUpperCase());
        assertEquals(instance.getHost(), instanceInfo.getHostName());
        assertEquals(instance.getPort(), instanceInfo.getPort());
        assertEquals(instance.getAppName().toUpperCase(), instanceInfo.getAppName());
    }

    @Test
    public void testSelectInstancesAndWatcher() {
        repository.selectInstances(instance.getAppName());
        repository.close();
        assertTrue(eurekaEventStorage.isEmpty());
    }

    @Test
    public void testSelectedInstanceAddressMatchesUpdatedUpstream() throws ReflectiveOperationException {
        InstanceInfo previous = newInstance("instance-1");
        EurekaClient client = mock(EurekaClient.class);
        when(client.getInstancesByVipAddressAndAppName(nullable(String.class), eq(instance.getAppName()), anyBoolean()))
                .thenReturn(Collections.singletonList(previous));
        Field clientField = EurekaInstanceRegisterRepository.class.getDeclaredField("eurekaClient");
        clientField.setAccessible(true);
        clientField.set(repository, client);

        InstanceEntity selected = repository.selectInstances(instance.getAppName()).get(0);
        assertEquals("10.0.0.1", selected.getHost());
        assertEquals(selected.getUri().getHost(), selected.getHost());
        assertEquals(selected.getUri().getPort(), selected.getPort());

        InstanceInfo current = newInstance("instance-1");
        current.getMetadata().put("weight", "20");
        ChangedEventListener listener = notifyChange(Collections.singletonList(previous), Collections.singletonList(current));
        ArgumentCaptor<String> payload = ArgumentCaptor.forClass(String.class);
        verify(listener).onEvent(eq("SHENYU-INSTANCES"), payload.capture(), eq(ChangedEventListener.Event.UPDATED));
        assertEquals(selected.getHost() + ":" + selected.getPort(), GsonUtils.getInstance().fromJson(payload.getValue(), Map.class).get("url"));
        verifyNoMoreInteractions(listener);
    }

    @Test
    public void testWatchInstancesKeepsPollingAfterFailure() throws Exception {
        InstanceInfo info = mock(InstanceInfo.class);
        when(info.getAppName()).thenReturn(instance.getAppName());
        when(info.getIPAddr()).thenReturn("10.0.0.1");
        when(info.getPort()).thenReturn(8080);
        when(info.getInstanceId()).thenReturn("instance-1");
        when(info.getMetadata()).thenReturn(new HashMap<>());
        when(info.isPortEnabled(InstanceInfo.PortType.SECURE)).thenReturn(false);
        when(info.getStatus()).thenReturn(InstanceInfo.InstanceStatus.UP);

        EurekaClient failingThenRecovering = mock(EurekaClient.class);
        when(failingThenRecovering.getApplicationInfoManager()).thenReturn(mock(ApplicationInfoManager.class));
        when(failingThenRecovering.getInstancesByVipAddressAndAppName(nullable(String.class), eq(instance.getAppName()), anyBoolean()))
                .thenReturn(new ArrayList<>())
                .thenThrow(new RuntimeException("eureka temporarily unavailable"))
                .thenReturn(Collections.singletonList(info));

        Field eurekaClientField = repository.getClass().getDeclaredField("eurekaClient");
        eurekaClientField.setAccessible(true);
        eurekaClientField.set(repository, failingThenRecovering);

        ChangedEventListener listener = mock(ChangedEventListener.class);
        repository.watchInstances(instance.getAppName(), listener);

        verify(listener, timeout(5000).atLeastOnce())
                .onEvent(eq(instance.getAppName()), anyString(), eq(ChangedEventListener.Event.ADDED));
        repository.close();
    }

    @ParameterizedTest
    @CsvSource({"weight, 20", "protocol, https://", "props, new-props"})
    public void testMetadataChangeOnlyPublishesUpdate(final String field, final String value) throws ReflectiveOperationException {
        InstanceInfo previous = newInstance("instance-1");
        InstanceInfo current = newInstance("instance-1");
        current.getMetadata().put(field, value);
        assertEquals(previous, current);

        ChangedEventListener listener = notifyChange(Collections.singletonList(previous), Collections.singletonList(current));
        ArgumentCaptor<String> payload = ArgumentCaptor.forClass(String.class);
        verify(listener).onEvent(eq("SHENYU-INSTANCES"), payload.capture(), eq(ChangedEventListener.Event.UPDATED));
        assertEquals(value, GsonUtils.getInstance().fromJson(payload.getValue(), Map.class).get(field));
        verifyNoMoreInteractions(listener);
        assertEquals(InstanceInfo.InstanceStatus.UP, previous.getStatus());
    }

    @Test
    public void testUnchangedUpstreamDoesNotPublishEvent() throws ReflectiveOperationException {
        InstanceInfo previous = newInstance("instance-1");
        InstanceInfo current = newInstance("instance-1");
        current.setLastDirtyTimestamp(123L);
        ChangedEventListener listener = notifyChange(Collections.singletonList(previous), Collections.singletonList(current));
        verifyNoMoreInteractions(listener);
    }

    @Test
    public void testUnrelatedMetadataDoesNotPublishEvent() throws ReflectiveOperationException {
        InstanceInfo previous = newInstance("instance-1");
        InstanceInfo current = newInstance("instance-1");
        current.getMetadata().put("zone", "another-zone");
        ChangedEventListener listener = notifyChange(Collections.singletonList(previous), Collections.singletonList(current));
        verifyNoMoreInteractions(listener);
    }

    @Test
    public void testDifferentInstanceIdsOnlyPublishAddAndDelete() throws ReflectiveOperationException {
        ChangedEventListener listener = notifyChange(Collections.singletonList(newInstance("instance-1")), Collections.singletonList(newInstance("instance-2")));
        ArgumentCaptor<String> payload = ArgumentCaptor.forClass(String.class);
        verify(listener).onEvent(eq("SHENYU-INSTANCES"), anyString(), eq(ChangedEventListener.Event.ADDED));
        verify(listener).onEvent(eq("SHENYU-INSTANCES"), payload.capture(), eq(ChangedEventListener.Event.DELETED));
        assertEquals(1.0, GsonUtils.getInstance().fromJson(payload.getValue(), Map.class).get("status"));
        verifyNoMoreInteractions(listener);
    }

    private ChangedEventListener notifyChange(final List<InstanceInfo> previous, final List<InstanceInfo> current) throws ReflectiveOperationException {
        Method compare = EurekaInstanceRegisterRepository.class.getDeclaredMethod("compareInstances", Set.class, Set.class, ChangedEventListener.class);
        compare.setAccessible(true);
        ChangedEventListener listener = mock(ChangedEventListener.class);
        compare.invoke(repository, new HashSet<>(previous), new HashSet<>(current), listener);
        return listener;
    }

    private InstanceInfo newInstance(final String instanceId) {
        Map<String, String> metadata = new HashMap<>();
        metadata.put("weight", "10");
        metadata.put("protocol", "http://");
        metadata.put("props", "old-props");
        return InstanceInfo.Builder.newBuilder((VipAddressResolver) vipAddress -> vipAddress)
                .setInstanceId(instanceId)
                .setAppName(instance.getAppName())
                .setHostName("shenyu-host")
                .setIPAddr("10.0.0.1")
                .setPort(8080)
                .setStatus(InstanceInfo.InstanceStatus.UP)
                .setMetadata(metadata)
                .build();
    }

    @AfterEach
    public void clear() {
        discoveryClientMockedConstruction.close();
    }
}
