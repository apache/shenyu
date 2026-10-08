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
import com.netflix.discovery.DiscoveryClient;
import com.netflix.discovery.EurekaClient;
import com.netflix.discovery.EurekaEventListener;
import org.apache.shenyu.registry.api.config.RegisterConfig;
import org.apache.shenyu.registry.api.entity.InstanceEntity;
import org.apache.shenyu.registry.api.event.ChangedEventListener;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

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

    @AfterEach
    public void clear() {
        discoveryClientMockedConstruction.close();
    }
}
