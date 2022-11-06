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

package org.apache.shenyu.register.instance.eureka;

import com.netflix.appinfo.InstanceInfo;
import com.netflix.discovery.DiscoveryClient;
import com.netflix.discovery.EurekaClient;
import com.netflix.discovery.EurekaEventListener;
import com.netflix.discovery.shared.transport.EurekaHttpClient;
import com.netflix.discovery.shared.transport.EurekaHttpResponse;
import com.netflix.discovery.shared.transport.jersey.JerseyApplicationClient;
import org.apache.shenyu.register.instance.api.entity.InstanceEntity;
import org.apache.shenyu.register.instance.api.watcher.WatcherListener;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;

public final class EurekaInstanceRegisterRepositoryTest {

    private EurekaInstanceRegisterRepository repository;

    private final String instanceId = "shenyu-instances";

    private final Map<String, InstanceInfo> instanceStorage = new HashMap<>();

    private final Map<String, EurekaEventListener> eurekaEventStorage = new HashMap<>();

    @BeforeEach
    public void setUp() throws Exception {
        repository = new EurekaInstanceRegisterRepository();
        Class<? extends EurekaInstanceRegisterRepository> clazz = repository.getClass();

        Field eurekaClientField = clazz.getDeclaredField("eurekaClient");
        eurekaClientField.setAccessible(true);
        eurekaClientField.set(repository, mockEurekaClient());

        Field eurekaHttpClientField = clazz.getDeclaredField("eurekaHttpClient");
        eurekaHttpClientField.setAccessible(true);
        eurekaHttpClientField.set(repository, mockEurekaHttpClient());
    }

    private EurekaClient mockEurekaClient() {
        DiscoveryClient discoveryClient = mock(DiscoveryClient.class);

        doAnswer(invocationOnMock -> {
            eurekaEventStorage.put(instanceId, invocationOnMock.getArgument(0));
            return null;
        }).when(discoveryClient).registerEventListener(any());

        doAnswer(invocationOnMock -> {
            eurekaEventStorage.clear();
            return null;
        }).when(discoveryClient).shutdown();

        return discoveryClient;
    }

    private EurekaHttpClient mockEurekaHttpClient() {
        EurekaHttpClient eurekaHttpClient = mock(JerseyApplicationClient.class);
        doAnswer(invocationOnMock -> {
            InstanceInfo instanceInfo = invocationOnMock.getArgument(0);
            instanceStorage.put(instanceInfo.getAppName(), instanceInfo);
            return EurekaHttpResponse.anEurekaHttpResponse(204, "response success")
                    .build();
        }).when(eurekaHttpClient).register(any());
        return eurekaHttpClient;
    }

    @Test
    public void persistInstance() {
        InstanceEntity data = InstanceEntity.builder()
                .appName(instanceId)
                .host("shenyu-host")
                .port(9195)
                .build();
        repository.persistInstance(data);
        assertTrue(instanceStorage.containsKey(data.getAppName().toUpperCase()));
        InstanceInfo instanceInfo = instanceStorage.get(data.getAppName().toUpperCase());
        assertEquals(data.getHost(), instanceInfo.getHostName());
        assertEquals(data.getPort(), instanceInfo.getPort());
        assertEquals(data.getAppName().toUpperCase(), instanceInfo.getAppName());
    }

    @Test
    public void testSelectInstancesAndWatcher() {
        WatcherListener watcherListener = mock(WatcherListener.class);
        repository.selectInstancesAndWatcher(instanceId, watcherListener);
        assertTrue(eurekaEventStorage.containsKey(instanceId));
        repository.close();
        assertTrue(eurekaEventStorage.isEmpty());
    }
}
