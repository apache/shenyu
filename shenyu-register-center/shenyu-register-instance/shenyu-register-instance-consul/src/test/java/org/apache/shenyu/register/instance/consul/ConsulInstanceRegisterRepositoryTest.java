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

package org.apache.shenyu.register.instance.consul;

import com.ecwid.consul.v1.ConsulClient;
import com.ecwid.consul.v1.agent.model.NewService;
import org.apache.shenyu.register.instance.api.config.RegisterConfig;
import org.apache.shenyu.register.instance.api.entity.InstanceEntity;
import org.apache.shenyu.register.instance.api.path.InstancePathConstants;
import org.apache.shenyu.register.instance.api.watcher.WatcherListener;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;

import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.when;

class ConsulInstanceRegisterRepositoryTest {

    private ConsulInstanceRegisterRepository repository;

    private final Map<String, NewService> consulBroker = new HashMap<>();

    @BeforeEach
    public void setUp() throws NoSuchFieldException, IllegalAccessException {
        this.repository = new ConsulInstanceRegisterRepository();
        Class<? extends ConsulInstanceRegisterRepository> clazz = this.repository.getClass();

        Field consulClientField = clazz.getDeclaredField("consulClient");
        consulClientField.setAccessible(true);
        consulClientField.set(repository, mockConsulClient());

        Field checkField = clazz.getDeclaredField("newService");
        checkField.setAccessible(true);
        checkField.set(repository, mockNewService());
    
        Field ttl = clazz.getDeclaredField("ttlScheduler");
        ttl.setAccessible(true);
        ttl.set(repository, mock(TtlScheduler.class));

        Field tokenField = clazz.getDeclaredField("token");
        tokenField.setAccessible(true);
        tokenField.set(repository, "");

        consulBroker.clear();
    }

    private ConsulClient mockConsulClient() {
        ConsulClient consulClient = mock(ConsulClient.class);

        doAnswer(invocationOnMock -> {
            NewService newService = invocationOnMock.getArgument(0);
            consulBroker.put(newService.getName(), newService);
            return null;
        }).when(consulClient).agentServiceRegister(any(NewService.class), anyString());

        return consulClient;
    }

    private NewService mockNewService() {
        return mock(NewService.class);
    }

    @Test
    public void testPersistInstance() {
        InstanceEntity data = InstanceEntity.builder()
                .appName("shenyu-test")
                .host("shenyu-host")
                .port(9195)
                .build();

        repository.persistInstance(data);
        //assertTrue(consulBroker.containsKey(data.getAppName()));
    }

    @Test
    public void testSelectInstancesAndWatcher() {
        InstanceEntity data = InstanceEntity.builder()
                .appName("shenyu-test")
                .host("shenyu-host")
                .port(9195)
                .build();

        try (MockedConstruction<ConsulClient> construction = mockConstruction(ConsulClient.class, (mock, context) -> {
            when(mock.agentCheckRegister(any())).thenReturn(any());
        })) {
            RegisterConfig instanceConfig = new RegisterConfig();
            final ConsulInstanceRegisterRepository repository = mock(ConsulInstanceRegisterRepository.class);
            Properties properties = new Properties();
            properties.setProperty("enabledServerRebalance", "true");
            instanceConfig.setProps(properties);
            repository.init(instanceConfig);
            repository.selectInstancesAndWatcher(InstancePathConstants.buildInstanceParentPath(), mock(WatcherListener.class));
            repository.close();
        }
    }
}
