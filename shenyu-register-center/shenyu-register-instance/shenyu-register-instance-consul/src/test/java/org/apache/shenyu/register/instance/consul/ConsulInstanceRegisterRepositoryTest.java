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
import com.ecwid.consul.v1.agent.model.NewCheck;
import com.google.common.collect.Lists;
import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.dto.InstanceRegisterDTO;
import org.apache.shenyu.register.common.path.RegisterPathConstants;
import org.apache.shenyu.register.common.subsriber.WatcherListener;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.when;

class ConsulInstanceRegisterRepositoryTest {

    private ConsulInstanceRegisterRepository repository;

    private final Map<String, String> consulBroker = new HashMap<>();

    @BeforeEach
    public void setUp() throws NoSuchFieldException, IllegalAccessException {
        this.repository = new ConsulInstanceRegisterRepository();
        Class<? extends ConsulInstanceRegisterRepository> clazz = this.repository.getClass();

        Field consulClientField = clazz.getDeclaredField("consulClient");
        consulClientField.setAccessible(true);
        consulClientField.set(repository, mockConsulClient());

        Field checkField = clazz.getDeclaredField("check");
        checkField.setAccessible(true);
        checkField.set(repository, mockNewCheck());

        consulBroker.clear();
    }

    private ConsulClient mockConsulClient() {
        ConsulClient consulClient = mock(ConsulClient.class);

        doAnswer(invocationOnMock -> {
            String key = invocationOnMock.getArgument(0);
            String value = invocationOnMock.getArgument(1);
            consulBroker.put(key, value);
            return null;
        }).when(consulClient).setKVValue(anyString(), anyString());

        return consulClient;
    }

    private NewCheck mockNewCheck() {
        return mock(NewCheck.class);
    }

    @Test
    public void testPersistInstance() {
        InstanceRegisterDTO data = InstanceRegisterDTO.builder()
                .appName("shenyu-test")
                .host("shenyu-host")
                .port(9195)
                .build();

        final String realNode = "/shenyu/register/instance/shenyu-host:9195";
        repository.persistInstance(data);
        assertTrue(consulBroker.containsKey(realNode));
        assertEquals(GsonUtils.getInstance().toJson(data), consulBroker.get(realNode));
        repository.close();
    }

    @Test
    public void testSelectInstancesAndWatcher() {

        InstanceRegisterDTO data = InstanceRegisterDTO.builder()
                .appName("shenyu-test")
                .host("shenyu-host")
                .port(9195)
                .build();

        try (MockedConstruction<ConsulClient> construction = mockConstruction(ConsulClient.class, (mock, context) -> {
            when(mock.agentCheckRegister(any())).thenReturn(any());
        })) {
            ShenyuConfig.RegisterConfig instanceConfig = new ShenyuConfig.RegisterConfig();
            final ConsulInstanceRegisterRepository repository = mock(ConsulInstanceRegisterRepository.class);
            Properties properties = new Properties();
            properties.setProperty("enabledServerRebalance", "true");
            instanceConfig.setProps(properties);
            repository.init(instanceConfig);
            when(repository.getInstanceRegisterDTOListByKey(anyString())).thenReturn(Lists.newArrayList(data));
            repository.selectInstancesAndWatcher(RegisterPathConstants.buildInstanceParentPath(), mock(WatcherListener.class));
            repository.close();
        }
    }

}
