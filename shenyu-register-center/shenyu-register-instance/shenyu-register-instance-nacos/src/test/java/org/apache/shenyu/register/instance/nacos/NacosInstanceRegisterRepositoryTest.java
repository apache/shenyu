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

package org.apache.shenyu.register.instance.nacos;

import com.alibaba.nacos.api.exception.NacosException;
import com.alibaba.nacos.api.naming.NamingService;
import com.alibaba.nacos.api.naming.pojo.Instance;
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
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;

public final class NacosInstanceRegisterRepositoryTest {

    private NacosInstanceRegisterRepository repository;

    private final Map<String, Instance> storage = new HashMap<>();

    @BeforeEach
    public void setUp() throws NoSuchFieldException, IllegalAccessException, NacosException {
        this.repository = new NacosInstanceRegisterRepository();
        Class<? extends NacosInstanceRegisterRepository> clazz = this.repository.getClass();

        Field field = clazz.getDeclaredField("namingService");
        field.setAccessible(true);
        field.set(repository, mockNamingService());

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
        WatcherListener watcherListener = mock(WatcherListener.class);
        repository.selectInstancesAndWatcher(selectKey, watcherListener);
        repository.close();
    }
}
