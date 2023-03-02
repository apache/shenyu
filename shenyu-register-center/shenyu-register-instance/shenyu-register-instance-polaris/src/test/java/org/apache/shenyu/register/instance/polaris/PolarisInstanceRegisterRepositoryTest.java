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

package org.apache.shenyu.register.instance.polaris;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.tencent.polaris.api.core.ConsumerAPI;
import com.tencent.polaris.api.core.ProviderAPI;
import com.tencent.polaris.api.exception.PolarisException;
import com.tencent.polaris.api.listener.ServiceListener;
import com.tencent.polaris.api.pojo.Instance;
import com.tencent.polaris.api.rpc.GetHealthyInstancesRequest;
import com.tencent.polaris.api.rpc.InstanceRegisterRequest;
import com.tencent.polaris.api.rpc.InstancesResponse;
import com.tencent.polaris.api.rpc.WatchServiceRequest;
import com.tencent.polaris.api.rpc.WatchServiceResponse;
import org.apache.shenyu.register.instance.api.entity.InstanceEntity;
import org.apache.shenyu.register.instance.api.watcher.WatcherListener;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * PolarisInstanceRegisterRepositoryTest.
 */
public class PolarisInstanceRegisterRepositoryTest {

    private static final String NAMESPACE = "namespace";

    private PolarisInstanceRegisterRepository repository;

    private final Map<String, List<Instance>> storage = Maps.newHashMap();

    private final Map<String, List<ServiceListener>> listenerStorage = Maps.newHashMap();

    @BeforeEach
    public void setUp() throws NoSuchFieldException, IllegalAccessException, PolarisException {
        this.repository = new PolarisInstanceRegisterRepository();
        final Class<? extends PolarisInstanceRegisterRepository> clazz = this.repository.getClass();

        Properties props = new Properties();
        props.setProperty("namespace", NAMESPACE);
        props.setProperty("version", "token");
        props.setProperty("weight", "0");
        setField(clazz, "providerAPI", mockProviderAPI());
        setField(clazz, "consumerAPI", mockConsumerAPI());
        setField(clazz, "props", props);
        setField(clazz, "namespace", NAMESPACE);

        storage.clear();
    }

    private <T> void setField(final Class<T> clazz, final String fieldName, final Object value) throws NoSuchFieldException, IllegalAccessException {
        Field field = clazz.getDeclaredField(fieldName);
        field.setAccessible(true);
        field.set(repository, value);
        field.setAccessible(false);
    }

    private ProviderAPI mockProviderAPI() throws PolarisException {
        ProviderAPI providerAPI = mock(ProviderAPI.class);

        doAnswer(invocationOnMock -> {
            InstanceRegisterRequest request = invocationOnMock.getArgument(0);
            String service = request.getService();
            String namespace = request.getNamespace();
            String host = request.getHost();
            Integer post = request.getPort();
            Instance instance = Instance.createDefaultInstance("1", namespace, service, host, post);
            List<Instance> list = storage.getOrDefault(service + namespace, Lists.newArrayList());
            list.add(instance);
            storage.put(service + namespace, list);
            return null;
        }).when(providerAPI).registerInstance(any());

        doAnswer(invocationOnMock -> {
            storage.clear();
            return null;
        }).when(providerAPI).close();
        return providerAPI;
    }

    private ConsumerAPI mockConsumerAPI() throws PolarisException {
        ConsumerAPI consumerAPI = mock(ConsumerAPI.class);

        doAnswer(invocationOnMock -> {
            WatchServiceRequest request = invocationOnMock.getArgument(0);
            String service = request.getService();
            String namespace = request.getNamespace();
            List<ServiceListener> listeners = request.getListeners();
            listenerStorage.put(service + namespace, listeners);

            WatchServiceResponse response = mock(WatchServiceResponse.class);
            InstancesResponse resp = mock(InstancesResponse.class);
            when(response.getResponse()).thenReturn(resp);
            List<Instance> instances = storage.getOrDefault(service + namespace, Lists.newArrayList());
            when(resp.getInstances()).thenReturn(instances.toArray(instances.toArray(new Instance[0])));
            return response;
        }).when(consumerAPI).watchService(any());

        doAnswer(invocationOnMock -> {
            GetHealthyInstancesRequest request = invocationOnMock.getArgument(0);
            String service = request.getService();
            String namespace = request.getNamespace();
            List<Instance> instances = storage.get(service + namespace);

            InstancesResponse response = mock(InstancesResponse.class);
            when(response.getInstances()).thenReturn(instances.toArray(instances.toArray(new Instance[0])));
            return response;
        }).when(consumerAPI).getHealthyInstances(any());

        doAnswer(invocationOnMock -> {
            listenerStorage.clear();
            return null;
        }).when(consumerAPI).close();
        return consumerAPI;
    }

    @Test
    public void testPersistInstance() {
        InstanceEntity data = InstanceEntity.builder()
                .appName("shenyu-test")
                .host("shenyu-host")
                .port(9195)
                .build();

        final String key = "shenyu-test" + NAMESPACE;
        repository.persistInstance(data);
        assertTrue(storage.containsKey(key));

        final Instance instance = storage.get(key).get(0);
        assertEquals(data.getHost(), instance.getHost());
        assertEquals(data.getPort(), instance.getPort());
        assertEquals(data.getAppName(), instance.getService());
    }

    @Test
    public void testSelectInstancesAndWatcher() {
        final String selectKey = "shenyu-instances";
        WatcherListener watcherListener = mock(WatcherListener.class);
        List<InstanceEntity> instanceEntities = repository.selectInstancesAndWatcher(selectKey, watcherListener);
        assertNotNull(listenerStorage.get(selectKey + NAMESPACE));
        assertNotNull(instanceEntities);
    }

    @AfterEach
    public void clos() {
        repository.close();
    }

}
