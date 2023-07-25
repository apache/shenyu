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

package org.apache.shenyu.registry.polaris;

import com.tencent.polaris.api.core.ConsumerAPI;
import com.tencent.polaris.api.core.ProviderAPI;
import com.tencent.polaris.api.exception.PolarisException;
import com.tencent.polaris.api.pojo.Instance;
import com.tencent.polaris.api.rpc.GetHealthyInstancesRequest;
import com.tencent.polaris.api.rpc.InstanceRegisterRequest;
import java.lang.reflect.Field;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import org.apache.shenyu.registry.api.entity.InstanceEntity;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;

public class PolarisInstanceRegisterRepositoryTest {

    private PolarisInstanceRegisterRepository repository;

    private final Map<String, Instance> storage = new HashMap<>();

    @BeforeEach
    public void setUp() throws NoSuchFieldException, IllegalAccessException, PolarisException {
        this.repository = new PolarisInstanceRegisterRepository();
        Class<? extends PolarisInstanceRegisterRepository> clazz = this.repository.getClass();

        setField(clazz, "namespace", "shenyu");
        setField(clazz, "consumerAPI", mockConsumerAPI());
        setField(clazz, "providerAPI", mockProviderAPI());

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
            final InstanceRegisterRequest req = invocationOnMock.getArgument(0);
            storage.put(req.getService(), Instance.createDefaultInstance(req.getInstanceId(), req.getNamespace(), req.getService(), req.getHost(), req.getPort()));
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
            final GetHealthyInstancesRequest request = invocationOnMock.getArgument(0);
            return Collections.singletonList(storage.get(request.getService()));
        }).when(consumerAPI).getHealthyInstances(any());

        doAnswer(invocationOnMock -> {
            storage.clear();
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

        final String key = "shenyu-test";
        repository.persistInstance(data);
        assertTrue(storage.containsKey(key));

        final Instance instance = storage.get(key);
        assertEquals(data.getHost(), instance.getHost());
        assertEquals(data.getPort(), instance.getPort());
        assertEquals(data.getAppName(), instance.getService());
        repository.close();
    }

    @Test
    public void testSelectInstancesAndWatcher() {
        String selectKey = "shenyu-instances";
        repository.selectInstances(selectKey);
        repository.close();
    }
}
