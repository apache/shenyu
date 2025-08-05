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

package org.apache.shenyu.registry.etcd;

import io.etcd.jetcd.Client;
import io.etcd.jetcd.ClientBuilder;
import io.etcd.jetcd.Lease;
import io.etcd.jetcd.lease.LeaseGrantResponse;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.infra.etcd.client.EtcdClient;
import org.apache.shenyu.registry.api.config.RegisterConfig;
import org.apache.shenyu.registry.api.entity.InstanceEntity;
import org.apache.shenyu.registry.api.path.InstancePathConstants;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;

import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

/**
 * The type Etcd instance register repository test.
 */
public final class EtcdInstanceRegisterRepositoryTest {

    private EtcdInstanceRegisterRepository repository;

    private final Map<String, String> etcdBroker = new HashMap<>();
    
    /**
     * Sets up.
     *
     * @throws NoSuchFieldException the no such field exception
     * @throws IllegalAccessException the illegal access exception
     */
    @BeforeEach
    public void setUp() throws NoSuchFieldException, IllegalAccessException {
        this.repository = new EtcdInstanceRegisterRepository();
        Class<? extends EtcdInstanceRegisterRepository> clazz = this.repository.getClass();

        String fieldString = "client";
        Field field = clazz.getDeclaredField(fieldString);
        field.setAccessible(true);
        field.set(repository, mockEtcdClient());

        etcdBroker.clear();
    }

    private EtcdClient mockEtcdClient() {
        EtcdClient etcdClient = mock(EtcdClient.class);

        doAnswer(invocationOnMock -> {
            String key = invocationOnMock.getArgument(0);
            String value = invocationOnMock.getArgument(1);
            etcdBroker.put(key, value);
            return null;
        }).when(etcdClient).putEphemeral(anyString(), anyString());

        doAnswer(invocationOnMock -> {
            etcdBroker.clear();
            return null;
        }).when(etcdClient).close();
        return etcdClient;
    }
    
    /**
     * Test persist instance.
     */
    @Test
    public void testPersistInstance() {
        InstanceEntity data = InstanceEntity.builder()
                .appName("shenyu-test")
                .host("shenyu-host")
                .port(9195)
                .build();

        final String realNode = "/shenyu/register/instance/shenyu-test/shenyu-host:9195";
        repository.persistInstance(data);
        assertTrue(etcdBroker.containsKey(realNode));
        assertEquals(GsonUtils.getInstance().toJson(data), etcdBroker.get(realNode));
        repository.close();
    }
    
    /**
     * Init test.
     */
    @Test
    public void initTest() {
        try (MockedStatic<Client> clientMockedStatic = mockStatic(Client.class)) {
            final ClientBuilder clientBuilder = mock(ClientBuilder.class);
            clientMockedStatic.when(Client::builder).thenReturn(clientBuilder);
            when(clientBuilder.endpoints(anyString())).thenReturn(clientBuilder);
            final Client client = mock(Client.class);
            when(clientBuilder.endpoints(anyString()).build()).thenReturn(client);
            final Lease lease = mock(Lease.class);
            when(client.getLeaseClient()).thenReturn(lease);
            final CompletableFuture<LeaseGrantResponse> completableFuture = mock(CompletableFuture.class);
            final LeaseGrantResponse leaseGrantResponse = mock(LeaseGrantResponse.class);

            when(client.getLeaseClient().grant(anyLong())).thenReturn(completableFuture);
            when(completableFuture.get()).thenReturn(leaseGrantResponse);
            RegisterConfig config = new RegisterConfig();
            config.setServerLists("url");
            Assertions.assertDoesNotThrow(() -> repository.init(config));
        } catch (Exception e) {
            throw new ShenyuException(e);
        }
    }
    
    /**
     * Test select instances and watcher.
     */
    @Test
    public void testSelectInstancesAndWatcher() {
        InstanceEntity data = InstanceEntity.builder()
                .appName("shenyu-test")
                .host("shenyu-host")
                .port(9195)
                .build();

        try (MockedConstruction<EtcdClient> construction = mockConstruction(EtcdClient.class, (mock, context) -> {
        })) {
            final EtcdInstanceRegisterRepository repository = new EtcdInstanceRegisterRepository();
            RegisterConfig config = new RegisterConfig();
            repository.init(config);
            repository.persistInstance(data);
            repository.selectInstances(InstancePathConstants.buildInstanceParentPath());
            repository.close();
        }
    }
}
