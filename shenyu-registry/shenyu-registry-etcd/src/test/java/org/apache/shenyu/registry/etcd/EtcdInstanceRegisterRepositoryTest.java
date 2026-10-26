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

import io.etcd.jetcd.ByteSequence;
import io.etcd.jetcd.Client;
import io.etcd.jetcd.ClientBuilder;
import io.etcd.jetcd.KeyValue;
import io.etcd.jetcd.Lease;
import io.etcd.jetcd.Watch;
import io.etcd.jetcd.lease.LeaseGrantResponse;
import io.etcd.jetcd.watch.WatchEvent;
import io.etcd.jetcd.watch.WatchResponse;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.infra.etcd.client.EtcdClient;
import org.apache.shenyu.registry.api.config.RegisterConfig;
import org.apache.shenyu.registry.api.entity.InstanceEntity;
import org.apache.shenyu.registry.api.event.ChangedEventListener;
import org.apache.shenyu.registry.api.path.InstancePathConstants;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedStatic;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import static java.nio.charset.StandardCharsets.UTF_8;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * The type Etcd instance register repository test.
 */
public final class EtcdInstanceRegisterRepositoryTest {

    private EtcdInstanceRegisterRepository repository;

    private EtcdClient etcdClient;

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
        etcdClient = mock(EtcdClient.class);
        when(etcdClient.getKeysMapByPrefix(anyString())).thenReturn(Collections.emptyMap());

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
     * Test selecting instances does not create a persistent watcher.
     */
    @Test
    public void testSelectInstancesDoesNotCreateWatcher() {
        assertTrue(repository.selectInstances("shenyu-test").isEmpty());
        assertTrue(repository.selectInstances("shenyu-test").isEmpty());

        verify(etcdClient, times(2)).getKeysMapByPrefix(InstancePathConstants.buildInstanceParentPath("shenyu-test"));
        verify(etcdClient, never()).watchKeyChanges(anyString(), any(Watch.Listener.class));
    }

    /**
     * Test each selection reads current instances and fills their URIs.
     */
    @Test
    public void testSelectInstancesReadsCurrentInstances() {
        InstanceEntity data = InstanceEntity.builder()
                .appName("shenyu-test")
                .host("shenyu-host")
                .port(9195)
                .build();
        String prefix = InstancePathConstants.buildInstanceParentPath(data.getAppName());
        String node = InstancePathConstants.buildRealNode(prefix, "shenyu-host:9195");
        Map<String, String> serverNodes = new HashMap<>();
        when(etcdClient.getKeysMapByPrefix(prefix)).thenAnswer(invocation -> new HashMap<>(serverNodes));

        assertTrue(repository.selectInstances(data.getAppName()).isEmpty());
        serverNodes.put(node, GsonUtils.getInstance().toJson(data));
        List<InstanceEntity> instances = repository.selectInstances(data.getAppName());
        assertEquals(1, instances.size());
        assertEquals(data.getAppName(), instances.get(0).getAppName());
        assertEquals(data.getHost(), instances.get(0).getHost());
        assertEquals(data.getPort(), instances.get(0).getPort());
        assertEquals(URI.create("http://shenyu-host:9195"), instances.get(0).getUri());

        data.setWeight(10);
        data.setUri(URI.create("https://shenyu-host:9195"));
        serverNodes.put(node, GsonUtils.getInstance().toJson(data));
        instances = repository.selectInstances(data.getAppName());
        assertEquals(1, instances.size());
        assertEquals(10, instances.get(0).getWeight());
        assertEquals(URI.create("https://shenyu-host:9195"), instances.get(0).getUri());

        InstanceEntity another = new InstanceEntity(data.getAppName(), "another-host", 9196);
        serverNodes.put(InstancePathConstants.buildRealNode(prefix, "another-host:9196"), GsonUtils.getInstance().toJson(another));
        assertEquals(2, repository.selectInstances(data.getAppName()).size());
        serverNodes.remove(node);
        instances = repository.selectInstances(data.getAppName());
        assertEquals(1, instances.size());
        assertEquals(URI.create("http://another-host:9196"), instances.get(0).getUri());
        serverNodes.clear();
        assertTrue(repository.selectInstances(data.getAppName()).isEmpty());
        verify(etcdClient, times(6)).getKeysMapByPrefix(prefix);
    }

    /**
     * Test selecting instances preserves an explicit watcher and its notifications.
     */
    @Test
    public void testSelectInstancesPreservesExplicitWatcher() {
        String key = InstancePathConstants.buildInstanceParentPath("shenyu-test");
        ChangedEventListener listener = mock(ChangedEventListener.class);
        Watch.Watcher watcher = mock(Watch.Watcher.class);
        when(etcdClient.watchKeyChanges(eq(key), any(Watch.Listener.class))).thenReturn(watcher);

        repository.watchInstances(key, listener);
        repository.selectInstances("shenyu-test");
        ArgumentCaptor<Watch.Listener> captor = ArgumentCaptor.forClass(Watch.Listener.class);
        verify(etcdClient).watchKeyChanges(eq(key), captor.capture());
        verify(watcher, never()).close();
        String node = key + "/shenyu-host:9195";
        String value = "instance-data";
        KeyValue keyValue = mock(KeyValue.class);
        when(keyValue.getKey()).thenReturn(ByteSequence.from(node, UTF_8));
        when(keyValue.getValue()).thenReturn(ByteSequence.from(value, UTF_8));
        WatchResponse response = mock(WatchResponse.class);
        when(response.getEvents()).thenReturn(Collections.singletonList(new WatchEvent(keyValue, keyValue, WatchEvent.EventType.PUT)));

        captor.getValue().onNext(response);
        verify(listener).onEvent(node, value, ChangedEventListener.Event.ADDED);
    }
}
