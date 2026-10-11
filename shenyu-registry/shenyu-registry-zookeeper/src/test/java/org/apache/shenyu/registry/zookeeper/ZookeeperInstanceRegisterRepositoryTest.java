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

package org.apache.shenyu.registry.zookeeper;

import org.apache.curator.framework.CuratorFramework;
import org.apache.curator.framework.CuratorFrameworkFactory;
import org.apache.curator.framework.api.CuratorWatcher;
import org.apache.curator.framework.recipes.cache.ChildData;
import org.apache.curator.framework.recipes.cache.CuratorCacheListener;
import org.apache.curator.framework.listen.Listenable;
import org.apache.curator.framework.state.ConnectionState;
import org.apache.curator.framework.state.ConnectionStateListener;
import org.apache.curator.retry.ExponentialBackoffRetry;
import org.apache.curator.test.TestingServer;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.registry.api.event.ChangedEventListener;
import org.apache.shenyu.infra.zookeeper.client.ZookeeperClient;
import org.apache.shenyu.registry.api.config.RegisterConfig;
import org.apache.shenyu.registry.api.entity.InstanceEntity;
import org.apache.shenyu.registry.api.path.InstancePathConstants;
import org.apache.zookeeper.CreateMode;
import org.apache.zookeeper.WatchedEvent;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.when;

public final class ZookeeperInstanceRegisterRepositoryTest {

    @Test
    public void testSelectAndWatchNativeInstances() throws Exception {
        try (TestingServer server = new TestingServer();
                CuratorFramework writer = CuratorFrameworkFactory.newClient(server.getConnectString(), new ExponentialBackoffRetry(100, 3))) {
            writer.start();
            assertTrue(writer.blockUntilConnected(5, TimeUnit.SECONDS));
            InstanceEntity instance = new InstanceEntity("native-test", "127.0.0.1", 9195);
            instance.setWeight(50);
            String parent = InstancePathConstants.buildInstanceParentPath(instance.getAppName());
            String path = parent + "/127.0.0.1:9195";
            String json = GsonUtils.getInstance().toJson(instance);
            writer.create().creatingParentsIfNeeded().withMode(CreateMode.EPHEMERAL).forPath(path, json.getBytes(StandardCharsets.UTF_8));
            ZookeeperInstanceRegisterRepository repository = new ZookeeperInstanceRegisterRepository();
            try {
                repository.init(new RegisterConfig("zookeeper", server.getConnectString(), new Properties()));
                List<InstanceEntity> instances = repository.selectInstances(parent);
                assertEquals(1, instances.size());
                assertEquals("http://127.0.0.1:9195", instances.get(0).getUri().toString());
                BlockingQueue<Map.Entry<ChangedEventListener.Event, String>> events = new LinkedBlockingQueue<>();
                repository.watchInstances(parent, (key, value, event) -> events.add(Map.entry(event, value)));
                assertInstanceEvent(events, ChangedEventListener.Event.ADDED, 50);
                instance.setWeight(80);
                writer.setData().forPath(path, GsonUtils.getInstance().toJson(instance).getBytes(StandardCharsets.UTF_8));
                assertInstanceEvent(events, ChangedEventListener.Event.UPDATED, 80);
                writer.delete().forPath(path);
                assertInstanceEvent(events, ChangedEventListener.Event.DELETED, 80);
            } finally {
                repository.close();
            }
        }
    }

    private void assertInstanceEvent(final BlockingQueue<Map.Entry<ChangedEventListener.Event, String>> events,
                                     final ChangedEventListener.Event expected, final int weight) throws InterruptedException {
        Map.Entry<ChangedEventListener.Event, String> event = events.poll(5, TimeUnit.SECONDS);
        assertNotNull(event);
        assertEquals(expected, event.getKey());
        DiscoveryUpstreamData upstream = GsonUtils.getInstance().fromJson(event.getValue(), DiscoveryUpstreamData.class);
        assertEquals("127.0.0.1:9195", upstream.getUrl());
        assertEquals(weight, upstream.getWeight());
    }

    @Test
    public void testZookeeperInstanceRegisterRepository() {
        final Listenable listenable = mock(Listenable.class);
        try (MockedConstruction<ZookeeperClient> construction = mockConstruction(ZookeeperClient.class, (mock, context) -> {
            final CuratorFramework curatorFramework = mock(CuratorFramework.class);
            when(mock.getClient()).thenReturn(curatorFramework);
            when(curatorFramework.getConnectionStateListenable()).thenReturn(listenable);
        })) {
            final ZookeeperInstanceRegisterRepository repository = new ZookeeperInstanceRegisterRepository();
            RegisterConfig config = new RegisterConfig();
            repository.init(config);
            final Properties configProps = config.getProps();
            configProps.setProperty("digest", "digest");
            List<ConnectionStateListener> connectionStateListeners = new ArrayList<>();
            doAnswer(invocationOnMock -> {
                connectionStateListeners.add(invocationOnMock.getArgument(0));
                return null;
            }).when(listenable).addListener(any());
            repository.init(config);
            repository.persistInstance(mock(InstanceEntity.class));
            connectionStateListeners.forEach(connectionStateListener -> connectionStateListener.stateChanged(null, ConnectionState.RECONNECTED));
            repository.close();
        }
    }

    @Test
    public void testSelectInstancesAndWatcher() throws Exception {
        InstanceEntity data = InstanceEntity.builder()
                .appName("shenyu-test")
                .host("shenyu-host")
                .port(9195)
                .build();
        final Listenable listenable = mock(Listenable.class);
        final CuratorWatcher[] watcherArr = new CuratorWatcher[1];
        final boolean[] hasInstance = {true};

        try (MockedConstruction<ZookeeperClient> construction = mockConstruction(ZookeeperClient.class, (mock, context) -> {
            final CuratorFramework curatorFramework = mock(CuratorFramework.class);
            when(mock.getClient()).thenReturn(curatorFramework);
            when(mock.subscribeChildrenChanges(anyString(), any(CuratorWatcher.class))).thenAnswer(invocation -> {
                Object[] args = invocation.getArguments();
                watcherArr[0] = (CuratorWatcher) args[1];
                return hasInstance[0] ? Collections.singletonList("shenyu-test") : Collections.emptyList();
            });
            when(mock.get(anyString())).thenReturn(GsonUtils.getInstance().toJson(data));
            when(curatorFramework.getConnectionStateListenable()).thenReturn(listenable);
        })) {
            final ZookeeperInstanceRegisterRepository repository = new ZookeeperInstanceRegisterRepository();
            RegisterConfig config = new RegisterConfig();
            repository.init(config);
            final Properties configProps = config.getProps();
            configProps.setProperty("digest", "digest");
            repository.init(config);
            String selectKey = data.getAppName();
            assertEquals(1, repository.selectInstances(selectKey).size());
            WatchedEvent mockEvent = mock(WatchedEvent.class);
            when(mockEvent.getPath()).thenReturn(null);
            hasInstance[0] = false;
            watcherArr[0].process(mockEvent);
            assertTrue(repository.selectInstances(selectKey).isEmpty());
            hasInstance[0] = true;
            watcherArr[0].process(mockEvent);
            assertEquals(1, repository.selectInstances(selectKey).size());
            repository.close();
        }
    }

    @Test
    public void testWatchInstancesEmitsDeletedEventForEphemeralNode() {
        final Listenable listenable = mock(Listenable.class);
        try (MockedConstruction<ZookeeperClient> construction = mockConstruction(ZookeeperClient.class, (mock, context) -> {
            final CuratorFramework curatorFramework = mock(CuratorFramework.class);
            when(mock.getClient()).thenReturn(curatorFramework);
            when(curatorFramework.getConnectionStateListenable()).thenReturn(listenable);
        })) {
            final ZookeeperInstanceRegisterRepository repository = new ZookeeperInstanceRegisterRepository();
            RegisterConfig config = new RegisterConfig();
            repository.init(config);
            ZookeeperClient client = construction.constructed().get(0);
            org.mockito.ArgumentCaptor<CuratorCacheListener> captor = org.mockito.ArgumentCaptor.forClass(CuratorCacheListener.class);
            ChangedEventListener changedEventListener = mock(ChangedEventListener.class);
            repository.watchInstances("/shenyu/register/instance", changedEventListener);
            org.mockito.Mockito.verify(client).addCache(org.mockito.ArgumentMatchers.eq("/shenyu/register/instance"), captor.capture());
            org.apache.zookeeper.data.Stat stat = new org.apache.zookeeper.data.Stat();
            stat.setEphemeralOwner(1L);
            String path = InstancePathConstants.buildInstanceParentPath("app") + "/host:9195";
            String instanceData = "{\"appName\":\"app\",\"host\":\"host\",\"port\":9195,\"status\":0,\"weight\":50}";
            ChildData deletedNode = new ChildData(path, stat, instanceData.getBytes(java.nio.charset.StandardCharsets.UTF_8));
            // Curator delivers NODE_DELETED with a null new ChildData and the node in oldData
            captor.getValue().event(CuratorCacheListener.Type.NODE_DELETED, deletedNode, null);
            org.mockito.ArgumentCaptor<String> value = org.mockito.ArgumentCaptor.forClass(String.class);
            org.mockito.Mockito.verify(changedEventListener).onEvent(org.mockito.ArgumentMatchers.eq(path), value.capture(), org.mockito.ArgumentMatchers.eq(ChangedEventListener.Event.DELETED));
            org.apache.shenyu.common.dto.DiscoveryUpstreamData upstream = GsonUtils.getInstance().fromJson(value.getValue(), org.apache.shenyu.common.dto.DiscoveryUpstreamData.class);
            assertEquals("host:9195", upstream.getUrl());
            assertEquals(50, upstream.getWeight());
            assertEquals(0, upstream.getStatus());
        }
    }

}
