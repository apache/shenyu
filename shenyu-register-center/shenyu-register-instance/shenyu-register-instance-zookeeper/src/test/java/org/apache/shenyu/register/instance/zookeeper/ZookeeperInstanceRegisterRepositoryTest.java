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

package org.apache.shenyu.register.instance.zookeeper;

import org.apache.curator.framework.CuratorFramework;
import org.apache.curator.framework.api.CuratorWatcher;
import org.apache.curator.framework.listen.Listenable;
import org.apache.curator.framework.state.ConnectionState;
import org.apache.curator.framework.state.ConnectionStateListener;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.instance.api.config.RegisterConfig;
import org.apache.shenyu.register.instance.api.entity.InstanceEntity;
import org.apache.shenyu.register.instance.api.path.InstancePathConstants;
import org.apache.shenyu.register.instance.api.watcher.WatcherListener;
import org.apache.zookeeper.WatchedEvent;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Properties;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.when;

public final class ZookeeperInstanceRegisterRepositoryTest {

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
            connectionStateListeners.forEach(connectionStateListener -> {
                connectionStateListener.stateChanged(null, ConnectionState.RECONNECTED);
            });
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

        try (MockedConstruction<ZookeeperClient> construction = mockConstruction(ZookeeperClient.class, (mock, context) -> {
            final CuratorFramework curatorFramework = mock(CuratorFramework.class);
            when(mock.getClient()).thenReturn(curatorFramework);
            when(mock.subscribeChildrenChanges(anyString(), any(CuratorWatcher.class))).thenAnswer(invocation -> {
                Object[] args = invocation.getArguments();
                watcherArr[0] = (CuratorWatcher) args[1];
                return Collections.singletonList("shenyu-test");
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
            repository.selectInstancesAndWatcher(InstancePathConstants.buildInstanceParentPath(), mock(WatcherListener.class));
            WatchedEvent mockEvent = mock(WatchedEvent.class);
            when(mockEvent.getPath()).thenReturn(InstancePathConstants.buildInstanceParentPath());
            watcherArr[0].process(mockEvent);
            repository.close();
        }
    }

}
