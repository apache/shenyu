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

package org.apache.shenyu.register.client.server.zookeeper;

import org.apache.curator.framework.CuratorFramework;
import org.apache.curator.framework.listen.Listenable;
import org.apache.curator.framework.recipes.cache.ChildData;
import org.apache.curator.framework.recipes.cache.TreeCacheEvent;
import org.apache.curator.framework.recipes.cache.TreeCacheListener;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.register.client.server.api.ShenyuClientServerRegisterPublisher;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Properties;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.when;

/**
 * Test for Zookeeper register center.
 */
public class ZookeeperServerRegisterRepositoryTest {

    @Test
    public void testZookeeperInstanceRegisterRepository() {
        final Listenable listenable = mock(Listenable.class);
        final List<TreeCacheListener> treeCacheListeners = new ArrayList<>();
        final CuratorFramework curatorFramework = mock(CuratorFramework.class);
        try (MockedConstruction<ZookeeperClient> construction = mockConstruction(ZookeeperClient.class, (mock, context) -> {
            when(mock.getClient()).thenReturn(curatorFramework);
            when(curatorFramework.getConnectionStateListenable()).thenReturn(listenable);
            doAnswer(invocationOnMock -> {
                treeCacheListeners.add(invocationOnMock.getArgument(1));
                return null;
            }).when(mock).addCache(any(), any());
            List<String> childrenList = new ArrayList<>(1);
            childrenList.add("");
            when(mock.getChildren(anyString())).thenReturn(childrenList);
            when(mock.get(anyString())).thenReturn("{}");
        })) {
            final ZookeeperClientServerRegisterRepository repository = new ZookeeperClientServerRegisterRepository();
            ShenyuRegisterCenterConfig config = new ShenyuRegisterCenterConfig();
            final ShenyuClientServerRegisterPublisher shenyuClientServerRegisterPublisher = mock(ShenyuClientServerRegisterPublisher.class);
            repository.init(shenyuClientServerRegisterPublisher, config);

            List<TreeCacheEvent> treeCacheEvent = new ArrayList<>();
            // register uri
            treeCacheEvent.add(treeCacheEvent("/shenyu/register/uri/test/test/test"));
            // register metadata
            treeCacheEvent.add(treeCacheEvent("/shenyu/register/metadata/test/test/test"));

            for (TreeCacheListener treeCacheListener : treeCacheListeners) {
                for (TreeCacheEvent event : treeCacheEvent) {
                    treeCacheListener.childEvent(curatorFramework, event);
                }
            }
            final TreeCacheListener treeCacheListener = treeCacheListeners.stream().findFirst().orElse(null);
            if (Objects.nonNull(treeCacheListener)) {
                TreeCacheEvent event = mock(TreeCacheEvent.class);
                ChildData childData = mock(ChildData.class);
                treeCacheListener.childEvent(curatorFramework, event);
                when(event.getData()).thenReturn(childData);
                when(childData.getPath()).thenReturn("");
                treeCacheListener.childEvent(curatorFramework, event);
            }
            final Properties configProps = config.getProps();
            configProps.setProperty("digest", "digest");
            repository.init(shenyuClientServerRegisterPublisher, config);
            repository.close();
        } catch (Exception e) {
            throw new ShenyuException(e);
        }
    }

    private static TreeCacheEvent treeCacheEvent(final String path) {
        TreeCacheEvent treeCacheEvent = mock(TreeCacheEvent.class);
        ChildData childData = mock(ChildData.class);
        when(treeCacheEvent.getData()).thenReturn(childData);
        when(childData.getPath()).thenReturn(path);
        when(childData.getData()).thenReturn("{}".getBytes());
        return treeCacheEvent;
    }
}
