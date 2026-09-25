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

package org.apache.shenyu.sync.data.zookeeper;

import org.apache.curator.framework.CuratorFramework;
import org.apache.curator.framework.recipes.cache.ChildData;
import org.apache.curator.framework.recipes.cache.CuratorCacheListener;
import org.apache.curator.framework.recipes.cache.TreeCacheEvent;
import org.apache.curator.framework.recipes.cache.TreeCacheListener;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.constant.DefaultPathConstants;
import org.apache.shenyu.infra.zookeeper.client.ZookeeperClient;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.api.ProxySelectorDataSubscriber;
import org.apache.shenyu.sync.data.api.DiscoveryUpstreamDataSubscriber;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public final class ZookeeperSyncDataServiceTest {

    @Test
    public void testDiscoveryUpstreamDeleteUsesOldNodeWithoutPayload() {
        ZookeeperClient zkClient = mock(ZookeeperClient.class);
        Map<String, CuratorCacheListener> listeners = new HashMap<>();
        doAnswer(invocation -> {
            CuratorCacheListener registered = invocation.getArgument(1);
            listeners.put(invocation.getArgument(0), registered);
            return null;
        }).when(zkClient).addCuratorCache(any(), any(CuratorCacheListener[].class));
        ShenyuConfig config = mock(ShenyuConfig.class);
        when(config.getNamespace()).thenReturn(Constants.SYS_DEFAULT_NAMESPACE_ID);
        DiscoveryUpstreamDataSubscriber subscriber = mock(DiscoveryUpstreamDataSubscriber.class);
        new ZookeeperSyncDataService(config, zkClient, mock(PluginDataSubscriber.class), Collections.emptyList(),
                Collections.emptyList(), Collections.emptyList(), Collections.singletonList(subscriber));

        String namespacePath = Constants.PATH_SEPARATOR + Constants.SYS_DEFAULT_NAMESPACE_ID;
        String registerPath = namespacePath + DefaultPathConstants.DISCOVERY_UPSTREAM;
        ChildData oldData = mock(ChildData.class);
        when(oldData.getPath()).thenReturn(registerPath + "/divide/selector-id");
        listeners.get(registerPath).event(CuratorCacheListener.Type.NODE_DELETED, oldData, null);

        verify(subscriber).unSubscribe(argThat(key -> "divide".equals(key.pluginName())
                && "selector-id".equals(key.selectorId())));
        verify(subscriber, never()).onSubscribe(any());
    }

    @Test
    public void testZookeeperInstanceRegisterRepository() throws Exception {

        ZookeeperClient zkClient = mock(ZookeeperClient.class);
        PluginDataSubscriber pluginDataSubscriber = mock(PluginDataSubscriber.class);

        List<TreeCacheListener> treeCacheListeners = new ArrayList<>();
        doAnswer(invocationOnMock -> {
            treeCacheListeners.add(invocationOnMock.getArgument(1));
            return null;
        }).when(zkClient).addCache(any(), any());
        final AuthDataSubscriber authDataSubscriber = mock(AuthDataSubscriber.class);
        final MetaDataSubscriber metaDataSubscriber = mock(MetaDataSubscriber.class);
        final ProxySelectorDataSubscriber proxySelectorDataSubscriber = mock(ProxySelectorDataSubscriber.class);
        final DiscoveryUpstreamDataSubscriber discoveryUpstreamDataSubscriber = mock(DiscoveryUpstreamDataSubscriber.class);
        final ShenyuConfig shenyuConfig = mock(ShenyuConfig.class);
        when(shenyuConfig.getNamespace()).thenReturn(Constants.SYS_DEFAULT_NAMESPACE_ID);
        final ZookeeperSyncDataService zookeeperSyncDataService = new ZookeeperSyncDataService(shenyuConfig, zkClient,
                pluginDataSubscriber, Collections.singletonList(metaDataSubscriber), Collections.singletonList(authDataSubscriber),
                Collections.singletonList(proxySelectorDataSubscriber), Collections.singletonList(discoveryUpstreamDataSubscriber));

        List<TreeCacheEvent> treeCacheEvents = new ArrayList<>();
        String defaultNameSpaceId = Constants.PATH_SEPARATOR + Constants.SYS_DEFAULT_NAMESPACE_ID;
        // register uri
        treeCacheEvents.add(treeCacheEvent(defaultNameSpaceId + "/shenyu/uri/test/test/test", TreeCacheEvent.Type.NODE_REMOVED));
        // register metadata
        treeCacheEvents.add(treeCacheEvent(defaultNameSpaceId + "/shenyu/metaData/test", TreeCacheEvent.Type.NODE_REMOVED));
        // register auth
        treeCacheEvents.add(treeCacheEvent(defaultNameSpaceId + "/shenyu/auth/test", TreeCacheEvent.Type.NODE_REMOVED));
        // register rule
        treeCacheEvents.add(treeCacheEvent(defaultNameSpaceId + "/shenyu/rule/test/test-1", TreeCacheEvent.Type.NODE_REMOVED));
        // register plugin
        treeCacheEvents.add(treeCacheEvent(defaultNameSpaceId + "/shenyu/plugin/test", TreeCacheEvent.Type.NODE_REMOVED));
        // register selector
        treeCacheEvents.add(treeCacheEvent(defaultNameSpaceId + "/shenyu/selector/test/test", TreeCacheEvent.Type.NODE_REMOVED));

        final CuratorFramework curatorFramework = mock(CuratorFramework.class);
        for (TreeCacheListener treeCacheListener : treeCacheListeners) {
            for (TreeCacheEvent event : treeCacheEvents) {
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

        zookeeperSyncDataService.close();
    }

    private static TreeCacheEvent treeCacheEvent(final String path, final TreeCacheEvent.Type type) {
        TreeCacheEvent treeCacheEvent = mock(TreeCacheEvent.class);
        ChildData childData = mock(ChildData.class);
        when(treeCacheEvent.getData()).thenReturn(childData);
        when(treeCacheEvent.getType()).thenReturn(type);
        when(childData.getPath()).thenReturn(path);
        when(childData.getData()).thenReturn("{}".getBytes());
        return treeCacheEvent;
    }

}
