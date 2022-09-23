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
import org.apache.curator.framework.CuratorFrameworkFactory;
import org.apache.curator.framework.api.ACLBackgroundPathAndBytesable;
import org.apache.curator.framework.api.BackgroundVersionable;
import org.apache.curator.framework.api.ChildrenDeletable;
import org.apache.curator.framework.api.CreateBuilder;
import org.apache.curator.framework.api.CreateBuilder2;
import org.apache.curator.framework.api.DeleteBuilder;
import org.apache.curator.framework.api.GetChildrenBuilder;
import org.apache.curator.framework.api.GetDataBuilder;
import org.apache.curator.framework.api.ProtectACLCreateModeStatPathAndBytesable;
import org.apache.curator.framework.imps.ExistsBuilderImpl;
import org.apache.curator.framework.recipes.cache.TreeCache;
import org.apache.curator.framework.recipes.cache.TreeCacheListener;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.zookeeper.CreateMode;
import org.apache.zookeeper.data.Stat;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import java.lang.reflect.Field;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

class ZookeeperClientTest {

    private ZookeeperClient client;

    private CuratorFramework curatorFramework;

    @BeforeEach
    public void setup() {
        curatorFramework = mock(CuratorFramework.class);
        try (MockedStatic<CuratorFrameworkFactory> frameworkFactoryMockedStatic = mockStatic(CuratorFrameworkFactory.class)) {
            CuratorFrameworkFactory.Builder builder = mock(CuratorFrameworkFactory.Builder.class);
            frameworkFactoryMockedStatic.when(CuratorFrameworkFactory::builder).thenReturn(builder);
            when(builder.connectString(anyString())).thenReturn(builder);
            when(builder.retryPolicy(any())).thenReturn(builder);
            when(builder.connectionTimeoutMs(anyInt())).thenReturn(builder);
            when(builder.sessionTimeoutMs(anyInt())).thenReturn(builder);
            when(builder.namespace(anyString())).thenReturn(builder);
            when(builder.build()).thenReturn(curatorFramework);
            ZookeeperConfig config = new ZookeeperConfig("services");
            config.setNamespace("namespace");
            config.setDigest("digest");
            client = new ZookeeperClient(config);
            client.start();
            doThrow(InterruptedException.class).when(curatorFramework).blockUntilConnected();
            assertDoesNotThrow(() -> client.start());
        } catch (Exception e) {
            throw new ShenyuException(e);
        }
    }

    @AfterEach
    public void cleanup() {
        client.close();
    }

    @Test
    void getClient() {
        CuratorFramework curatorFramework = client.getClient();
        assertNotNull(curatorFramework);
    }

    @Test
    void isExist() throws Exception {
        assertThrows(ShenyuException.class, () -> client.isExist("/test"));
        ExistsBuilderImpl existsBuilder = mock(ExistsBuilderImpl.class);
        when(curatorFramework.checkExists()).thenReturn(existsBuilder);
        when(existsBuilder.forPath(anyString())).thenReturn(new Stat());
        boolean exist = client.isExist("/test");
        assertTrue(exist);
    }

    @Test
    void getDirectly() throws Exception {
        assertThrows(ShenyuException.class, () -> client.getDirectly("/test"));
        GetDataBuilder getDataBuilder = mock(GetDataBuilder.class);
        when(curatorFramework.getData()).thenReturn(getDataBuilder);
        when(getDataBuilder.forPath(anyString())).thenReturn("hello".getBytes());
        String val = client.getDirectly("/test");
        assertEquals("hello", val);
        when(getDataBuilder.forPath(anyString())).thenReturn(null);
        String val2 = client.getDirectly("/test");
        assertNull(val2);
    }

    @Test
    void delete() throws Exception {
        assertThrows(ShenyuException.class, () -> client.delete("/test"));
        DeleteBuilder deleteBuilder = mock(DeleteBuilder.class);
        when(curatorFramework.delete()).thenReturn(deleteBuilder);
        ChildrenDeletable childrenDeletable = mock(ChildrenDeletable.class);
        when(deleteBuilder.guaranteed()).thenReturn(childrenDeletable);
        BackgroundVersionable backgroundVersionable = mock(BackgroundVersionable.class);
        when(childrenDeletable.deletingChildrenIfNeeded()).thenReturn(backgroundVersionable);
        doNothing().when(backgroundVersionable).forPath(anyString());
        assertDoesNotThrow(() -> client.delete("/test"));
    }

    @Test
    void getChildren() throws Exception {
        assertThrows(ShenyuException.class, () -> client.getChildren("/test"));
        GetChildrenBuilder getChildrenBuilder = mock(GetChildrenBuilder.class);
        when(curatorFramework.getChildren()).thenReturn(getChildrenBuilder);
        when(getChildrenBuilder.forPath(anyString())).thenReturn(new ArrayList<>());
        List<String> children = client.getChildren("/test");
        assertEquals(0, children.size());
    }

    @Test
    void createOrUpdate() throws Exception {
        assertThrows(ShenyuException.class, () ->
                client.createOrUpdate("/test", "hello", CreateMode.PERSISTENT));
        CreateBuilder createBuilder = mock(CreateBuilder.class);
        when(curatorFramework.create()).thenReturn(createBuilder);
        CreateBuilder2 createBuilder2 = mock(CreateBuilder2.class);
        when(createBuilder.orSetData()).thenReturn(createBuilder2);
        ProtectACLCreateModeStatPathAndBytesable protectACLCreateModeStatPathAndBytesable = mock(ProtectACLCreateModeStatPathAndBytesable.class);
        when(createBuilder2.creatingParentsIfNeeded()).thenReturn(protectACLCreateModeStatPathAndBytesable);
        ACLBackgroundPathAndBytesable pathAndBytesable = mock(ACLBackgroundPathAndBytesable.class);
        when(protectACLCreateModeStatPathAndBytesable.withMode(any())).thenReturn(pathAndBytesable);
        when(pathAndBytesable.forPath(anyString(), any(byte[].class))).thenReturn(null);
        client.createOrUpdate("/test", "hello", CreateMode.PERSISTENT);
        client.createOrUpdate("", "hello", CreateMode.PERSISTENT);
        client.createOrUpdate("", (Object) null, CreateMode.PERSISTENT);
        client.createOrUpdate("", new Object(), CreateMode.PERSISTENT);
    }

    @Test
    void cacheTest() throws Exception {
        assertThrows(ShenyuException.class, () -> client.addCache("/path", mock(TreeCacheListener.class), mock(TreeCacheListener.class)));
        Field clientField = ZookeeperClient.class.getDeclaredField("client");
        clientField.setAccessible(true);
        CuratorFramework curatorFramework = mock(CuratorFramework.class);
        clientField.set(client, curatorFramework);

        GetDataBuilder getDataBuilder = mock(GetDataBuilder.class);
        when(curatorFramework.getData()).thenReturn(getDataBuilder);
        when(getDataBuilder.forPath(any())).thenReturn("path".getBytes(StandardCharsets.UTF_8));
        client.get("/path");
        client.get("/test");
        client.getCache("/test");
        MockedStatic<TreeCache> treeCacheMockedStatic = mockStatic(TreeCache.class);
        TreeCache.Builder treeCacheBuilder = mock(TreeCache.Builder.class);
        treeCacheMockedStatic.when(() -> TreeCache.newBuilder(any(), any())).thenReturn(treeCacheBuilder);
        TreeCache treeCache = mock(TreeCache.class);
        when(treeCacheBuilder.build()).thenReturn(treeCache);
        when(treeCache.start()).thenThrow(ShenyuException.class);
        Assertions.assertThrows(ShenyuException.class, () -> client.addCache("/path"));
        treeCacheMockedStatic.close();
    }
}
