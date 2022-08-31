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

package org.apache.shenyu.register.client.zookeeper;

import org.apache.curator.framework.CuratorFramework;
import org.apache.curator.framework.api.GetDataBuilder;
import org.apache.curator.framework.recipes.cache.ChildData;
import org.apache.curator.framework.recipes.cache.TreeCache;
import org.apache.curator.framework.recipes.cache.TreeCacheListener;
import org.apache.curator.test.TestingServer;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.zookeeper.CreateMode;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import java.io.IOException;
import java.lang.reflect.Field;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

class ZookeeperClientTest {

    private ZookeeperClient client;

    private TestingServer server;

    @BeforeEach
    public void setup() throws Exception {
        this.server = new TestingServer();
        ZookeeperConfig config = new ZookeeperConfig(server.getConnectString());
        client = new ZookeeperClient(config);
        client.start();
    }

    @AfterEach
    public void cleanup() throws IOException {
        if (this.client != null) {
            this.client.close();
        }
        if (this.server != null) {
            this.server.close();
        }
    }

    @Test
    void getClient() {
        CuratorFramework curatorFramework = client.getClient();
        assertNotNull(curatorFramework);
    }

    @Test
    void zookeeperClientTest() {
        ZookeeperConfig config = new ZookeeperConfig(server.getConnectString());
        config.setDigest("digest");
        config.setNamespace("namespace");
        client = new ZookeeperClient(config);

    }

    @Test
    void isExist() {
        boolean exist = client.isExist("/test");
        assertFalse(exist);

        client.createOrUpdate("/test", "", CreateMode.PERSISTENT);
        exist = client.isExist("/test");
        assertTrue(exist);
    }

    @Test
    void getDirectly() {
        client.createOrUpdate("/test", "hello", CreateMode.PERSISTENT);
        String val = client.getDirectly("/test");
        assertEquals("hello", val);
    }

    @Test
    void get() {
        client.createOrUpdate("/test", "hello", CreateMode.PERSISTENT);
        String val = client.get("/test");
        assertEquals("hello", val);
    }

    @Test
    void createOrUpdate() {
        client.createOrUpdate("/test", "hello", CreateMode.PERSISTENT);
        String val = client.get("/test");
        assertEquals("hello", val);
    }

    @Test
    void testCreateOrUpdate() {
        MetaData data = new MetaData();
        data.setAppName("test");
        client.createOrUpdate("/test", data, CreateMode.PERSISTENT);
        String val = client.get("/test");
        assertEquals(GsonUtils.getInstance().toJson(data), val);
    }

    @Test
    void delete() {
        client.createOrUpdate("/test", "hello", CreateMode.PERSISTENT);
        String val = client.get("/test");
        assertEquals("hello", val);

        client.delete("/test");
        boolean exist = client.isExist("/test");
        assertFalse(exist);
    }

    @Test
    void getChildren() {
        client.createOrUpdate("/test/1", "hello", CreateMode.PERSISTENT);
        client.createOrUpdate("/test/2", "hello", CreateMode.PERSISTENT);

        List<String> children = client.getChildren("/test");
        assertTrue(children.contains("1"));
        assertTrue(children.contains("2"));
        assertEquals(2, children.size());
    }

    @Test
    void getCache() {
        TreeCache cache = client.getCache("/test");
        assertNull(cache);

        client.addCache("/test");
        cache = client.getCache("/test");
        assertNotNull(cache);
    }

    @Test
    void addCache() {
        client.addCache("/test");
        TreeCache cache = client.getCache("/test");
        assertNotNull(cache);
    }

    @Test
    void startErrorTest() throws Exception {
        Field clientField = ZookeeperClient.class.getDeclaredField("client");
        clientField.setAccessible(true);
        CuratorFramework curatorFramework = mock(CuratorFramework.class);
        clientField.set(client, curatorFramework);

        doThrow(InterruptedException.class).when(curatorFramework).blockUntilConnected();
        client.start();

        doThrow(ShenyuException.class).when(curatorFramework).checkExists();
        Assertions.assertThrows(ShenyuException.class, () -> client.isExist("key"));

        GetDataBuilder getDataBuilder = mock(GetDataBuilder.class);
        when(curatorFramework.getData()).thenReturn(getDataBuilder);
        when(getDataBuilder.forPath(any())).thenReturn(null);
        Assertions.assertDoesNotThrow(() -> client.getDirectly("key"));

        doThrow(ShenyuException.class).when(curatorFramework).getData();
        Assertions.assertThrows(ShenyuException.class, () -> client.getDirectly("key"));

        doThrow(ShenyuException.class).when(curatorFramework).getChildren();
        Assertions.assertThrows(ShenyuException.class, () -> client.getChildren("key"));

        doThrow(ShenyuException.class).when(curatorFramework).delete();
        Assertions.assertThrows(ShenyuException.class, () -> client.delete("key"));

        doThrow(ShenyuException.class).when(curatorFramework).create();
        Assertions.assertThrows(ShenyuException.class, () -> client.createOrUpdate(null, null, null));
        curatorFramework.close();
    }

    @Test
    void cacheTest() throws Exception {
        client.addCache("/path", mock(TreeCacheListener.class), mock(TreeCacheListener.class));
        Field clientField = ZookeeperClient.class.getDeclaredField("client");
        clientField.setAccessible(true);
        CuratorFramework curatorFramework = mock(CuratorFramework.class);
        clientField.set(client, curatorFramework);

        GetDataBuilder getDataBuilder = mock(GetDataBuilder.class);
        when(curatorFramework.getData()).thenReturn(getDataBuilder);
        when(getDataBuilder.forPath(any())).thenReturn("path".getBytes(StandardCharsets.UTF_8));
        client.get("/path");
        client.get("/test");

        MockedStatic<TreeCache> treeCacheMockedStatic = mockStatic(TreeCache.class);
        TreeCache.Builder treeCacheBuilder = mock(TreeCache.Builder.class);
        treeCacheMockedStatic.when(() -> TreeCache.newBuilder(any(), any())).thenReturn(treeCacheBuilder);
        TreeCache treeCache = mock(TreeCache.class);
        when(treeCacheBuilder.build()).thenReturn(treeCache);
        when(treeCache.start()).thenThrow(ShenyuException.class);
        Assertions.assertThrows(ShenyuException.class, () -> client.addCache("/path"));
        treeCacheMockedStatic.close();
    }

    @Test
    void findFromCacheTest() throws Exception {
        Field clientField = ZookeeperClient.class.getDeclaredField("client");
        clientField.setAccessible(true);
        CuratorFramework curatorFramework = mock(CuratorFramework.class);
        clientField.set(client, curatorFramework);
        GetDataBuilder getDataBuilder = mock(GetDataBuilder.class);
        when(curatorFramework.getData()).thenReturn(getDataBuilder);
        when(getDataBuilder.forPath(any())).thenReturn("path".getBytes(StandardCharsets.UTF_8));
        Assertions.assertDoesNotThrow(() -> client.get("/path"));

        Field cachesField = ZookeeperClient.class.getDeclaredField("caches");
        cachesField.setAccessible(true);

        TreeCache treeCache = mock(TreeCache.class);
        Map<String, TreeCache> caches = new ConcurrentHashMap<>();
        caches.put("/path", treeCache);
        cachesField.set(client, caches);

        ChildData childData = mock(ChildData.class);
        when(treeCache.getCurrentData(any())).thenReturn(childData);
        Assertions.assertDoesNotThrow(() -> client.get("/path"));

        when(childData.getData()).thenReturn(new byte[1]);
        Assertions.assertDoesNotThrow(() -> client.get("/path"));
    }

    @Test
    void createOrUpdateTest() {
        Assertions.assertThrows(ShenyuException.class, () -> client.createOrUpdate("key", (Object) null, CreateMode.PERSISTENT));
        Assertions.assertThrows(ShenyuException.class, () -> client.createOrUpdate("key", new Object(), CreateMode.PERSISTENT));
    }
}
