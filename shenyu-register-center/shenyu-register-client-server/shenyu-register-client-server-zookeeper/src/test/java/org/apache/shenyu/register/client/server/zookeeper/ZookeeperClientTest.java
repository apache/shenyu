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
import org.apache.curator.framework.recipes.cache.TreeCache;
import org.apache.curator.framework.recipes.cache.TreeCacheEvent;
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

import java.io.IOException;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;

class ZookeeperClientTest {

    private ZookeeperClient client;

    private TestingServer server;

    @BeforeEach
    public void setup() throws Exception {
        this.server = new TestingServer();
        ZookeeperConfig config = new ZookeeperConfig(server.getConnectString());
        config.setNamespace("namespace");
        config.setDigest("digest");
        client = new ZookeeperClient(config);
        client.start();
    }

    @AfterEach
    public void cleanup() throws IOException {
        client.close();
        this.server.close();
    }

    @Test
    void getClient() {
        CuratorFramework curatorFramework = client.getClient();
        assertNotNull(curatorFramework);
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
    void errorTest() throws NoSuchFieldException, InterruptedException, IllegalAccessException {
        final Field curatorFramework = ZookeeperClient.class.getDeclaredField("client");
        final CuratorFramework curatorFrameworkMock = mock(CuratorFramework.class);
        curatorFramework.setAccessible(true);
        curatorFramework.set(client, curatorFrameworkMock);
        doThrow(InterruptedException.class).when(curatorFrameworkMock).blockUntilConnected();
        Assertions.assertDoesNotThrow(() -> client.start());

        doThrow(ShenyuException.class).when(curatorFrameworkMock).checkExists();
        Assertions.assertThrows(ShenyuException.class, () -> client.isExist("key"));

        doThrow(ShenyuException.class).when(curatorFrameworkMock).getData();
        Assertions.assertThrows(ShenyuException.class, () -> client.getDirectly("key"));

        doThrow(ShenyuException.class).when(curatorFrameworkMock).create();
        Assertions.assertThrows(ShenyuException.class, () -> client.createOrUpdate("key", "value", CreateMode.PERSISTENT));

        doThrow(ShenyuException.class).when(curatorFrameworkMock).delete();
        Assertions.assertThrows(ShenyuException.class, () -> client.delete("key"));

        doThrow(ShenyuException.class).when(curatorFrameworkMock).getChildren();
        Assertions.assertThrows(ShenyuException.class, () -> client.getChildren("key"));
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
    void addCache() throws InterruptedException {
        List<String> paths = new ArrayList<>();
        TreeCacheListener listener = (client, event) -> {
            if (event.getType() == TreeCacheEvent.Type.NODE_ADDED || event.getType() == TreeCacheEvent.Type.NODE_UPDATED) {
                paths.add(event.getData().getPath());
            }
        };
        client.createOrUpdate("/test", "", CreateMode.PERSISTENT);
        client.addCache("/test", listener);
        TreeCache cache = client.getCache("/test");

        Thread.sleep(500);
        assertNotNull(cache);
        assertEquals("/test", paths.get(0));
    }
}
