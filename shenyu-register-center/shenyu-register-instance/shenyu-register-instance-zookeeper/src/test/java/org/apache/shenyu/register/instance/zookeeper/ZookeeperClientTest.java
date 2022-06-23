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
import org.apache.curator.framework.recipes.cache.TreeCache;
import org.apache.curator.test.TestingServer;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.zookeeper.CreateMode;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

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
}
