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

package org.apache.shenyu.infra.zookeeper.client;

import org.apache.curator.test.TestingServer;
import org.apache.shenyu.infra.zookeeper.config.ZookeeperConfig;
import org.apache.zookeeper.CreateMode;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for ZookeeperClient.
 */
class ZookeeperClientTest {

    private static TestingServer testingServer;

    private static ZookeeperClient client;

    @BeforeAll
    static void setUp() throws Exception {
        testingServer = new TestingServer(2181, true);
        ZookeeperConfig config = ZookeeperConfig.builder()
                .url("127.0.0.1:2181")
                .namespace("test")
                .baseSleepTimeMilliseconds(1000)
                .maxRetries(3)
                .sessionTimeoutMilliseconds(60000)
                .connectionTimeoutMilliseconds(15000)
                .maxSleepTimeMilliseconds(1000)
                .build();
        client = ZookeeperClient.builder().config(config).build();
        client.start();
    }

    @AfterAll
    static void tearDown() throws Exception {
        if (Objects.nonNull(client)) {
            client.close();
        }
        if (Objects.nonNull(testingServer)) {
            testingServer.close();
        }
    }

    @Test
    void testCreateAndGet() {
        String key = "/test/key1";
        String value = "value1";
        client.createOrUpdate(key, value, CreateMode.PERSISTENT);
        assertTrue(client.isExist(key));
        assertEquals(value, client.get(key));
    }

    @Test
    void testUpdate() {
        String key = "/test/key2";
        String value1 = "value1";
        String value2 = "value2";
        client.createOrUpdate(key, value1, CreateMode.PERSISTENT);
        assertEquals(value1, client.get(key));
        client.createOrUpdate(key, value2, CreateMode.PERSISTENT);
        assertEquals(value2, client.get(key));
    }

    @Test
    void testDelete() {
        String key = "/test/key3";
        String value = "value3";
        client.createOrUpdate(key, value, CreateMode.PERSISTENT);
        assertTrue(client.isExist(key));
        client.delete(key);
        assertFalse(client.isExist(key));
    }

    @Test
    void testGetChildren() {
        String parent = "/test/parent";
        client.createOrUpdate(parent + "/child1", "value1", CreateMode.PERSISTENT);
        client.createOrUpdate(parent + "/child2", "value2", CreateMode.PERSISTENT);
        List<String> children = client.getChildren(parent);
        assertNotNull(children);
        assertEquals(2, children.size());
        assertTrue(children.contains("child1"));
        assertTrue(children.contains("child2"));
    }
}
