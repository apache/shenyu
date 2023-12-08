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

package org.apache.shenyu.admin.config;

import org.apache.curator.test.TestingServer;
import org.apache.shenyu.admin.config.properties.ZookeeperConfig;
import org.apache.shenyu.admin.listener.zookeeper.ZookeeperClient;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertNotNull;

public class ZookeeperSyncConfigurationTest {
    
    private static TestingServer zkServer;
    
    private static ZookeeperClient zkClient;
    
    @BeforeAll
    public static void setUpBeforeClass() throws Exception {
        zkServer = new TestingServer();
        ZookeeperConfig config = new ZookeeperConfig(zkServer.getConnectString());
        zkClient = new ZookeeperClient(config);
    }
    
    @AfterAll
    public static void tearDown() throws Exception {
        zkClient.close();
        zkServer.stop();
    }
    
    @Test
    public void testZookeeperDataChangedListener() {
        ZookeeperSyncConfiguration zookeeperListener = new ZookeeperSyncConfiguration();
        assertNotNull(zookeeperListener.zookeeperDataChangedListener(zkClient));
    }
    
    @Test
    public void testZookeeperDataInit() {
        ZookeeperSyncConfiguration zookeeperListener = new ZookeeperSyncConfiguration();
        assertNotNull(zookeeperListener.zookeeperDataChangedInit(zkClient));
    }
    
    @AfterEach
    public void after() {
        zkClient.close();
    }

}
