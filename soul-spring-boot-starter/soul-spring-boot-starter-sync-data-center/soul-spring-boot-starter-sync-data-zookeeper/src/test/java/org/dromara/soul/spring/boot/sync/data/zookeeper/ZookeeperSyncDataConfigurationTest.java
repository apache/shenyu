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

package org.dromara.soul.spring.boot.sync.data.zookeeper;

import org.I0Itec.zkclient.ZkClient;
import org.apache.curator.test.TestingServer;
import org.dromara.soul.sync.data.api.PluginDataSubscriber;
import org.dromara.soul.sync.data.api.SyncDataService;
import org.dromara.soul.sync.data.zookeeper.ZookeeperSyncDataService;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit4.SpringRunner;

import java.io.IOException;

/**
 * test case for {@link ZookeeperSyncDataConfiguration}.
 *
 * @author David Liu
 */
@RunWith(SpringRunner.class)
@SpringBootTest(
        classes = {
                ZookeeperSyncDataConfiguration.class
        },
        webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT,
        properties = {
                "soul.sync.zookeeper.url=localhost:2181",
                "soul.sync.zookeeper.sessionTimeout=30000",
                "soul.sync.zookeeper.connectionTimeout=30000"
        })
@EnableAutoConfiguration
@MockBean(PluginDataSubscriber.class)
public class ZookeeperSyncDataConfigurationTest {

    private static TestingServer mockZkServer;

    @Autowired
    private ZookeeperConfig zookeeperConfig;

    @Autowired
    private SyncDataService syncDataService;

    @Autowired
    private ZkClient zkClient;

    @BeforeClass
    public static void setUp() throws Exception {
        mockZkServer = new TestingServer(2181, true);
    }

    @AfterClass
    public static void tearDown() throws IOException {
        mockZkServer.stop();
    }

    /**
     * case to test {@link ZookeeperSyncDataConfiguration} to register bean {@link ZookeeperSyncDataService}.
     */
    @Test
    public void testZookeeperSyncDataConfigurationRegisterBeanSyncDataService() {
        Assert.assertNotNull(syncDataService);
        Assert.assertTrue(syncDataService instanceof ZookeeperSyncDataService);
    }

    /**
     * case to test {@link ZookeeperSyncDataConfiguration} to register bean {@link ZkClient}.
     */
    @Test
    public void testZookeeperSyncDataConfigurationRegisterBeanZkClient() {
        Assert.assertNotNull(zkClient);
    }

    /**
     * case to test {@link ZookeeperSyncDataConfiguration} to register bean {@link ZookeeperConfig}.
     */
    @Test
    public void testZookeeperSyncDataConfigurationRegisterBeanZookeeperConfig() {
        Assert.assertEquals("localhost:2181", zookeeperConfig.getUrl());
        Assert.assertEquals(30000, (int) zookeeperConfig.getSessionTimeout());
        Assert.assertEquals(30000, (int) zookeeperConfig.getConnectionTimeout());
    }
}
