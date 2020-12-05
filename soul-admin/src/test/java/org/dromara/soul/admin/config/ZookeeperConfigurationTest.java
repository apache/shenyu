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

package org.dromara.soul.admin.config;

import org.I0Itec.zkclient.ZkClient;
import org.apache.curator.test.TestingServer;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertFalse;

/**
 * Test case for ZookeeperConfiguration.
 *
 * @author fengzhenbing
 */
public final class ZookeeperConfigurationTest {

    private static TestingServer zkServer;

    private ZkClient zkClient;

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        zkServer = new TestingServer(21810, true);
    }

    @Test
    public void testGetAndUseZkClient() {
        // init zkClient by ZookeeperConfiguration
        ZookeeperProperties properties = new ZookeeperProperties();
        properties.setUrl("127.0.0.1:21810");
        properties.setSessionTimeout(5000);
        properties.setConnectionTimeout(2000);
        ZookeeperConfiguration zookeeperConfiguration = new ZookeeperConfiguration();
        zkClient = zookeeperConfiguration.zkClient(properties);
        assertNotNull(zkClient);

        // zkClient create path
        final String zkPath = "/test";
        final String zkPathData = "testData";
        zkClient.createEphemeral(zkPath, zkPathData);
        assertThat(zkClient.readData(zkPath), is(zkPathData));

        // zkClient delete path
        zkClient.delete(zkPath);
        assertFalse(zkClient.exists(zkPath));
    }

    @After
    public void after() {
        zkClient.close();
    }

    @AfterClass
    public static void tearDown() throws Exception {
        zkServer.stop();
    }
}
