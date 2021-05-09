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

import org.I0Itec.zkclient.ZkClient;
import org.apache.curator.test.TestingServer;
import org.apache.shenyu.admin.AbstractConfigurationTest;
import org.apache.shenyu.admin.config.properties.ZookeeperProperties;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

/**
 * Test case for ZookeeperConfiguration.
 */
public final class ZookeeperConfigurationTest extends AbstractConfigurationTest {

    private static TestingServer zkServer;

    private final String[] inlinedProperties = new String[]{
        "shenyu.sync.zookeeper.url=127.0.0.1:21810",
        "shenyu.sync.zookeeper.sessionTimeout=5000",
        "shenyu.sync.zookeeper.connectionTimeout=2000",
        "shenyu.sync.zookeeper.serializer=org.I0Itec.zkclient.serialize.SerializableSerializer",
    };

    @BeforeClass
    public static void setUpBefore() throws Exception {
        zkServer = new TestingServer(21810, true);
    }

    @Test
    public void testOnMissingBean() {
        // init zkClient by ZookeeperConfiguration
        load(ZookeeperConfiguration.class, inlinedProperties);
        ZkClient zkClient = (ZkClient) getContext().getBean("zkClient");
        assertNotNull(zkClient);
    }

    @Test
    public void testOnExistBean() {
        // verify zkClient by ZookeeperConfiguration
        load(CustomZkClientConfiguration.class, inlinedProperties);
        boolean isExistZkClient = getContext().containsBean("zkClient");
        assertFalse(isExistZkClient);

        // get customZkClient
        ZkClient customZkClient = (ZkClient) getContext().getBean("customZkClient");
        assertNotNull(customZkClient);
    }

    @AfterClass
    public static void tearDown() throws Exception {
        zkServer.stop();
    }

    @EnableConfigurationProperties(ZookeeperProperties.class)
    static class CustomZkClientConfiguration {

        @Bean
        public ZkClient customZkClient(final ZookeeperProperties zookeeperProp) {
            return new ZkClient(zookeeperProp.getUrl(), zookeeperProp.getSessionTimeout(), zookeeperProp.getConnectionTimeout());
        }
    }
}
