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

package org.apache.shenyu.springboot.sync.data.zookeeper;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.api.SyncDataService;
import org.apache.shenyu.sync.data.zookeeper.ZookeeperClient;
import org.apache.shenyu.sync.data.zookeeper.ZookeeperSyncDataService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringExtension;

/**
 * test case for {@link ZookeeperSyncDataConfiguration}.
 */
@ExtendWith(SpringExtension.class)
@SpringBootTest(
        classes = {
                ZookeeperSyncDataConfiguration.class
        },
        webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT,
        properties = {
                "shenyu.sync.zookeeper.url=localhost:2181",
                "shenyu.sync.zookeeper.sessionTimeout=30000",
                "shenyu.sync.zookeeper.connectionTimeout=500"
        })
@EnableAutoConfiguration
@MockBean({PluginDataSubscriber.class, ZookeeperClient.class})
public final class ZookeeperSyncDataConfigurationTest {

    @Autowired
    private ZookeeperProperties zookeeperConfig;

    @Autowired
    private SyncDataService syncDataService;

    /**
     * case to test {@link ZookeeperSyncDataConfiguration} to register bean {@link ZookeeperSyncDataService}.
     */
    @Test
    public void testZookeeperSyncDataConfigurationRegisterBeanSyncDataService() {
        assertNotNull(syncDataService);
        assertTrue(syncDataService instanceof ZookeeperSyncDataService);
    }

    /**
     * case to test {@link ZookeeperSyncDataConfiguration} to register bean {@link ZookeeperProperties}.
     */
    @Test
    public void testZookeeperSyncDataConfigurationRegisterBeanZookeeperConfig() {
        assertThat(zookeeperConfig.getUrl(), is("localhost:2181"));
        assertThat(zookeeperConfig.getSessionTimeout(), is(30000));
        assertThat(zookeeperConfig.getConnectionTimeout(), is(500));
    }
}
