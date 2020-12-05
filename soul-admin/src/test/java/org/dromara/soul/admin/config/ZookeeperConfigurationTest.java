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
import org.dromara.soul.admin.AbstractSpringIntegrationTest;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.jupiter.api.AfterEach;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.test.context.support.TestPropertySourceUtils;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;

/**
 * Test case for ZookeeperConfiguration.
 *
 * @author fengzhenbing
 */
public final class ZookeeperConfigurationTest extends AbstractSpringIntegrationTest {

    private static TestingServer zkServer;

    private final AnnotationConfigApplicationContext context = new AnnotationConfigApplicationContext();

    private final String[] inlinedProperties = new String[]{
        "soul.sync.zookeeper.url=127.0.0.1:21810",
        "soul.sync.zookeeper.sessionTimeout=5000",
        "soul.sync.zookeeper.connectionTimeout=2000",
        "soul.sync.zookeeper.serializer=org.I0Itec.zkclient.serialize.SerializableSerializer",
    };

    @BeforeClass
    public static void setUpBefore() throws Exception {
        zkServer = new TestingServer(21810, true);
    }

    @AfterEach
    void cleanup() {
        this.context.close();
    }

    @Test
    public void testOnMissingBean() {
        // init zkClient by ZookeeperConfiguration
        load(ZookeeperConfiguration.class, inlinedProperties);
        ZkClient zkClient = (ZkClient) this.context.getBean("zkClient");
        assertNotNull(zkClient);

        // test operation by zkClient
        testZkClient(zkClient);
    }

    @Test
    public void testOnExistBean() {
        // verify zkClient by ZookeeperConfiguration
        load(CustomZkClientConfiguration.class, inlinedProperties);
        Boolean isExistZkClient = this.context.containsBean("zkClient");
        assertFalse(isExistZkClient);

        // get customZkClient
        ZkClient customZkClient = (ZkClient) this.context.getBean("customZkClient");
        assertNotNull(customZkClient);

        // test operation by customZkClient
        testZkClient(customZkClient);
    }

    @AfterClass
    public static void tearDown() throws Exception {
        zkServer.stop();
    }

    private void load(final Class<?> configuration, final String... inlinedProperties) {
        load(new Class<?>[]{configuration}, inlinedProperties);
    }

    private void load(final Class<?>[] configuration, final String... inlinedProperties) {
        TestPropertySourceUtils.addInlinedPropertiesToEnvironment(context, inlinedProperties);
        context.register(configuration);
        context.refresh();
    }

    private void testZkClient(final ZkClient zkClient) {
        // zkClient create path
        final String zkPath = "/test";
        final String zkPathData = "testData";
        zkClient.createEphemeral(zkPath, zkPathData);
        assertThat(zkClient.readData(zkPath), is(zkPathData));

        // zkClient delete path
        zkClient.delete(zkPath);
        assertFalse(zkClient.exists(zkPath));

        // close zkClient
        zkClient.close();
    }

    @Configuration
    @EnableConfigurationProperties(ZookeeperProperties.class)
    static class CustomZkClientConfiguration {

        @Bean
        @ConditionalOnMissingBean(ZkClient.class)
        public ZkClient customZkClient(final ZookeeperProperties zookeeperProp) {
            return new ZkClient(zookeeperProp.getUrl(), zookeeperProp.getSessionTimeout(), zookeeperProp.getConnectionTimeout());
        }
    }
}
