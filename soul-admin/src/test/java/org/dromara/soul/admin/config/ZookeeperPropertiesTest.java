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

import org.junit.Test;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Configuration;
import org.springframework.test.context.support.TestPropertySourceUtils;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

/**
 * Test case for ZookeeperProperties.
 *
 * @author fengzhenbing
 */
public final class ZookeeperPropertiesTest {

    private static final String URL = "127.0.0.1:2181";

    private static final Integer SESSION_TIME_OUT = 5000;

    private static final Integer CONNECTION_TIMEOUT = 2000;

    private static final String SERIALIZER = "org.I0Itec.zkclient.serialize.SerializableSerializer";

    private final AnnotationConfigApplicationContext context = new AnnotationConfigApplicationContext();

    @Test
    public void testZookeeperProperties() {
        ZookeeperProperties properties = new ZookeeperProperties();
        properties.setUrl(URL);
        properties.setSessionTimeout(SESSION_TIME_OUT);
        properties.setConnectionTimeout(CONNECTION_TIMEOUT);
        properties.setSerializer(SERIALIZER);
        assertThat(properties.getUrl(), is(URL));
        assertThat(properties.getSessionTimeout(), is(SESSION_TIME_OUT));
        assertThat(properties.getConnectionTimeout(), is(CONNECTION_TIMEOUT));
        assertThat(properties.getSerializer(), is(SERIALIZER));
    }

    @Test
    public void testLoadPropertiesBySpringContext() {
        final String[] inlinedProperties = new String[]{
            "soul.sync.zookeeper.url=127.0.0.1:2181",
            "soul.sync.zookeeper.sessionTimeout=5000",
            "soul.sync.zookeeper.connectionTimeout=2000",
            "soul.sync.zookeeper.serializer=org.I0Itec.zkclient.serialize.SerializableSerializer",
        };
        load(ZookeeperPropertiesConfiguration.class, inlinedProperties);
        ZookeeperProperties properties = this.context.getBean(ZookeeperProperties.class);
        assertThat(properties.getUrl(), is(URL));
        assertThat(properties.getSessionTimeout(), is(SESSION_TIME_OUT));
        assertThat(properties.getConnectionTimeout(), is(CONNECTION_TIMEOUT));
        assertThat(properties.getSerializer(), is(SERIALIZER));
        this.context.close();
    }

    private void load(final Class<?> configuration, final String... inlinedProperties) {
        load(new Class<?>[]{configuration}, inlinedProperties);
    }

    private void load(final Class<?>[] configuration, final String... inlinedProperties) {
        TestPropertySourceUtils.addInlinedPropertiesToEnvironment(this.context, inlinedProperties);
        this.context.register(configuration);
        this.context.refresh();
    }

    @Configuration
    @EnableConfigurationProperties(ZookeeperProperties.class)
    static class ZookeeperPropertiesConfiguration {
    }
}
