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

package org.apache.shenyu.infra.zookeeper.properties;

import org.apache.shenyu.infra.zookeeper.autoconfig.ZookeeperProperties;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Configuration;
import org.junit.jupiter.api.Test;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

/**
 * Test case for ZookeeperProperties.
 */
public final class ZookeeperPropertiesTest extends AbstractConfigurationTest {
    
    @Test
    public void testLoadPropertiesBySpringContext() {
        final String url = "127.0.0.1:2181";
        final Integer sessionTimeOut = 100;
        final Integer connectionTimeout = 20560;
        final String[] inlinedProperties = new String[]{
            "shenyu.sync.zookeeper.url=" + url,
            "shenyu.sync.zookeeper.sessionTimeoutMilliseconds=" + sessionTimeOut,
            "shenyu.sync.zookeeper.connectionTimeoutMilliseconds=" + connectionTimeout,
        };
        load(ZookeeperPropertiesConfiguration.class, inlinedProperties);
        ZookeeperProperties properties = getContext().getBean(ZookeeperProperties.class);
        assertThat(properties.getZookeeper().getUrl(), is(url));
        assertThat(properties.getZookeeper().getSessionTimeoutMilliseconds(), is(sessionTimeOut));
        assertThat(properties.getZookeeper().getConnectionTimeoutMilliseconds(), is(connectionTimeout));
    }
    
    @Configuration
    @EnableConfigurationProperties(ZookeeperProperties.class)
    static class ZookeeperPropertiesConfiguration {
    }
}
