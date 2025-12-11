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

package org.apache.shenyu.infra.etcd.properties;

import org.apache.shenyu.infra.etcd.autoconfig.EtcdProperties;
import org.apache.shenyu.infra.etcd.config.EtcdConfig;
import org.junit.jupiter.api.Test;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Configuration;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Test case for ZookeeperProperties.
 */
public final class EtcdPropertiesTest extends AbstractConfigurationTest {
    
    @Test
    public void testLoadPropertiesBySpringContext() {
        final String url = "127.0.0.1:2181";
        final Integer sessionTimeOut = 100;
        final Integer connectionTimeout = 20560;
        final String[] inlinedProperties = new String[]{
            "shenyu.sync.etcd.url=" + url,
            "shenyu.sync.etcd.sessionTimeout=" + sessionTimeOut,
            "shenyu.sync.etcd.connectionTimeout=" + connectionTimeout,
        };
        load(EtcdPropertiesConfiguration.class, inlinedProperties);
        EtcdProperties properties = getContext().getBean(EtcdProperties.class);
        assertThat(properties.getEtcd().getUrl(), is(url));
        assertThat(properties.getEtcd().getSessionTimeout(), is(sessionTimeOut));
        assertThat(properties.getEtcd().getConnectionTimeout(), is(connectionTimeout));
    }

    @Test
    public void testSetEtcdConfig() {
        EtcdProperties etcdProperties = new EtcdProperties();
        EtcdConfig customConfig = EtcdConfig.builder()
                .connectionTimeout(60 * 1000)
                .sessionTimeout(60 * 1000)
                .build();

        etcdProperties.setEtcd(customConfig);
        EtcdConfig etcdConfig = etcdProperties.getEtcd();

        assertNotNull(etcdConfig, "EtcdConfig should not be null after setting");
        assertEquals(60 * 1000, etcdConfig.getConnectionTimeout(), "Connection timeout should be updated to 60 seconds");
        assertEquals(60 * 1000, etcdConfig.getSessionTimeout(), "Session timeout(TTL) should be updated to 60 seconds");
    }

    @Configuration
    @EnableConfigurationProperties(EtcdProperties.class)
    static class EtcdPropertiesConfiguration {
    }

}
