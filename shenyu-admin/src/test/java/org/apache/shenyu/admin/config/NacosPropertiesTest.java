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

import org.apache.shenyu.admin.AbstractConfigurationTest;

import org.apache.shenyu.admin.config.properties.NacosProperties;
import org.junit.jupiter.api.Test;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Configuration;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * Test cases for NacosProperties.
 */
public final class NacosPropertiesTest extends AbstractConfigurationTest {

    @Test
    public void testNacosPropertiesDefault() {
        load(NacosPropertiesTest.NacosPropertiesConfiguration.class);
        NacosProperties nacosProperties = getContext().getBean(NacosProperties.class);
        nacosProperties.setPassword("password");
        nacosProperties.setUsername("username");
        assertNull(nacosProperties.getUrl());
        assertNull(nacosProperties.getNamespace());
        assertNull(nacosProperties.getAcm());
        assertEquals(nacosProperties.getPassword(), "password");
        assertEquals(nacosProperties.getUsername(), "username");
    }

    @Test
    public void testNacosPropertiesSpecified() {
        final String url = "localhost:8848";
        final String namespace = "1c10d748-af86-43b9-8265-75f487d20c6c";
        NacosProperties.NacosACMProperties acm = new NacosProperties.NacosACMProperties();
        acm.setEnabled(false);
        acm.setEndpoint("acm.aliyun.com");
        acm.setAccessKey("accessKey");
        acm.setNamespace("namespace");
        acm.setSecretKey("secretKey");
        assertEquals(acm.getAccessKey(), "accessKey");
        assertEquals(acm.getNamespace(), "namespace");
        assertEquals(acm.getSecretKey(), "secretKey");
        load(NacosPropertiesTest.NacosPropertiesConfiguration.class, "shenyu.sync.nacos.url=localhost:8848",
                "shenyu.sync.nacos.namespace=1c10d748-af86-43b9-8265-75f487d20c6c",
                "shenyu.sync.nacos.acm.enabled=false",
                "shenyu.sync.nacos.acm.endpoint=acm.aliyun.com");
        NacosProperties nacosProperties = getContext().getBean(NacosProperties.class);
        assertEquals(nacosProperties.getUrl(), url);
        assertEquals(nacosProperties.getNamespace(), namespace);
        assertEquals(nacosProperties.getAcm().isEnabled(), acm.isEnabled());
        assertEquals(nacosProperties.getAcm().getEndpoint(), acm.getEndpoint());
    }

    @Configuration
    @EnableConfigurationProperties(NacosProperties.class)
    static class NacosPropertiesConfiguration {
    }
}
