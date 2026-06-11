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

package org.apache.shenyu.infra.nacos.config;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;

/**
 * add test case for {@link NacosConfig}.
 */
public final class NacosConfigTest {

    private static final String URL = "url";

    private static final String NAMESPACE = "nameSpace";

    private static final String PASSWORD = "password";

    private static final String USERNAME = "username";

    private static final String CONTEXT_PATH = "nacos";

    private static final NacosACMConfig ACM = NacosACMConfig.builder().build();

    private NacosConfig nacosConfig;

    private NacosConfig that;

    @BeforeEach
    public void setUp() {

        nacosConfig = NacosConfig.builder()
                .acm(ACM)
                .contextPath(CONTEXT_PATH)
                .password(PASSWORD)
                .username(USERNAME)
                .url(URL)
                .namespace(NAMESPACE)
                .build();

        that = NacosConfig.builder()
                .acm(ACM)
                .contextPath(CONTEXT_PATH)
                .password(PASSWORD)
                .username(USERNAME)
                .url(URL)
                .namespace(NAMESPACE)
                .build();
    }

    @Test
    public void testGetterSetter() {
        assertEquals(URL, nacosConfig.getUrl());
        assertEquals(NAMESPACE, nacosConfig.getNamespace());
        assertEquals(PASSWORD, nacosConfig.getPassword());
        assertEquals(USERNAME, nacosConfig.getUsername());
        assertEquals(CONTEXT_PATH, nacosConfig.getContextPath());
        assertEquals(ACM, nacosConfig.getAcm());
    }

    @Test
    public void testEquals() {
        assertEquals(nacosConfig, nacosConfig);
        assertEquals(nacosConfig, that);
        assertNotEquals(null, nacosConfig);
        assertNotEquals(nacosConfig, new Object());
    }

    @Test
    public void testHashCode() {
        assertEquals(Objects.hash(nacosConfig.getUrl(), nacosConfig.getNamespace(),
                        nacosConfig.getUsername(), nacosConfig.getPassword(), nacosConfig.getContextPath(), nacosConfig.getAcm()),
                nacosConfig.hashCode());
    }
}
