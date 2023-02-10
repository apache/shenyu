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

package org.apache.shenyu.sync.data.nacos.config;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * add test case for {@link NacosConfig}.
 */
public final class NacosConfigTest {

    private static final String URL = "url";

    private static final String NAMESPACE = "nameSpace";

    private static final String PASSWORD = "password";

    private static final String USERNAME = "username";

    private static final NacosACMConfig ACM = new NacosACMConfig();

    private NacosConfig nacosConfig;

    private NacosConfig that;

    @BeforeEach
    public void setUp() {
        nacosConfig = new NacosConfig();
        nacosConfig.setUrl(URL);
        nacosConfig.setNamespace(NAMESPACE);
        nacosConfig.setPassword(PASSWORD);
        nacosConfig.setUsername(USERNAME);
        nacosConfig.setAcm(ACM);
        that = new NacosConfig();
        that.setUrl(URL);
        that.setNamespace(NAMESPACE);
        that.setPassword(PASSWORD);
        that.setUsername(USERNAME);
        that.setAcm(ACM);
    }

    @Test
    public void testGetterSetter() {
        assertEquals(URL, nacosConfig.getUrl());
        assertEquals(NAMESPACE, nacosConfig.getNamespace());
        assertEquals(PASSWORD, nacosConfig.getPassword());
        assertEquals(USERNAME, nacosConfig.getUsername());
        assertEquals(ACM, nacosConfig.getAcm());
    }

    @Test
    public void testEquals() {
        assertTrue(nacosConfig.equals(nacosConfig));
        assertTrue(nacosConfig.equals(that));
        assertFalse(nacosConfig.equals(null));
        assertFalse(nacosConfig.equals(new Object()));
    }

    @Test
    public void testHashCode() {
        assertEquals(Objects.hash(nacosConfig.getUrl(), nacosConfig.getNamespace(),
                        nacosConfig.getUsername(), nacosConfig.getPassword(), nacosConfig.getAcm()),
                nacosConfig.hashCode());
    }
}
