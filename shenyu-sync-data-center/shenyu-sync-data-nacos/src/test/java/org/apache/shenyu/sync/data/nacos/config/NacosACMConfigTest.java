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

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.Objects;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

/**
 * add test case for {@link NacosACMConfig}.
 */
public final class NacosACMConfigTest {

    private static final String ACCESS_KEY = "accessKey";

    private static final boolean ENABLE = false;

    private static final String ENDPOINT = "endpoint";

    private static final String NAMESPACE = "namespace";

    private static final String SECRE_KEY = "secreKey";

    private NacosACMConfig nacosACMConfig;

    private NacosACMConfig that;

    @Before
    public void setUp() {
        nacosACMConfig = new NacosACMConfig();
        nacosACMConfig.setAccessKey(ACCESS_KEY);
        nacosACMConfig.setEnabled(ENABLE);
        nacosACMConfig.setEndpoint(ENDPOINT);
        nacosACMConfig.setNamespace(NAMESPACE);
        nacosACMConfig.setSecretKey(SECRE_KEY);
        that = new NacosACMConfig();
        that.setAccessKey(ACCESS_KEY);
        that.setEnabled(ENABLE);
        that.setEndpoint(ENDPOINT);
        that.setNamespace(NAMESPACE);
        that.setSecretKey(SECRE_KEY);
    }

    @Test
    public void testGetterSetter() {
        NacosACMConfig nacosACMConfig = new NacosACMConfig();
        nacosACMConfig.setAccessKey(ACCESS_KEY);
        nacosACMConfig.setEnabled(ENABLE);
        nacosACMConfig.setEndpoint(ENDPOINT);
        nacosACMConfig.setNamespace(NAMESPACE);
        nacosACMConfig.setSecretKey(SECRE_KEY);
        Assert.assertEquals(ACCESS_KEY, nacosACMConfig.getAccessKey());
        Assert.assertEquals(ENABLE, nacosACMConfig.isEnabled());
        Assert.assertEquals(ENDPOINT, nacosACMConfig.getEndpoint());
        Assert.assertEquals(NAMESPACE, nacosACMConfig.getNamespace());
        Assert.assertEquals(SECRE_KEY, nacosACMConfig.getSecretKey());
    }

    @Test
    public void testEquals() {
        assertTrue(nacosACMConfig.equals(nacosACMConfig));
        assertTrue(nacosACMConfig.equals(that));
        assertFalse(nacosACMConfig.equals(null));
        assertFalse(nacosACMConfig.equals(new Object()));
    }

    @Test
    public void testHashCode() {
        assertEquals(Objects.hash(nacosACMConfig.isEnabled(), nacosACMConfig.getEndpoint(),
                        nacosACMConfig.getNamespace(), nacosACMConfig.getAccessKey(), nacosACMConfig.getSecretKey()),
                nacosACMConfig.hashCode());
    }
}

