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

    @BeforeEach
    public void setUp() {

        nacosACMConfig = NacosACMConfig.builder()
                .namespace(NAMESPACE)
                .endpoint(ENDPOINT)
                .secretKey(SECRE_KEY)
                .accessKey(ACCESS_KEY)
                .enabled(ENABLE)
                .build();

        that = NacosACMConfig.builder()
                .namespace(NAMESPACE)
                .endpoint(ENDPOINT)
                .secretKey(SECRE_KEY)
                .accessKey(ACCESS_KEY)
                .enabled(ENABLE)
                .build();
    }

    @Test
    public void testGetterSetter() {

        var nacosACMConfig = NacosACMConfig.builder()
                .namespace(NAMESPACE)
                .endpoint(ENDPOINT)
                .secretKey(SECRE_KEY)
                .accessKey(ACCESS_KEY)
                .enabled(ENABLE)
                .build();

        assertEquals(ACCESS_KEY, nacosACMConfig.getAccessKey());
        assertEquals(ENABLE, nacosACMConfig.isEnabled());
        assertEquals(ENDPOINT, nacosACMConfig.getEndpoint());
        assertEquals(NAMESPACE, nacosACMConfig.getNamespace());
        assertEquals(SECRE_KEY, nacosACMConfig.getSecretKey());
    }

    @Test
    public void testEquals() {
        assertEquals(nacosACMConfig, nacosACMConfig);
        assertEquals(nacosACMConfig, that);
        assertNotEquals(null, nacosACMConfig);
        assertNotEquals(nacosACMConfig, new Object());
    }

    @Test
    public void testHashCode() {
        assertEquals(Objects.hash(nacosACMConfig.isEnabled(), nacosACMConfig.getEndpoint(),
                        nacosACMConfig.getNamespace(), nacosACMConfig.getAccessKey(), nacosACMConfig.getSecretKey()),
                nacosACMConfig.hashCode());
    }
}

