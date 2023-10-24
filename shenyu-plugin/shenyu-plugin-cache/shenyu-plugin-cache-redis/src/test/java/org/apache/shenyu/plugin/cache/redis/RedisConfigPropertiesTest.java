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

package org.apache.shenyu.plugin.cache.redis;

import org.apache.shenyu.common.enums.RedisModeEnum;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * RedisConfigProperties default value test.
 */
@ExtendWith(MockitoExtension.class)
public final class RedisConfigPropertiesTest {

    private static final int DEFAULT_MAX_IDLE = 8;

    private static final int DEFAULT_MAX_ACTIVE = 8;

    private static final int DEFAULT_MIN_IDLE = 0;

    private RedisConfigProperties redisConfigProperties;

    @BeforeEach
    public void setUp() {
        this.redisConfigProperties = new RedisConfigProperties();
    }

    /**
     * mode default value test.
     */
    @Test
    public void modeDefaultValueTest() {
        assertEquals(RedisModeEnum.STANDALONE.getName(), redisConfigProperties.getMode());
    }

    /**
     * maxIdle default value test.
     */
    @Test
    public void maxIdleDefaultValueTest() {
        assertEquals(DEFAULT_MAX_IDLE, redisConfigProperties.getMaxIdle());
    }

    /**
     * maxActive default value test.
     */
    @Test
    public void maxActiveDefaultValueTest() {
        assertEquals(DEFAULT_MAX_ACTIVE, redisConfigProperties.getMaxActive());
    }

    /**
     * minIdle default value test.
     */
    @Test
    public void minIdleDefaultValueTest() {
        assertEquals(DEFAULT_MIN_IDLE, redisConfigProperties.getMinIdle());
    }

    /**
     * equals test.
     */
    @Test
    public void equalsTest() {
        RedisConfigProperties defaultConfig = new RedisConfigProperties();
        assertEquals(defaultConfig, this.redisConfigProperties);
        defaultConfig.setMaster("master");
        defaultConfig.setDatabase(2);
        defaultConfig.setPassword("password");
        defaultConfig.setMaxIdle(30);
        defaultConfig.setMinIdle(10);
        defaultConfig.setMaxActive(100);
        defaultConfig.setMaxWait(-1);
        defaultConfig.setUrl("url");
        defaultConfig.setMode("mode");
        RedisConfigProperties defaultConfig2 = new RedisConfigProperties();
        defaultConfig2.setMaster("master");
        defaultConfig2.setDatabase(2);
        defaultConfig2.setPassword("password");
        defaultConfig2.setMaxIdle(30);
        defaultConfig2.setMinIdle(10);
        defaultConfig2.setMaxActive(100);
        defaultConfig2.setMaxWait(-1);
        defaultConfig2.setUrl("url");
        defaultConfig2.setMode("mode");
        assertNotEquals(defaultConfig, this.redisConfigProperties);
        assertEquals(defaultConfig, defaultConfig2);
        assertEquals(defaultConfig, defaultConfig);
        assertNotEquals(defaultConfig, "String");
        assertNotEquals(defaultConfig, null);
        assertEquals(defaultConfig.getMaster(), "master");
        assertTrue(defaultConfig.hashCode() != 0);
    }
}
