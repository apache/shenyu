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
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.cache.ICache;
import org.apache.shenyu.plugin.cache.ICacheBuilder;
import org.apache.shenyu.spi.ExtensionLoader;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import redis.embedded.RedisServer;

import java.nio.charset.StandardCharsets;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * RedisCacheTest.
 */
@ExtendWith(MockitoExtension.class)
public class RedisCacheTest {

    private static RedisServer redisServer;

    @BeforeAll
    public static void startup() {
        redisServer = RedisServer.builder()
                .port(63793)
                .setting("maxmemory 64m")
                .build();
        redisServer.start();
    }

    @AfterAll
    public static void end() {
        redisServer.stop();
    }

    @Test
    public void testRedisCache() {
        final String testKey = "testRedisCache";
        final ICache cache = new RedisCache(getConfig());
        assertEquals(Boolean.FALSE, cache.isExist(testKey));
        boolean flag = cache.cacheData(testKey, testKey.getBytes(StandardCharsets.UTF_8), 1000);
        assertEquals(Boolean.TRUE, flag);
        assertEquals(Boolean.TRUE, cache.isExist(testKey));
        final byte[] value = cache.getData(testKey);
        assert null != value;
        assertEquals(testKey, new String(value, StandardCharsets.UTF_8));
    }

    @Test
    public void testLoadRedisCache() {
        final ICacheBuilder cacheBuilder = ExtensionLoader.getExtensionLoader(ICacheBuilder.class).getJoin("redis");
        ICache cache = cacheBuilder.builderCache(GsonUtils.getInstance().toJson(getConfig()));
        final String testKey = "testLoadRedisCache";
        assertEquals(Boolean.FALSE, cache.isExist(testKey));
        boolean flag = cache.cacheData(testKey, testKey.getBytes(StandardCharsets.UTF_8), 1000);
        assertEquals(Boolean.TRUE, flag);
        assertEquals(Boolean.TRUE, cache.isExist(testKey));
        final byte[] value = cache.getData(testKey);
        assert null != value;
        assertEquals(testKey, new String(value, StandardCharsets.UTF_8));
    }

    private RedisConfigProperties getConfig() {
        RedisConfigProperties config = new RedisConfigProperties();
        config.setMode(RedisModeEnum.STANDALONE.getName());
        config.setUrl("127.0.0.1:63793");
        return config;
    }
}
