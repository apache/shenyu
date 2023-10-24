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

package org.apache.shenyu.plugin.cache;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.cache.config.CacheConfig;
import org.apache.shenyu.plugin.cache.handler.CachePluginDataHandler;
import org.apache.shenyu.plugin.cache.redis.RedisConfigProperties;
import org.apache.shenyu.plugin.cache.utils.CacheUtils;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import redis.embedded.RedisServer;

import java.nio.charset.StandardCharsets;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * CachePluginDataHandlerTest.
 */
@ExtendWith(MockitoExtension.class)
public class CachePluginDataHandlerTest {

    private static RedisServer redisServer;

    @BeforeAll
    public static void startup() {
        redisServer = RedisServer.builder()
                .port(63794)
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
        final CachePluginDataHandler cacheHandler = new CachePluginDataHandler();
        final PluginData pluginData = new PluginData();
        RedisConfigProperties redisConfigProperties = new RedisConfigProperties();
        redisConfigProperties.setUrl("127.0.0.1:63794");
        pluginData.setConfig(GsonUtils.getInstance().toJson(redisConfigProperties));
        cacheHandler.handlerPlugin(pluginData);
        testCacheData("redis-cache-data");
    }

    @Test
    public void testMemoryCache() {
        final CacheConfig cacheConfig = new CacheConfig();
        cacheConfig.setCacheType("memory");
        cacheConfig.setConfig("config");
        final CachePluginDataHandler cacheHandler = new CachePluginDataHandler();
        final PluginData pluginData = new PluginData();
        pluginData.setConfig(GsonUtils.getInstance().toJson(cacheConfig));
        Assertions.assertEquals(cacheConfig.getConfig(), "config");
        Assertions.assertTrue(cacheConfig.hashCode() != 0);
        Assertions.assertEquals(cacheConfig, cacheConfig);
        cacheHandler.handlerPlugin(pluginData);
        testCacheData("memory-cache-data");
    }

    @Test
    public void handlerPluginTest() {
        final CacheConfig cacheConfig = new CacheConfig();
        final CachePluginDataHandler cacheHandler = new CachePluginDataHandler();
        Assertions.assertDoesNotThrow(() -> cacheHandler.handlerPlugin(null));
        final PluginData pluginData = new PluginData();
        pluginData.setEnabled(false);
        Assertions.assertDoesNotThrow(() -> cacheHandler.handlerPlugin(pluginData));
        pluginData.setConfig(null);
        pluginData.setEnabled(true);
        Assertions.assertDoesNotThrow(() -> cacheHandler.handlerPlugin(pluginData));
        pluginData.setConfig(GsonUtils.getInstance().toJson(cacheConfig));
        Assertions.assertDoesNotThrow(() -> cacheHandler.handlerPlugin(pluginData));
        Assertions.assertDoesNotThrow(() -> cacheHandler.handlerPlugin(pluginData));
    }

    @Test
    public void pluginNamedTest() {
        final CachePluginDataHandler cacheHandler = new CachePluginDataHandler();
        Assertions.assertEquals(PluginEnum.CACHE.getName(), cacheHandler.pluginNamed());
    }

    @Test
    public void handlerRuleTest() {
        final CachePluginDataHandler cacheHandler = new CachePluginDataHandler();
        cacheHandler.handlerRule(RuleData.builder().handle("{}").build());
    }

    @Test
    public void removeRuleTest() {
        final CachePluginDataHandler cacheHandler = new CachePluginDataHandler();
        cacheHandler.removeRule(RuleData.builder().handle("{}").build());
    }

    private void testCacheData(final String testKey) {
        ICache cache = CacheUtils.getCache();
        assert null != cache;
        cache.isExist(testKey).subscribe(v -> assertEquals(Boolean.FALSE, v));
        cache.cacheData(testKey, testKey.getBytes(StandardCharsets.UTF_8), 10)
                .subscribe(v -> assertEquals(Boolean.TRUE, v));
        cache.isExist(testKey).subscribe(v -> assertEquals(Boolean.TRUE, v));
        cache.getData(testKey).subscribe(data -> assertEquals(testKey, new String(data, StandardCharsets.UTF_8)));
    }
}
