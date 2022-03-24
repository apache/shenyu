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

package org.apache.shenyu.plugin.cache.base.redis.handler;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.cache.base.ICache;
import org.apache.shenyu.plugin.cache.base.config.CacheConfig;
import org.apache.shenyu.plugin.cache.base.enums.CacheEnum;
import org.apache.shenyu.plugin.cache.base.handler.CacheHandler;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import java.nio.charset.StandardCharsets;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * CacheHandlerTest.
 */
@ExtendWith(MockitoExtension.class)
public class CacheHandlerTest {

    private void testCacheHandler() {
        final CacheConfig cacheConfig = new CacheConfig();
        cacheConfig.setCacheType(CacheEnum.MEMORY.getName());
        final CacheHandler cacheHandler = new CacheHandler();
        final PluginData pluginData = new PluginData();
        pluginData.setConfig(GsonUtils.getInstance().toJson(cacheConfig));
        cacheHandler.handlerPlugin(pluginData);
        ICache cache = Singleton.INST.get(ICache.class);
        final String testKey = "testCacheHandler2";
        assertEquals(Boolean.FALSE, cache.isExist(testKey));
        boolean flag = cache.cacheData(testKey, testKey.getBytes(StandardCharsets.UTF_8), 100);
        assertEquals(Boolean.TRUE, flag);
        assertEquals(Boolean.TRUE, cache.isExist(testKey));
        final byte[] value = cache.getData(testKey);
        assert null != value;
        assertEquals(testKey, new String(value, StandardCharsets.UTF_8));
    }
}
