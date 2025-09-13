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

import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.selector.CacheUpstream;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.cache.cache.ApplicationConfigCache;
import org.apache.shenyu.plugin.cache.memory.MemoryCache;
import org.apache.shenyu.plugin.cache.redis.RedisCache;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * ApplicationConfigCacheTest.
 */
@ExtendWith(MockitoExtension.class)
public class ApplicationConfigCacheTest {

    private ApplicationConfigCache cache;

    private MetaData metaData;

    private SelectorData selectorData;

    @BeforeEach
    void setUp() {
        metaData = new MetaData();
        metaData.setId("1332017966661636096");
        metaData.setAppName("cache");
        metaData.setPath("/http/findAll");
        metaData.setServiceName("org.apache.shenyu.test.http.api.service.httpTestService");
        metaData.setMethodName("findAll");
        metaData.setRpcType(RpcTypeEnum.HTTP.getName());
        selectorData = SelectorData.builder().id("153153464562434")
                .handle("{\n"
                        + "  \"cacheType\": \"redis\",\n"
                        + "  \"url\": \"localhost:6379\",\n"
                        + "  \"password\": \"shenyu123\",\n"
                        + "  \"database\": \"0\",\n"
                        + "  \"master\": \"mymaster\",\n"
                        + "  \"mode\": \"sentinel\",\n"
                        + "  \"maxIdle\": 8,\n"
                        + "  \"minIdle\": 2,\n"
                        + "  \"maxActive\": 100,\n"
                        + "  \"maxWait\": 3000,\n"
                        + "  \"protocol\": \"redis\",\n"
                        + "  \"upstreamHost\": \"127.0.0.1\",\n"
                        + "  \"upstreamUrl\": \"localhost:6379\",\n"
                        + "  \"weight\": 100,\n"
                        + "  \"status\": true,\n"
                        + "  \"timestamp\": 1688611200000\n"
                        + "}").build();

        cache = ApplicationConfigCache.getInstance();
        cache.invalidateAll();
    }

    @Test
    public void testInit() {
        CacheUpstream cacheUpstream = GsonUtils.getInstance().fromJson(selectorData.getHandle(), CacheUpstream.class);
        ICache redisCache = cache.init(selectorData.getId(), cacheUpstream);
        assertNotNull(redisCache);
        assertTrue(redisCache instanceof RedisCache);
        cache.invalidateAll();
        cacheUpstream.setCacheType("memory");
        ICache memCache = cache.init(selectorData.getId(), cacheUpstream);
        assertNotNull(memCache);
        assertTrue(memCache instanceof MemoryCache);
    }
}
