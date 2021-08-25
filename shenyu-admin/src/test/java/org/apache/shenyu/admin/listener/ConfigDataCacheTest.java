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

package org.apache.shenyu.admin.listener;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

/**
 * The TestCase for ConfigDataCache.
 */
public final class ConfigDataCacheTest {

    @Test
    public void testUpdate() {
        String group = "default";
        String json = "{\"name\":\"shenyu\"}";
        int hashValue1 = 697816813;
        ConfigDataCache cache = new ConfigDataCache(group, json, hashValue1, 0);
        assertEquals(cache.getHashValue(), hashValue1);
        assertEquals(cache.getJson(), json);
        assertEquals(cache.getGroup(), group);
        int hashValue2 = 157496442;
        cache.update(hashValue2, 1);
        assertEquals(cache.getHashValue(), hashValue2);
        assertEquals(cache.getLastModifyTime(), 1);
    }
}
