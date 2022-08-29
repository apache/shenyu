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

package org.apache.shenyu.plugin.global.cache;

import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.plugin.base.cache.MetaDataCache;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * The Test Case For MetaDataCache.
 */
public final class MetaDataCacheTest {

    private MetaData metaData;

    private MetaDataCache metaDataCache;

    @BeforeEach
    public void setUp() {
        metaData = MetaData.builder()
                .id("1")
                .path("/home")
                .build();
        metaDataCache = MetaDataCache.getInstance();
    }

    @Test
    public void getInstance() {
        assertNotNull(metaDataCache);
    }

    @Test
    public void testMetaDataCache() {
        assertNull(metaDataCache.obtain("/test"));
        metaDataCache.cache(this.metaData);
        assertEquals("/home", metaDataCache.obtain("/home").getPath());
        metaDataCache.remove(this.metaData);
        assertNull(metaDataCache.obtain("/home"));
    }
}
