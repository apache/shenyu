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

package org.apache.shenyu.plugin.base.cache;

import org.apache.shenyu.common.dto.MetaData;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertSame;

class MetaDataCacheTest {

    private final MetaDataCache cache = MetaDataCache.getInstance();

    @AfterEach
    void tearDown() {
        cache.getMetaDataMap().clear();
        cache.getMetaDataCache().clear();
    }

    @Test
    void shouldPreferMostSpecificMatchingPattern() {
        MetaData broad = createMetaData("p", "/api/**");
        MetaData specific = createMetaData("a", "/api/users/**");
        cache.cache(broad);
        cache.cache(specific);

        assertSame(specific, cache.obtain("/api/users/42"));
    }

    private MetaData createMetaData(final String id, final String path) {
        MetaData metaData = new MetaData();
        metaData.setId(id);
        metaData.setPath(path);
        metaData.setEnabled(true);
        return metaData;
    }
}
