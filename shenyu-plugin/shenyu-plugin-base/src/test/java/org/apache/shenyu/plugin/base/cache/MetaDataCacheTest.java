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

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertNull;
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
        assertSame(broad, cache.obtain("/api/users/42"));

        cache.cache(specific);

        assertSame(specific, cache.obtain("/api/users/42"));
    }

    @Test
    void shouldRemoveByIdOnlyRecordWithoutPath() {
        MetaData full = createMetaData("id-1", "/rpc/dubbo/save");
        cache.cache(full);
        assertSame(full, cache.obtain("/rpc/dubbo/save"));

        MetaData idOnly = new MetaData();
        idOnly.setId("id-1");

        assertDoesNotThrow(() -> cache.remove(idOnly));
        assertNull(cache.obtain("/rpc/dubbo/save"));
    }

    @Test
    void shouldRemoveByPathOnlyRecordWithoutId() {
        MetaData full = createMetaData("id-2", "/rpc/sofa/find");
        cache.cache(full);
        assertSame(full, cache.obtain("/rpc/sofa/find"));

        MetaData pathOnly = new MetaData();
        pathOnly.setPath("/rpc/sofa/find");

        assertDoesNotThrow(() -> cache.remove(pathOnly));
        assertNull(cache.obtain("/rpc/sofa/find"));
    }

    @Test
    void shouldKeepRemovingFullRecord() {
        MetaData full = createMetaData("id-3", "/rpc/http/get");
        cache.cache(full);

        cache.remove(createMetaData("id-3", "/rpc/http/get"));

        assertNull(cache.obtain("/rpc/http/get"));
    }

    @Test
    void shouldBeIdempotentForPartialRecords() {
        MetaData idOnly = new MetaData();
        idOnly.setId("missing-id");
        MetaData pathOnly = new MetaData();
        pathOnly.setPath("/rpc/never/cached");

        assertDoesNotThrow(() -> cache.remove(idOnly));
        assertDoesNotThrow(() -> cache.remove(pathOnly));
        assertDoesNotThrow(() -> cache.remove(idOnly));
    }

    private MetaData createMetaData(final String id, final String path) {
        MetaData metaData = new MetaData();
        metaData.setId(id);
        metaData.setPath(path);
        metaData.setEnabled(true);
        return metaData;
    }
}
