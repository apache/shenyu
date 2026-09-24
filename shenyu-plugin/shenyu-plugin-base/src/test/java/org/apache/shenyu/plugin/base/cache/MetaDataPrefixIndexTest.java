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
import org.apache.shenyu.plugin.base.utils.PathMatchUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.times;

/**
 * Regression tests for metadata prefix indexing.
 */
public class MetaDataPrefixIndexTest {

    private final MetaDataCache cache = MetaDataCache.getInstance();

    private final List<MetaData> registered = new ArrayList<>();

    @AfterEach
    public void cleanup() {
        registered.forEach(cache::remove);
        cache.clean();
    }

    @Test
    public void testColdLookupOnlyMatchesRelevantPrefix() {
        for (int i = 0; i < 100; i++) {
            register("route-" + i, "/service-" + i + "/**");
        }
        try (MockedStatic<PathMatchUtils> matcher = Mockito.mockStatic(PathMatchUtils.class, Mockito.CALLS_REAL_METHODS)) {
            assertEquals("route-42", cache.obtain("/service-42/cold").getId());
            matcher.verify(() -> PathMatchUtils.match("/service-42/**", "/service-42/cold"), times(1));
            matcher.verifyNoMoreInteractions();
        }
    }

    @Test
    public void testFallbackPatternsAndRepeatedSeparators() {
        register("literal", "/prefix//items/**");
        register("variable", "/{tenant}/records/**");
        register("question", "/tenant?/items/**");
        assertEquals("literal", cache.obtain("//prefix/items/one").getId());
        assertEquals("variable", cache.obtain("/acme/records/one").getId());
        assertEquals("question", cache.obtain("/tenant1/items/one").getId());
    }

    @Test
    public void testChangedPrefixAndRemovalUpdateIndex() {
        final MetaData old = register("changing", "/old-prefix/**");
        assertEquals("changing", cache.obtain("/old-prefix/one").getId());
        MetaData updated = register("changing", "/new-prefix/**");
        assertNull(cache.obtain("/old-prefix/two"));
        assertEquals("changing", cache.obtain("/new-prefix/one").getId());
        cache.remove(updated);
        assertNull(cache.obtain("/new-prefix/two"));
        registered.remove(old);
    }

    @Test
    public void testDisabledWildcardDoesNotMatch() {
        MetaData metadata = MetaData.builder().id("disabled").path("/disabled-prefix/**").enabled(false).build();
        registered.add(metadata);
        cache.cache(metadata);
        assertNull(cache.obtain("/disabled-prefix/one"));
    }

    private MetaData register(final String id, final String path) {
        MetaData metadata = MetaData.builder().id(id).path(path).enabled(true).build();
        registered.add(metadata);
        cache.cache(metadata);
        return metadata;
    }
}
