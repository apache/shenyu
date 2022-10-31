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

package org.apache.shenyu.plugin.cache.memory;

import org.junit.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import java.nio.charset.StandardCharsets;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * MemoryCacheTest.
 */
@ExtendWith(MockitoExtension.class)
public class MemoryCacheTest {

    @Test
    public void testMemoryCache() {
        final MemoryCache memoryCache = new MemoryCache();
        final String key = "data";
        memoryCache.isExist(key).subscribe(v -> assertEquals(Boolean.FALSE, v));
        memoryCache.cacheData(key, "data".getBytes(StandardCharsets.UTF_8), 10)
                .subscribe(v -> assertEquals(Boolean.TRUE, v));
        memoryCache.isExist(key).subscribe(v -> assertEquals(Boolean.TRUE, v));
        memoryCache.getData(key).subscribe(v -> assertEquals("data", new String(v, StandardCharsets.UTF_8)));
    }

}
