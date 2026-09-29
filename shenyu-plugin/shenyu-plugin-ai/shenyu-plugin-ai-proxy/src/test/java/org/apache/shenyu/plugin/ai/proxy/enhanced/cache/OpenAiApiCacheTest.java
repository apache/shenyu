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

package org.apache.shenyu.plugin.ai.proxy.enhanced.cache;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.ai.openai.api.OpenAiApi;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;

class OpenAiApiCacheTest {

    private OpenAiApiCache cache;

    @BeforeEach
    void setUp() {
        cache = new OpenAiApiCache();
        cache.clearAll();
    }

    @Test
    void testComputeIfAbsentReusesExisting() {
        final OpenAiApi api = createTestApi("https://api.openai.com", "key1");
        final OpenAiApi cached = cache.computeIfAbsent("key1", () -> api);
        final OpenAiApi reused = cache.computeIfAbsent("key1", () -> createTestApi("https://api.openai.com", "key1-other"));

        assertSame(cached, reused);
        assertEquals(1, cache.size());
    }

    @Test
    void testComputeIfAbsentCreatesNewForDifferentKey() {
        final OpenAiApi api1 = cache.computeIfAbsent("key1", () -> createTestApi("https://api.openai.com", "key1"));
        final OpenAiApi api2 = cache.computeIfAbsent("key2", () -> createTestApi("https://api.deepseek.com", "key2"));

        // Different keys produce different instances
        assertEquals(2, cache.size());
    }

    @Test
    void testRemoveBySelectorId() {
        cache.computeIfAbsent("sel1|main_123", () -> createTestApi("https://a.com", "k1"));
        cache.computeIfAbsent("sel1|adminFallback_456", () -> createTestApi("https://b.com", "k2"));
        cache.computeIfAbsent("sel2|main_789", () -> createTestApi("https://c.com", "k3"));

        assertEquals(3, cache.size());

        cache.remove("sel1");

        assertEquals(1, cache.size());
    }

    @Test
    void testRemoveWithNullSelectorIdDoesNotThrow() {
        cache.computeIfAbsent("key1", () -> createTestApi("https://a.com", "k1"));
        cache.remove(null);
        assertEquals(1, cache.size());
    }

    @Test
    void testClearAll() {
        cache.computeIfAbsent("key1", () -> createTestApi("https://a.com", "k1"));
        cache.computeIfAbsent("key2", () -> createTestApi("https://b.com", "k2"));

        assertEquals(2, cache.size());

        cache.clearAll();

        assertEquals(0, cache.size());
    }

    private OpenAiApi createTestApi(final String baseUrl, final String apiKey) {
        return OpenAiApi.builder().baseUrl(baseUrl).apiKey(apiKey).build();
    }
}
