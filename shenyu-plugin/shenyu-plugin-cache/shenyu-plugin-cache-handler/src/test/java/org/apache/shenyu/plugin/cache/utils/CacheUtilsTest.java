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

package org.apache.shenyu.plugin.cache.utils;

import org.junit.jupiter.api.Test;
import org.springframework.http.HttpMethod;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;

import java.net.URI;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;

/**
 * Tests request identity dimensions in cache keys.
 */
class CacheUtilsTest {

    @Test
    void testMethodSeparatesEntries() {
        assertNotEquals(key("GET", "http://gateway.example/quote"), key("POST", "http://gateway.example/quote"));
        assertNotEquals(key("GET", "http://gateway.example/quote"), key("HEAD", "http://gateway.example/quote"));
    }

    @Test
    void testAuthoritySeparatesEntries() {
        assertNotEquals(key("GET", "http://first.example/quote"), key("GET", "http://second.example/quote"));
    }

    @Test
    void testRawQuerySeparatesEntries() {
        assertNotEquals(key("GET", "http://gateway.example/quote?v=a%26b=c"), key("GET", "http://gateway.example/quote?v=a&b=c"));
    }

    @Test
    void testIdenticalRequestsHaveIdenticalKeys() {
        assertEquals(key("GET", "http://gateway.example/quote?v=1"), key("GET", "http://gateway.example/quote?v=1"));
    }

    @Test
    void testMatchedSelectorAndRuleSeparateEntries() {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("http://gateway.example/quote").build());
        assertNotEquals(CacheUtils.responseKey(exchange, "first-selector", "rule"), CacheUtils.responseKey(exchange, "second-selector", "rule"));
        assertNotEquals(CacheUtils.responseKey(exchange, "selector", "first-rule"), CacheUtils.responseKey(exchange, "selector", "second-rule"));
        assertNotEquals(CacheUtils.responseKey(exchange, "a-b", "c"), CacheUtils.responseKey(exchange, "a", "b-c"));
    }

    private static String key(final String method, final String uri) {
        return CacheUtils.dataKey(MockServerWebExchange.from(MockServerHttpRequest.method(HttpMethod.valueOf(method), URI.create(uri)).build()));
    }
}
