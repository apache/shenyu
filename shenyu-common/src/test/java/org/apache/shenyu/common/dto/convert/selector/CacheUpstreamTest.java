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

package org.apache.shenyu.common.dto.convert.selector;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

/**
 * Test case for CacheUpstream.
 */
public final class CacheUpstreamTest {

    @Test
    public void testBuilderCopiesAllStateIntoBuiltInstance() {
        CacheUpstream upstream = CacheUpstream.builder()
                .upstreamHost("host-1")
                .protocol("http")
                .upstreamUrl("localhost:6379")
                .status(false)
                .timestamp(1650549243L)
                .cacheType("redis")
                .url("localhost:6379")
                .password("secret")
                .database("0")
                .master("mymaster")
                .mode("cluster")
                .maxIdle(8)
                .minIdle(2)
                .maxActive(16)
                .maxWait(3000)
                .build();
        assertEquals("host-1", upstream.getUpstreamHost());
        assertEquals("http", upstream.getProtocol());
        assertEquals("localhost:6379", upstream.getUpstreamUrl());
        assertFalse(upstream.isStatus());
        assertEquals(1650549243L, upstream.getTimestamp());
        assertEquals("redis", upstream.getCacheType());
        assertEquals("localhost:6379", upstream.getUrl());
        assertEquals("secret", upstream.getPassword());
        assertEquals("0", upstream.getDatabase());
        assertEquals("mymaster", upstream.getMaster());
        assertEquals("cluster", upstream.getMode());
        assertEquals(8, upstream.getMaxIdle());
        assertEquals(2, upstream.getMinIdle());
        assertEquals(16, upstream.getMaxActive());
        assertEquals(3000, upstream.getMaxWait());
    }
}
