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

package org.apache.shenyu.loadbalancer.cache;

import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Tests bounded predicate ownership and immutable subset publication.
 */
class UpstreamLabelCacheTest {

    @Test
    void testExactConjunctionAndNegativeCaching() {
        UpstreamLabelCache cache = new UpstreamLabelCache();
        Upstream east = upstream("east", Map.of("release", "canary", "region", "east", "extra", "value"));
        Upstream west = upstream("west", Map.of("release", "canary", "region", "west"));
        Map<String, String> predicate = new HashMap<>(Map.of("release", "canary", "region", "east"));
        Map<String, String> absent = Map.of("release", "missing");
        cache.register("s", "r", List.of(predicate, absent), List.of(east, west));
        predicate.put("region", "west");
        List<Upstream> matched = cache.get("s", Map.of("release", "canary", "region", "east"));
        assertEquals(List.of(east), matched);
        assertSame(east, matched.get(0));
        assertThrows(UnsupportedOperationException.class, matched::clear);
        assertTrue(cache.get("s", absent).isEmpty());
        assertNull(cache.get("s", predicate));
        assertNull(cache.get("other", absent));
    }

    @Test
    void testOwnersSharePredicatesAndReleaseUnusedPools() {
        UpstreamLabelCache cache = new UpstreamLabelCache();
        Map<String, String> blue = Map.of("team", "blue");
        Map<String, String> red = Map.of("team", "red");
        List<Upstream> upstreams = List.of(upstream("blue", blue), upstream("red", red));
        cache.register("s", "r1", List.of(blue), upstreams);
        cache.register("s", "r2", List.of(blue), upstreams);
        cache.register("s", "r1", List.of(red), upstreams);
        assertEquals(1, cache.get("s", blue).size());
        assertEquals(1, cache.get("s", red).size());
        cache.unregister("s", "r2", upstreams);
        assertNull(cache.get("s", blue));
        cache.register("s", "r1", List.of(), upstreams);
        assertNull(cache.get("s", red));
    }

    @Test
    void testRefreshPublishesNewMembershipWithoutChangingPreviousList() {
        UpstreamLabelCache cache = new UpstreamLabelCache();
        Map<String, String> predicate = Map.of("release", "stable");
        Upstream original = upstream("original", predicate);
        Upstream added = upstream("added", predicate);
        cache.register("s", "r", List.of(predicate), List.of(original));
        List<Upstream> previous = cache.get("s", predicate);
        cache.refresh("s", List.of(added));
        assertEquals(List.of(original), previous);
        assertEquals(List.of(added), cache.get("s", predicate));
        cache.refresh("s", null);
        assertTrue(cache.get("s", predicate).isEmpty());
        cache.refresh("s", List.of(original));
        assertEquals(previous, cache.get("s", predicate));
        cache.remove("s");
        assertNull(cache.get("s", predicate));
    }

    private Upstream upstream(final String url, final Map<String, String> labels) {
        Upstream upstream = Upstream.builder().url(url).build();
        upstream.setLabels(labels);
        return upstream;
    }
}
