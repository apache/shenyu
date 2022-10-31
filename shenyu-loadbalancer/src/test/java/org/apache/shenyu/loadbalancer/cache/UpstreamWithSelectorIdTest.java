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
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * The type UpstreamWithSelectorId check task test.
 */
public class UpstreamWithSelectorIdTest {

    private static final String SELECTOR_ID = "selectorId";

    @Test
    public void initUpstreamCacheManagerTest() {
        final UpstreamWithSelectorId upstream = new UpstreamWithSelectorId(SELECTOR_ID, null);
        Assertions.assertEquals(upstream, upstream);
        final UpstreamWithSelectorId upstream2 = new UpstreamWithSelectorId(SELECTOR_ID, null);
        Assertions.assertEquals(upstream, upstream2);
        upstream.setUpstream(null);
        upstream.setSelectorId(null);
        Assertions.assertTrue(upstream.hashCode() >= 0);
        Assertions.assertNotNull(upstream.toString());
        Assertions.assertNotEquals(upstream, upstream2);
        Assertions.assertNotEquals(upstream, null);
        Assertions.assertNotEquals(upstream, SELECTOR_ID);
        final UpstreamWithSelectorId upstream3 = new UpstreamWithSelectorId(SELECTOR_ID, Upstream.builder().build());
        Assertions.assertNotEquals(upstream, upstream3);
        final UpstreamWithSelectorId upstream4 = new UpstreamWithSelectorId(null, Upstream.builder().build());
        Assertions.assertNotEquals(upstream, upstream4);
    }
}
