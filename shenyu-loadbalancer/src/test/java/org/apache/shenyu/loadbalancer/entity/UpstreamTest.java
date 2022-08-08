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

package org.apache.shenyu.loadbalancer.entity;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * The type Upstream check task test.
 */
public class UpstreamTest {

    @Test
    public void upstreamTest() {
        Upstream upstream = Upstream.builder()
                .group("group")
                .url("url")
                .timestamp(1)
                .warmup(1)
                .version("version")
                .weight(1)
                .status(true)
                .build();
        upstream.setGroup("group");
        upstream.setHealthy(true);
        upstream.setUrl("url");
        upstream.setLastHealthTimestamp(1L);
        upstream.setStatus(true);
        upstream.setLastUnhealthyTimestamp(1L);
        upstream.setVersion("version");
        Assertions.assertEquals(upstream.buildDomain(), "http://url");
        Assertions.assertEquals(upstream.getGroup(), "group");
        Assertions.assertNull(upstream.getProtocol());
        Assertions.assertEquals(upstream.getLastHealthTimestamp(), 1L);
        Assertions.assertEquals(upstream.getLastUnhealthyTimestamp(), 1L);
        Assertions.assertEquals(upstream.getUrl(), "url");
        Assertions.assertEquals(upstream.getWarmup(), 1);
        Assertions.assertEquals(upstream.getWeight(), 1);
        Assertions.assertEquals(upstream.getVersion(), "version");
        Assertions.assertEquals(upstream.getTimestamp(), 1);
        Assertions.assertTrue(upstream.isHealthy());
        Assertions.assertTrue(upstream.isStatus());
        Upstream upstream2 = Upstream.builder()
                .protocol("https://")
                .url("url")
                .weight(1)
                .status(true)
                .build();
        Assertions.assertEquals(upstream2.buildDomain(), "https://url");
        Assertions.assertNotEquals(upstream, upstream2);
        Assertions.assertNotEquals(upstream, null);
        Assertions.assertNotEquals(upstream, "");
        Assertions.assertEquals(upstream, upstream);
        Upstream upstream3 = Upstream.builder()
                .protocol("https://")
                .url("url")
                .weight(1)
                .status(true)
                .build();
        Assertions.assertEquals(upstream2, upstream3);
        Assertions.assertNotNull(upstream2.toString());
        Assertions.assertTrue(upstream2.hashCode() >= 0);
    }
}
