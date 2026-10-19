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

import com.google.gson.JsonObject;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.JsonUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.Map;

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
                .gray(false)
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
        Assertions.assertEquals("https://url", upstream2.buildDomain());
        Assertions.assertNotEquals(upstream, upstream2);
        Assertions.assertNotEquals(null, upstream);
        Assertions.assertNotEquals("", upstream);
        Assertions.assertEquals(upstream, upstream);
        Upstream upstream3 = Upstream.builder()
                .protocol("https://")
                .url("url")
                .weight(1)
                .status(true)
                .build();
        Upstream upstream4 = Upstream.builder()
                .protocol("https://")
                .url("url")
                .weight(2)
                .status(true)
                .build();
        Assertions.assertEquals(upstream2, upstream3);
        Assertions.assertEquals(upstream2, upstream4);
        Assertions.assertEquals(upstream2.hashCode(), upstream4.hashCode());
        Assertions.assertNotNull(upstream2.toString());
    }

    @Test
    public void identityShouldRemainAnImmutableSnapshot() {
        final Upstream upstream = Upstream.builder().protocol("dubbo://").url("127.0.0.1:20880")
                .group("blue").version("v1").weight(10).build();
        final UpstreamIdentity original = upstream.identity();
        final String originalHashKey = original.hashKey();
        final String originalLeastActiveKey = original.leastActiveKey();
        final Map<UpstreamIdentity, String> cached = new HashMap<>();
        cached.put(original, "original");

        upstream.setWeight(20);
        upstream.setStatus(false);
        Assertions.assertSame(original, upstream.identity());
        upstream.setUrl("127.0.0.2:20880");
        final UpstreamIdentity changedUrl = upstream.identity();
        Assertions.assertNotEquals(original, changedUrl);
        upstream.setGroup("green");
        final UpstreamIdentity changedGroup = upstream.identity();
        Assertions.assertNotEquals(changedUrl, changedGroup);
        upstream.setVersion("v2");
        Assertions.assertNotEquals(changedGroup, upstream.identity());
        Assertions.assertEquals(originalHashKey, original.hashKey());
        Assertions.assertEquals(originalLeastActiveKey, original.leastActiveKey());
        Assertions.assertEquals("original", cached.get(original));
        final Upstream originalTarget = Upstream.builder().protocol("dubbo://").url("127.0.0.1:20880")
                .group("blue").version("v1").build();
        Assertions.assertEquals("original", cached.get(originalTarget.identity()));
        final Upstream currentTarget = Upstream.builder().protocol("dubbo://").url("127.0.0.2:20880")
                .group("green").version("v2").build();
        Assertions.assertEquals(currentTarget, upstream);
        Assertions.assertEquals(currentTarget.hashCode(), upstream.hashCode());

        final JsonObject gsonFields = GsonUtils.getInstance().fromJson(GsonUtils.getInstance().toJson(upstream), JsonObject.class);
        Assertions.assertFalse(gsonFields.has("identity"));
        Assertions.assertEquals("127.0.0.2:20880", gsonFields.get("url").getAsString());
        Assertions.assertEquals("green", gsonFields.get("group").getAsString());
        Assertions.assertEquals("v2", gsonFields.get("version").getAsString());
        final Map<String, Object> jacksonFields = JsonUtils.toMap(upstream);
        Assertions.assertFalse(jacksonFields.containsKey("identity"));
        Assertions.assertEquals("127.0.0.2:20880", jacksonFields.get("url"));
        Assertions.assertEquals("green", jacksonFields.get("group"));
        Assertions.assertEquals("v2", jacksonFields.get("version"));
    }
}
