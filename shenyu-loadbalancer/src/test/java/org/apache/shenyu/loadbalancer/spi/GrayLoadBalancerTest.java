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

package org.apache.shenyu.loadbalancer.spi;

import org.apache.shenyu.common.dto.convert.rule.GrayCondition;
import org.apache.shenyu.common.dto.convert.rule.GrayConfig;
import org.apache.shenyu.common.dto.convert.rule.MetadataMatch;
import org.apache.shenyu.loadbalancer.entity.LoadBalanceData;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class GrayLoadBalancerTest {

    private static final MetadataMatch VERSION_V2 = new MetadataMatch("version", "v2");

    private GrayLoadBalancer grayLoadBalancer;

    private Upstream normalUpstream;

    private Upstream grayUpstream;

    @BeforeEach
    public void setUp() {
        grayLoadBalancer = new GrayLoadBalancer();
        normalUpstream = Upstream.builder()
                .url("normal-upstream")
                .weight(50)
                .status(true)
                .gray(false)
                .metadata(Map.of("version", "v1"))
                .build();
        grayUpstream = Upstream.builder()
                .url("gray-upstream")
                .weight(50)
                .status(true)
                .gray(true)
                .metadata(Map.of("version", "v2"))
                .build();
    }

    @Test
    public void testSelectEmptyList() {
        assertNull(grayLoadBalancer.select(Collections.emptyList(), new LoadBalanceData()));
    }

    @Test
    public void testSelectNullList() {
        assertNull(grayLoadBalancer.select(null, new LoadBalanceData()));
    }

    @Test
    public void testSelectWithoutGrayConfig() {
        List<Upstream> upstreams = Arrays.asList(normalUpstream, grayUpstream);
        Upstream result = grayLoadBalancer.select(upstreams, new LoadBalanceData());
        assertNotNull(result);
    }

    // ========== Condition + Percent cooperation ==========

    @Test
    public void testConditionMatchedWithPercent100RoutesToGray() {
        GrayCondition condition = new GrayCondition("header", "X-Canary", "=", "true");
        GrayConfig config = new GrayConfig(Collections.singletonList(condition), 100, "random", VERSION_V2);

        LoadBalanceData data = buildLoadBalanceData(config);
        Map<String, Collection<String>> headers = new HashMap<>();
        headers.put("X-Canary", Collections.singletonList("true"));
        data.setHeaders(headers);

        List<Upstream> upstreams = Arrays.asList(normalUpstream, grayUpstream);
        Upstream result = grayLoadBalancer.select(upstreams, data);
        assertEquals("gray-upstream", result.getUrl());
    }

    @Test
    public void testConditionMatchedWithPercent0RoutesToNormal() {
        // condition matches but percent=0 → should NOT route to gray
        GrayCondition condition = new GrayCondition("header", "X-Canary", "=", "true");
        GrayConfig config = new GrayConfig(Collections.singletonList(condition), 0, "random", VERSION_V2);

        LoadBalanceData data = buildLoadBalanceData(config);
        Map<String, Collection<String>> headers = new HashMap<>();
        headers.put("X-Canary", Collections.singletonList("true"));
        data.setHeaders(headers);

        List<Upstream> upstreams = Arrays.asList(normalUpstream, grayUpstream);
        Upstream result = grayLoadBalancer.select(upstreams, data);
        assertEquals("normal-upstream", result.getUrl());
    }

    @Test
    public void testConditionNotMatchedRoutesToNormal() {
        GrayCondition condition = new GrayCondition("header", "X-Canary", "=", "true");
        GrayConfig config = new GrayConfig(Collections.singletonList(condition), 100, "random", VERSION_V2);

        LoadBalanceData data = buildLoadBalanceData(config);
        data.setHeaders(new HashMap<>());

        List<Upstream> upstreams = Arrays.asList(normalUpstream, grayUpstream);
        Upstream result = grayLoadBalancer.select(upstreams, data);
        assertEquals("normal-upstream", result.getUrl());
    }

    @Test
    public void testNoConditionWithPercent100RoutesToNormal() {
        // no condition → gray not active, even with percent=100
        GrayConfig config = new GrayConfig(null, 100, "random", VERSION_V2);
        LoadBalanceData data = buildLoadBalanceData(config);

        List<Upstream> upstreams = Arrays.asList(normalUpstream, grayUpstream);
        Upstream result = grayLoadBalancer.select(upstreams, data);
        assertEquals("normal-upstream", result.getUrl());
    }

    @Test
    public void testEmptyConditionsWithPercent100RoutesToNormal() {
        // empty conditions list → gray not active
        GrayConfig config = new GrayConfig(Collections.emptyList(), 100, "random", VERSION_V2);
        LoadBalanceData data = buildLoadBalanceData(config);

        List<Upstream> upstreams = Arrays.asList(normalUpstream, grayUpstream);
        Upstream result = grayLoadBalancer.select(upstreams, data);
        assertEquals("normal-upstream", result.getUrl());
    }

    @Test
    public void testConditionMatchedWithPercent50Distribution() {
        // condition matched + percent=50 → roughly half go to gray
        GrayCondition condition = new GrayCondition("header", "X-Canary", "=", "true");
        GrayConfig config = new GrayConfig(Collections.singletonList(condition), 50, "random", VERSION_V2);

        LoadBalanceData data = buildLoadBalanceData(config);
        Map<String, Collection<String>> headers = new HashMap<>();
        headers.put("X-Canary", Collections.singletonList("true"));
        data.setHeaders(headers);

        List<Upstream> upstreams = Arrays.asList(normalUpstream, grayUpstream);
        Set<String> selected = new HashSet<>();
        for (int i = 0; i < 200; i++) {
            Upstream result = grayLoadBalancer.select(upstreams, data);
            selected.add(result.getUrl());
        }
        // with 200 iterations at 50%, both should appear
        assertTrue(selected.contains("gray-upstream"));
        assertTrue(selected.contains("normal-upstream"));
    }

    // ========== Condition types ==========

    @Test
    public void testCookieConditionMatch() {
        GrayCondition condition = new GrayCondition("cookie", "gray_flag", "=", "on");
        GrayConfig config = new GrayConfig(Collections.singletonList(condition), 100, "random", VERSION_V2);

        LoadBalanceData data = buildLoadBalanceData(config);
        Map<String, String> cookies = new HashMap<>();
        cookies.put("gray_flag", "on");
        data.setCookies(cookies);

        List<Upstream> upstreams = Arrays.asList(normalUpstream, grayUpstream);
        Upstream result = grayLoadBalancer.select(upstreams, data);
        assertEquals("gray-upstream", result.getUrl());
    }

    @Test
    public void testQueryConditionMatch() {
        GrayCondition condition = new GrayCondition("query", "version", "=", "v2");
        GrayConfig config = new GrayConfig(Collections.singletonList(condition), 100, "random", VERSION_V2);

        LoadBalanceData data = buildLoadBalanceData(config);
        Map<String, Collection<String>> queryParams = new HashMap<>();
        queryParams.put("version", Collections.singletonList("v2"));
        data.setQueryParams(queryParams);

        List<Upstream> upstreams = Arrays.asList(normalUpstream, grayUpstream);
        Upstream result = grayLoadBalancer.select(upstreams, data);
        assertEquals("gray-upstream", result.getUrl());
    }

    @Test
    public void testIpConditionMatch() {
        GrayCondition condition = new GrayCondition("ip", "", "=", "192.168.1.100");
        GrayConfig config = new GrayConfig(Collections.singletonList(condition), 100, "random", VERSION_V2);

        LoadBalanceData data = buildLoadBalanceData(config);
        data.setIp("192.168.1.100");

        List<Upstream> upstreams = Arrays.asList(normalUpstream, grayUpstream);
        Upstream result = grayLoadBalancer.select(upstreams, data);
        assertEquals("gray-upstream", result.getUrl());
    }

    // ========== Operators ==========

    @Test
    public void testRegexOperator() {
        GrayCondition condition = new GrayCondition("ip", "", "regex", "192\\.168\\..*");
        GrayConfig config = new GrayConfig(Collections.singletonList(condition), 100, "random", VERSION_V2);

        LoadBalanceData data = buildLoadBalanceData(config);
        data.setIp("192.168.1.55");

        List<Upstream> upstreams = Arrays.asList(normalUpstream, grayUpstream);
        Upstream result = grayLoadBalancer.select(upstreams, data);
        assertEquals("gray-upstream", result.getUrl());
    }

    @Test
    public void testMatchOperator() {
        // "match" uses contains() semantics — substring match should succeed
        GrayCondition condition = new GrayCondition("header", "X-Env", "match", "canary");
        GrayConfig config = new GrayConfig(Collections.singletonList(condition), 100, "random", VERSION_V2);

        LoadBalanceData data = buildLoadBalanceData(config);
        Map<String, Collection<String>> headers = new HashMap<>();
        headers.put("X-Env", Collections.singletonList("canary-v2"));
        data.setHeaders(headers);

        List<Upstream> upstreams = Arrays.asList(normalUpstream, grayUpstream);
        Upstream result = grayLoadBalancer.select(upstreams, data);
        assertEquals("gray-upstream", result.getUrl());
    }

    @Test
    public void testContainsOperator() {
        GrayCondition condition = new GrayCondition("header", "User-Agent", "contains", "Canary");
        GrayConfig config = new GrayConfig(Collections.singletonList(condition), 100, "random", VERSION_V2);

        LoadBalanceData data = buildLoadBalanceData(config);
        Map<String, Collection<String>> headers = new HashMap<>();
        headers.put("User-Agent", Collections.singletonList("Mozilla/5.0 CanaryBuild"));
        data.setHeaders(headers);

        List<Upstream> upstreams = Arrays.asList(normalUpstream, grayUpstream);
        Upstream result = grayLoadBalancer.select(upstreams, data);
        assertEquals("gray-upstream", result.getUrl());
    }

    @Test
    public void testExcludeOperator() {
        GrayCondition condition = new GrayCondition("header", "X-Env", "exclude", "prod");
        GrayConfig config = new GrayConfig(Collections.singletonList(condition), 100, "random", VERSION_V2);

        LoadBalanceData data = buildLoadBalanceData(config);
        Map<String, Collection<String>> headers = new HashMap<>();
        headers.put("X-Env", Collections.singletonList("staging"));
        data.setHeaders(headers);

        List<Upstream> upstreams = Arrays.asList(normalUpstream, grayUpstream);
        Upstream result = grayLoadBalancer.select(upstreams, data);
        assertEquals("gray-upstream", result.getUrl());
    }

    @Test
    public void testExcludeOperatorWithSubstring() {
        // "exclude" uses !contains() — "production" contains "prod" so exclude should NOT match
        GrayCondition condition = new GrayCondition("header", "X-Env", "exclude", "prod");
        GrayConfig config = new GrayConfig(Collections.singletonList(condition), 100, "random", VERSION_V2);

        LoadBalanceData data = buildLoadBalanceData(config);
        Map<String, Collection<String>> headers = new HashMap<>();
        headers.put("X-Env", Collections.singletonList("production"));
        data.setHeaders(headers);

        List<Upstream> upstreams = Arrays.asList(normalUpstream, grayUpstream);
        Upstream result = grayLoadBalancer.select(upstreams, data);
        assertEquals("normal-upstream", result.getUrl());
    }

    @Test
    public void testStartsWithOperator() {
        GrayCondition condition = new GrayCondition("header", "X-Version", "startsWith", "v2");
        GrayConfig config = new GrayConfig(Collections.singletonList(condition), 100, "random", VERSION_V2);

        LoadBalanceData data = buildLoadBalanceData(config);
        Map<String, Collection<String>> headers = new HashMap<>();
        headers.put("X-Version", Collections.singletonList("v2.1.0"));
        data.setHeaders(headers);

        List<Upstream> upstreams = Arrays.asList(normalUpstream, grayUpstream);
        Upstream result = grayLoadBalancer.select(upstreams, data);
        assertEquals("gray-upstream", result.getUrl());
    }

    @Test
    public void testEndsWithOperator() {
        GrayCondition condition = new GrayCondition("header", "X-Build", "endsWith", "-canary");
        GrayConfig config = new GrayConfig(Collections.singletonList(condition), 100, "random", VERSION_V2);

        LoadBalanceData data = buildLoadBalanceData(config);
        Map<String, Collection<String>> headers = new HashMap<>();
        headers.put("X-Build", Collections.singletonList("build-123-canary"));
        data.setHeaders(headers);

        List<Upstream> upstreams = Arrays.asList(normalUpstream, grayUpstream);
        Upstream result = grayLoadBalancer.select(upstreams, data);
        assertEquals("gray-upstream", result.getUrl());
    }

    @Test
    public void testIsBlankOperator() {
        GrayCondition condition = new GrayCondition("header", "X-Skip", "isBlank", "");
        GrayConfig config = new GrayConfig(Collections.singletonList(condition), 100, "random", VERSION_V2);

        LoadBalanceData data = buildLoadBalanceData(config);
        Map<String, Collection<String>> headers = new HashMap<>();
        headers.put("X-Skip", Collections.singletonList("  "));
        data.setHeaders(headers);

        List<Upstream> upstreams = Arrays.asList(normalUpstream, grayUpstream);
        Upstream result = grayLoadBalancer.select(upstreams, data);
        assertEquals("gray-upstream", result.getUrl());
    }

    @Test
    public void testIsBlankOperatorWhenHeaderAbsent() {
        GrayCondition condition = new GrayCondition("header", "X-Missing", "isBlank", "");
        GrayConfig config = new GrayConfig(Collections.singletonList(condition), 100, "random", VERSION_V2);

        LoadBalanceData data = buildLoadBalanceData(config);
        data.setHeaders(new HashMap<>());

        List<Upstream> upstreams = Arrays.asList(normalUpstream, grayUpstream);
        Upstream result = grayLoadBalancer.select(upstreams, data);
        assertEquals("gray-upstream", result.getUrl());
    }

    // ========== Header case-insensitive ==========

    @Test
    public void testHeaderCaseInsensitiveMatch() {
        GrayCondition condition = new GrayCondition("header", "x-canary", "=", "true");
        GrayConfig config = new GrayConfig(Collections.singletonList(condition), 100, "random", VERSION_V2);

        LoadBalanceData data = buildLoadBalanceData(config);
        Map<String, Collection<String>> headers = new HashMap<>();
        headers.put("X-Canary", Collections.singletonList("true"));
        data.setHeaders(headers);

        List<Upstream> upstreams = Arrays.asList(normalUpstream, grayUpstream);
        Upstream result = grayLoadBalancer.select(upstreams, data);
        assertEquals("gray-upstream", result.getUrl());
    }

    // ========== Multiple conditions (OR) ==========

    @Test
    public void testConditionsOrRelationship() {
        GrayCondition cond1 = new GrayCondition("header", "X-Canary", "=", "true");
        GrayCondition cond2 = new GrayCondition("header", "X-Gray", "=", "true");
        GrayConfig config = new GrayConfig(Arrays.asList(cond1, cond2), 100, "random", VERSION_V2);

        LoadBalanceData data = buildLoadBalanceData(config);
        Map<String, Collection<String>> headers = new HashMap<>();
        // only second condition matches
        headers.put("X-Gray", Collections.singletonList("true"));
        data.setHeaders(headers);

        List<Upstream> upstreams = Arrays.asList(normalUpstream, grayUpstream);
        Upstream result = grayLoadBalancer.select(upstreams, data);
        assertEquals("gray-upstream", result.getUrl());
    }

    // ========== MetadataMatch ==========

    @Test
    public void testMetadataMatchRoutesToMatchingUpstreams() {
        Upstream upA = Upstream.builder().url("10.0.0.1:8080").protocol("http")
                .metadata(Map.of("version", "v2")).weight(50).status(true).gray(true).build();
        Upstream upB = Upstream.builder().url("10.0.0.2:8080").protocol("http")
                .metadata(Map.of("version", "v1")).weight(50).status(true).gray(false).build();
        Upstream upC = Upstream.builder().url("10.0.0.3:8080").protocol("http")
                .metadata(Map.of("version", "v2")).weight(50).status(true).gray(true).build();
        List<Upstream> list = Arrays.asList(upA, upB, upC);

        MetadataMatch mm = new MetadataMatch("version", "v2");
        GrayCondition cond = new GrayCondition("header", "X-Canary", "=", "true");
        GrayConfig config = new GrayConfig(Collections.singletonList(cond), 100, "random", mm);

        LoadBalanceData data = buildLoadBalanceData(config);
        Map<String, Collection<String>> headers = new HashMap<>();
        headers.put("X-Canary", Collections.singletonList("true"));
        data.setHeaders(headers);

        Set<String> selected = new HashSet<>();
        for (int i = 0; i < 100; i++) {
            Upstream result = grayLoadBalancer.select(list, data);
            selected.add(result.getUrl());
        }
        assertTrue(selected.contains("10.0.0.1:8080"));
        assertTrue(selected.contains("10.0.0.3:8080"));
        assertFalse(selected.contains("10.0.0.2:8080"));
    }

    @Test
    public void testMetadataMatchFallsBackToNormalWhenNoConditionMatch() {
        Upstream upA = Upstream.builder().url("10.0.0.1:8080").protocol("http")
                .metadata(Map.of("version", "v2")).weight(50).status(true).gray(true).build();
        Upstream upB = Upstream.builder().url("10.0.0.2:8080").protocol("http")
                .metadata(Map.of("version", "v1")).weight(50).status(true).gray(false).build();
        List<Upstream> list = Arrays.asList(upA, upB);

        MetadataMatch mm = new MetadataMatch("version", "v2");
        GrayCondition cond = new GrayCondition("header", "X-Canary", "=", "true");
        GrayConfig config = new GrayConfig(Collections.singletonList(cond), 100, "random", mm);

        // No X-Canary header → condition not matched → route to normal
        LoadBalanceData data = buildLoadBalanceData(config);

        Set<String> selected = new HashSet<>();
        for (int i = 0; i < 100; i++) {
            Upstream result = grayLoadBalancer.select(list, data);
            selected.add(result.getUrl());
        }
        assertTrue(selected.contains("10.0.0.2:8080"));
        assertFalse(selected.contains("10.0.0.1:8080"));
    }

    @Test
    public void testNoMetadataMatchUsesGrayPool() {
        List<Upstream> upstreams = Arrays.asList(normalUpstream, grayUpstream);

        GrayCondition cond = new GrayCondition("header", "X-Canary", "=", "true");
        GrayConfig config = new GrayConfig(Collections.singletonList(cond), 100, "random");

        LoadBalanceData data = buildLoadBalanceData(config);
        Map<String, Collection<String>> headers = new HashMap<>();
        headers.put("X-Canary", Collections.singletonList("true"));
        data.setHeaders(headers);

        Upstream result = grayLoadBalancer.select(upstreams, data);
        assertEquals("gray-upstream", result.getUrl());
    }

    // ========== Fallback ==========

    @Test
    public void testFallbackWhenNoGrayUpstream() {
        GrayCondition condition = new GrayCondition("header", "X-Canary", "=", "true");
        GrayConfig config = new GrayConfig(Collections.singletonList(condition), 100, "random", VERSION_V2);

        LoadBalanceData data = buildLoadBalanceData(config);
        Map<String, Collection<String>> headers = new HashMap<>();
        headers.put("X-Canary", Collections.singletonList("true"));
        data.setHeaders(headers);

        // only normalUpstream (version=v1), no metadata match → grayList empty → fallback
        List<Upstream> upstreams = Collections.singletonList(normalUpstream);
        Upstream result = grayLoadBalancer.select(upstreams, data);
        assertEquals("normal-upstream", result.getUrl());
    }

    @Test
    public void testDelegateLoadBalanceRecursionGuard() {
        // loadBalance set to "gray" should fall back to "random" to avoid recursion
        GrayCondition condition = new GrayCondition("header", "X-Canary", "=", "true");
        GrayConfig config = new GrayConfig(Collections.singletonList(condition), 100, "gray", VERSION_V2);

        LoadBalanceData data = buildLoadBalanceData(config);
        Map<String, Collection<String>> headers = new HashMap<>();
        headers.put("X-Canary", Collections.singletonList("true"));
        data.setHeaders(headers);

        List<Upstream> upstreams = Arrays.asList(normalUpstream, grayUpstream);
        Upstream result = grayLoadBalancer.select(upstreams, data);
        assertEquals("gray-upstream", result.getUrl());
    }

    private LoadBalanceData buildLoadBalanceData(final GrayConfig config) {
        LoadBalanceData data = new LoadBalanceData();
        Map<String, Object> attributes = new HashMap<>();
        attributes.put(GrayLoadBalancer.GRAY_CONFIG_KEY, config);
        data.setAttributes(attributes);
        return data;
    }
}
