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

import org.junit.jupiter.api.Test;

import java.net.URI;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for LoadBalanceData.
 */
class LoadBalanceDataTest {

    @Test
    void defaultInstanceUsesSharedEmptyCollections() {
        LoadBalanceData data = new LoadBalanceData();
        assertEquals("GET", data.getHttpMethod());
        assertEquals("127.0.0.1", data.getIp());
        assertTrue(data.getHeaders().isEmpty());
        assertTrue(data.getCookies().isEmpty());
        assertTrue(data.getAttributes().isEmpty());
        assertTrue(data.getQueryParams().isEmpty());
        LoadBalanceData another = new LoadBalanceData();
        assertSame(data.getHeaders(), another.getHeaders());
        assertSame(data.getCookies(), another.getCookies());
        assertSame(data.getAttributes(), another.getAttributes());
        assertSame(data.getQueryParams(), another.getQueryParams());
    }

    @Test
    void fullConstructorKeepsProvidedValues() {
        Map<String, Collection<String>> headers = Collections.singletonMap("key", Collections.singletonList("value"));
        Map<String, String> cookies = Collections.singletonMap("cookie", "value");
        Map<String, Object> attributes = Collections.singletonMap("attribute", new Object());
        Map<String, Collection<String>> queryParams = Collections.singletonMap("param", Collections.singletonList("value"));
        LoadBalanceData data = new LoadBalanceData("POST", "1.2.3.4", URI.create("http://localhost"),
                headers, cookies, attributes, queryParams);
        assertEquals("POST", data.getHttpMethod());
        assertEquals("1.2.3.4", data.getIp());
        assertEquals(URI.create("http://localhost"), data.getUrl());
        assertSame(headers, data.getHeaders());
        assertSame(cookies, data.getCookies());
        assertSame(attributes, data.getAttributes());
        assertSame(queryParams, data.getQueryParams());
    }
}
