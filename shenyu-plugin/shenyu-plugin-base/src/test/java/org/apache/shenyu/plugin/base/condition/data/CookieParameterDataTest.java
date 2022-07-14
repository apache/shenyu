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

package org.apache.shenyu.plugin.base.condition.data;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpCookie;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;

import java.net.InetSocketAddress;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Test cases for {@link CookieParameterData}.
 */
public final class CookieParameterDataTest {

    private ServerWebExchange exchange;

    private CookieParameterData parameterData;

    @BeforeEach
    public void setUp() {
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/http")
                .remoteAddress(new InetSocketAddress("localhost", 8080))
                .cookie(new HttpCookie("cookie-name", "cookie-value"))
                .build());
        this.parameterData = new CookieParameterData();
    }

    @Test
    public void testBuilder() {
        assertEquals("", parameterData.builder("invalid-cookie-name", exchange));
        assertEquals("cookie-value", parameterData.builder("cookie-name", exchange));
    }
}
