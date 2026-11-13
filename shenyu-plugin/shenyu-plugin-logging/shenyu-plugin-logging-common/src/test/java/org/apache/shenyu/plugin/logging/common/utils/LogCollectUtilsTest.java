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

package org.apache.shenyu.plugin.logging.common.utils;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.logging.common.config.GenericGlobalConfig;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.http.HttpHeaders;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;

import java.net.InetSocketAddress;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * The Test Case For LogCollectUtils.
 */
public class LogCollectUtilsTest {
    
    private ServerHttpRequest request;

    @BeforeEach
    public void setUp() {
        MockServerHttpRequest request = MockServerHttpRequest
                .get("localhost")
                .remoteAddress(new InetSocketAddress(8090))
                .header("X-source", "mock test")
                .queryParam("queryParam", "Hello,World")
                .build();
        ServerWebExchange exchange = Mockito.spy(MockServerWebExchange.from(request));
        ShenyuContext shenyuContext = Mockito.mock(ShenyuContext.class);
        exchange.getAttributes().put(Constants.CONTEXT, shenyuContext);
        this.request = exchange.getRequest();
    }

    @Test
    public void testIsNotBinaryType() {
        assertTrue(LogCollectUtils.isNotBinaryType(request.getHeaders()));
    }

    @Test
    public void testGetHeaders() {
        assertEquals(LogCollectUtils.getHeaders(request.getHeaders()), "{\"X-source\":\"mock test\"}");
    }

    @Test
    public void testDefaultCredentialsAreMaskedWithoutChangingHeaders() {
        HttpHeaders headers = new HttpHeaders();
        for (String name : Set.of("Authorization", "PROXY-authorization", "cOoKiE", "Set-Cookie", "ShenYu-Authorization", "token")) {
            headers.add(name, "secret-one");
            headers.add(name, "secret-two");
        }
        headers.add("X-Source", "one");
        headers.add("X-Source", "two");
        Map<?, ?> logged = GsonUtils.getInstance().fromJson(LogCollectUtils.getHeaders(headers), Map.class);
        headers.forEach((name, values) -> {
            assertEquals("X-Source".equals(name) ? "one,two" : "***", logged.get(name));
            assertEquals(2, values.size());
        });
        assertEquals("secret-one", headers.getFirst("Authorization"));
        assertEquals("{}", LogCollectUtils.getHeaders(new HttpHeaders()));
    }

    @Test
    public void testAdditionalHeadersAndDisabledMasking() {
        GenericGlobalConfig config = new GenericGlobalConfig();
        config.setMaskedHeaders(new HashSet<>(Arrays.asList(null, " ", " X-Api-Key ")));
        HttpHeaders headers = new HttpHeaders();
        headers.set("x-API-key", "custom-secret");
        headers.set("Authorization", "credential");
        Set<String> policy = LogCollectUtils.getMaskedHeaders(config);
        config.setMaskedHeaders(Set.of("other"));
        Map<?, ?> masked = GsonUtils.getInstance().fromJson(LogCollectUtils.getHeaders(headers, policy), Map.class);
        assertEquals("***", masked.get("x-API-key"));
        assertEquals("***", masked.get("Authorization"));
        config.setMaskCredentialHeaders(false);
        Map<?, ?> plain = GsonUtils.getInstance().fromJson(LogCollectUtils.getHeaders(headers, LogCollectUtils.getMaskedHeaders(config)), Map.class);
        assertEquals("credential", plain.get("Authorization"));
        assertEquals("custom-secret", plain.get("x-API-key"));
    }

    @Test
    public void testMissingAndNullJsonSettingsKeepSecureDefaults() {
        for (String json : Set.of("{}", "{\"maskCredentialHeaders\":null,\"maskedHeaders\":null}", "{\"maskedHeaders\":[]}")) {
            GenericGlobalConfig config = GsonUtils.getInstance().fromJson(json, GenericGlobalConfig.class);
            assertTrue(LogCollectUtils.getMaskedHeaders(config).contains("authorization"));
        }
    }

}
