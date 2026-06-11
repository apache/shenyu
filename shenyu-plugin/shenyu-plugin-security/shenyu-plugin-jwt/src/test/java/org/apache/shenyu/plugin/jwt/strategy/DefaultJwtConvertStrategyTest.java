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

package org.apache.shenyu.plugin.jwt.strategy;

import com.google.common.collect.ImmutableMap;
import org.apache.shenyu.plugin.jwt.rule.DefaultJwtRuleHandle;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;

import java.util.Map;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class DefaultJwtConvertStrategyTest {

    private DefaultJwtConvertStrategy defaultJwtConvertStrategy;

    private ServerWebExchange exchange;

    private Map<String, Object> jwtBody;

    @BeforeEach
    public void setUp() {
        defaultJwtConvertStrategy = new DefaultJwtConvertStrategy();

        exchange = MockServerWebExchange.from(MockServerHttpRequest
                .get("localhost")
                .build());

        jwtBody = ImmutableMap.of("sub", "12345",
                "multi", ImmutableMap.of("web", "shenyu"));

    }

    @Test
    public void testParseHandleJson() {
        String handleJson = "{\"converter\":[{\"jwtVal\":\"sub\",\"headerVal\":\"id\"}]}";
        assertThat(defaultJwtConvertStrategy.parseHandleJson(handleJson), notNullValue(DefaultJwtRuleHandle.class));
        assertThat(defaultJwtConvertStrategy.parseHandleJson(null), nullValue());
    }

    @Test
    public void testConvert() {
        String handleJson = "{\"converter\":[{\"jwtVal\":\"sub\",\"headerVal\":\"id\"}]}";
        DefaultJwtRuleHandle defaultJwtRuleHandle = defaultJwtConvertStrategy.parseHandleJson(handleJson);
        ServerWebExchange newExchange = defaultJwtConvertStrategy
                .convert(defaultJwtRuleHandle, exchange, jwtBody);

        assertTrue(newExchange.getRequest().getHeaders().get("id").contains(jwtBody.get("sub")));

    }

    @Test
    public void testExecuteWithWrongHandleJson() {

        String wrongHandleJson = "{\"wrongConverter\":[{\"jwtVal\":\"sub\",\"headerVal\":\"id\"}]}";
        DefaultJwtRuleHandle defaultJwtRuleHandle = defaultJwtConvertStrategy.parseHandleJson(wrongHandleJson);

        ServerWebExchange newExchange = defaultJwtConvertStrategy
                .convert(defaultJwtRuleHandle, exchange, jwtBody);

        assertEquals(newExchange, exchange);

    }

    @Test
    public void testExecuteWithWrongConverter() {

        String wrongHandleJson = "{\"converter\":[{\"jwtVal \":\"sub\",\"headerVal \":\"id\"}]}";
        DefaultJwtRuleHandle defaultJwtRuleHandle = defaultJwtConvertStrategy.parseHandleJson(wrongHandleJson);

        ServerWebExchange newExchange = defaultJwtConvertStrategy
                .convert(defaultJwtRuleHandle, exchange, jwtBody);

        assertEquals(newExchange.getRequest().getHeaders(), exchange.getRequest().getHeaders());

    }

    @Test
    public void testMulti() {
        String handleJson = "{\"converter\":[{\"jwtVal\":\"multi.web\",\"headerVal\":\"web\"}]}";

        DefaultJwtRuleHandle defaultJwtRuleHandle = defaultJwtConvertStrategy
                .parseHandleJson(handleJson);

        ServerWebExchange newExchange = defaultJwtConvertStrategy
                .convert(defaultJwtRuleHandle, exchange, jwtBody);

        assertTrue(newExchange.getRequest().getHeaders().get("web").contains("shenyu"));
    }

    /**
     * Test to verify the fix for the duplicate header bug.
     * After the fix, when jwtVal contains ".", only one header should be added with the parsed nested value.
     */
    @Test
    public void testNestedJwtValueHandling() {
        // Setup JWT body with nested structure
        Map<String, Object> nestedJwtBody = ImmutableMap.of(
                "user", ImmutableMap.of("name", "john", "role", "admin"),
                "simple", "simpleValue"
        );

        // Test case 1: JWT value with dot notation (should work correctly after fix)
        String handleJsonWithDot = "{\"converter\":[{\"jwtVal\":\"user.name\",\"headerVal\":\"username\"}]}";
        DefaultJwtRuleHandle ruleHandleWithDot = defaultJwtConvertStrategy.parseHandleJson(handleJsonWithDot);
        
        ServerWebExchange exchangeWithDot = defaultJwtConvertStrategy
                .convert(ruleHandleWithDot, exchange, nestedJwtBody);
        
        // Check the headers - after fix, should only have one header value
        var headersWithDot = exchangeWithDot.getRequest().getHeaders().get("username");
        
        // After fix: should only have one header value with the correct parsed value
        assertEquals(1, headersWithDot.size(), "After fix: Header should only be added once");
        assertEquals("john", headersWithDot.get(0), "Header value should be parsed correctly from nested structure");

        // Test case 2: JWT value without dot notation (should continue to work correctly)
        String handleJsonWithoutDot = "{\"converter\":[{\"jwtVal\":\"simple\",\"headerVal\":\"simpleheader\"}]}";
        DefaultJwtRuleHandle ruleHandleWithoutDot = defaultJwtConvertStrategy.parseHandleJson(handleJsonWithoutDot);
        
        ServerWebExchange exchangeWithoutDot = defaultJwtConvertStrategy
                .convert(ruleHandleWithoutDot, exchange, nestedJwtBody);
        
        var headersWithoutDot = exchangeWithoutDot.getRequest().getHeaders().get("simpleheader");
        
        // This should continue to work correctly - only one header value
        assertEquals(1, headersWithoutDot.size(), "Simple values should only add one header");
        assertEquals("simpleValue", headersWithoutDot.get(0), "Header value should be correct");
    }

    /**
     * Test multiple converters with mixed dot notation and simple values.
     */
    @Test
    public void testMultipleConvertersWithMixedNotation() {
        Map<String, Object> complexJwtBody = ImmutableMap.of(
                "user", ImmutableMap.of("name", "alice", "profile", ImmutableMap.of("email", "alice@example.com")),
                "role", "user",
                "permissions", ImmutableMap.of("read", true, "write", false)
        );

        String handleJson = "{\"converter\":["
                + "{\"jwtVal\":\"user.name\",\"headerVal\":\"X-User-Name\"},"
                + "{\"jwtVal\":\"role\",\"headerVal\":\"X-User-Role\"},"
                + "{\"jwtVal\":\"user.profile.email\",\"headerVal\":\"X-User-Email\"},"
                + "{\"jwtVal\":\"permissions.read\",\"headerVal\":\"X-Can-Read\"}"
                + "]}";

        DefaultJwtRuleHandle ruleHandle = defaultJwtConvertStrategy.parseHandleJson(handleJson);
        ServerWebExchange newExchange = defaultJwtConvertStrategy.convert(ruleHandle, exchange, complexJwtBody);

        // Verify all headers are added correctly
        var headers = newExchange.getRequest().getHeaders();
        
        assertEquals(1, headers.get("X-User-Name").size());
        assertEquals("alice", headers.get("X-User-Name").get(0));
        
        assertEquals(1, headers.get("X-User-Role").size());
        assertEquals("user", headers.get("X-User-Role").get(0));
        
        assertEquals(1, headers.get("X-User-Email").size());
        assertEquals("alice@example.com", headers.get("X-User-Email").get(0));
        
        assertEquals(1, headers.get("X-Can-Read").size());
        assertEquals("true", headers.get("X-Can-Read").get(0));
    }

}
