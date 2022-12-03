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

}
