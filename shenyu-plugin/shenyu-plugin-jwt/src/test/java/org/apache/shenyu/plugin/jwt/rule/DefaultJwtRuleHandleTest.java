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

package org.apache.shenyu.plugin.jwt.rule;

import com.google.common.collect.ImmutableMap;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;

import java.util.HashMap;
import java.util.Map;

/**
 * Test case for {@link DefaultJwtRuleHandle}.
 */
public class DefaultJwtRuleHandleTest {

    private DefaultJwtRuleHandle defaultJwtRuleHandle;

    private ServerWebExchange exchange;

    private Map<String, Object> jwtBody;

    @BeforeEach
    public void setUp() {

        defaultJwtRuleHandle = new DefaultJwtRuleHandle();

        exchange = MockServerWebExchange.from(MockServerHttpRequest
                .get("localhost")
                .build());

        jwtBody = new HashMap<>();

        jwtBody.put("sub", "12345");
        jwtBody.put("multi", ImmutableMap.of("web", "shenyu"));

    }

    @Test
    public void testInit() {
        String expression = "{\"converter\":[{\"jwtVal\":\"sub\",\"headerVal\":\"id\"}]}";

        defaultJwtRuleHandle.init(expression);

        Assertions.assertEquals(defaultJwtRuleHandle.toJson(), expression);

    }

    @Test
    public void testExecute() {
        String expression = "{\"converter\":[{\"jwtVal\":\"sub\",\"headerVal\":\"id\"}]}";

        defaultJwtRuleHandle.init(expression);

        ServerWebExchange newExchange = defaultJwtRuleHandle.execute(exchange, jwtBody);

        Assertions.assertTrue(newExchange.getRequest().getHeaders().get("id").contains(jwtBody.get("sub")));

    }

    @Test
    public void testExecuteWithWrongExpression() {

        String wrongExpression = "{\"wrongConverter\":[{\"jwtVal\":\"sub\",\"headerVal\":\"id\"}]}";

        defaultJwtRuleHandle.init(wrongExpression);

        ServerWebExchange newExchange = defaultJwtRuleHandle.execute(exchange, jwtBody);

        Assertions.assertEquals(newExchange, exchange);

    }

    @Test
    public void testExecuteWithoutInit() {

        ServerWebExchange newExchange = defaultJwtRuleHandle.execute(exchange, jwtBody);

        Assertions.assertEquals(newExchange, exchange);

    }

    @Test
    public void testMulti() {
        String expression = "{\"converter\":[{\"jwtVal\":\"multi.web\",\"headerVal\":\"web\"}]}";

        defaultJwtRuleHandle.init(expression);

        ServerWebExchange newExchange = defaultJwtRuleHandle.execute(exchange, jwtBody);

        Assertions.assertTrue(newExchange.getRequest().getHeaders().get("web").contains("shenyu"));
    }

}
