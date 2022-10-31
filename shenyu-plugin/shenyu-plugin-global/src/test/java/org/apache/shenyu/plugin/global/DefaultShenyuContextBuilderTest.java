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

package org.apache.shenyu.plugin.global;

import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.context.ShenyuContextDecorator;
import org.apache.shenyu.plugin.global.fixture.FixtureHttpShenyuContextDecorator;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;

import java.net.InetSocketAddress;
import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * The Test Case For DefaultShenyuContextBuilder.
 */
public final class DefaultShenyuContextBuilderTest {

    private DefaultShenyuContextBuilder defaultShenyuContextBuilder;

    @BeforeEach
    public void setUp() {
        Map<String, ShenyuContextDecorator> decoratorMap = new HashMap<>();
        decoratorMap.put("http", new FixtureHttpShenyuContextDecorator());
        defaultShenyuContextBuilder = new DefaultShenyuContextBuilder(decoratorMap);
    }

    @Test
    public void testBuild() {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("http://localhost:8080/http")
                .remoteAddress(new InetSocketAddress(8092))
                .header("MetaDataCache", "Hello")
                .build());
        ShenyuContext shenyuContext = defaultShenyuContextBuilder.build(exchange);
        assertNotNull(shenyuContext);
        assertEquals(RpcTypeEnum.HTTP.getName(), shenyuContext.getRpcType());
    }
}
