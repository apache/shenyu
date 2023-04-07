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

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContextBuilder;
import org.apache.shenyu.plugin.api.context.ShenyuContextDecorator;
import org.apache.shenyu.plugin.global.fixture.FixtureHttpShenyuContextDecorator;
import org.apache.shenyu.plugin.global.fixture.FixtureWebSocketShenyuContextDecorator;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;

import java.net.InetSocketAddress;
import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.springframework.http.HttpHeaders.UPGRADE;

/**
 * The Test Case For GlobalPlugin.
 */
@ExtendWith(MockitoExtension.class)
public final class GlobalPluginTest {

    private ShenyuPluginChain chain;

    private GlobalPlugin globalPlugin;

    private ServerWebExchange exchange;

    @BeforeEach
    public void setUp() {
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest.get("http://localhost:8080/http")
                .remoteAddress(new InetSocketAddress(8091))
                .header(UPGRADE, "Upgrade")
                .build());
        Map<String, ShenyuContextDecorator> decoratorMap = new HashMap<>();
        decoratorMap.put(RpcTypeEnum.HTTP.getName(), new FixtureHttpShenyuContextDecorator());
        decoratorMap.put(RpcTypeEnum.WEB_SOCKET.getName(), new FixtureWebSocketShenyuContextDecorator());
        ShenyuContextBuilder builder = new DefaultShenyuContextBuilder(decoratorMap);
        this.globalPlugin = new GlobalPlugin(builder);
        this.chain = mock(ShenyuPluginChain.class);
    }

    @Test
    public void testExecuted() {
        this.globalPlugin.execute(this.exchange, this.chain);
        assertNotNull(this.exchange.getAttributes().get(Constants.CONTEXT));
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest.get("http://localhost:8080/http")
                .remoteAddress(new InetSocketAddress(8091))
                .header(UPGRADE, "websocket")
                .build());
        this.globalPlugin.execute(this.exchange, this.chain);
        assertNotNull(this.exchange.getAttributes().get(Constants.CONTEXT));
    }

    @Test
    public void testGetOrder() {
        assertEquals(-1, globalPlugin.getOrder());
    }

    @Test
    public void testNamed() {
        assertEquals(PluginEnum.GLOBAL.getName(), globalPlugin.named());
    }
}
