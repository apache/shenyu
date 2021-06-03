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

import lombok.SneakyThrows;
import org.apache.shenyu.plugin.global.fixture.FixtureShenyuContextDecorator;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.context.ShenyuContextBuilder;
import org.apache.shenyu.plugin.api.context.ShenyuContextDecorator;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.server.ServerWebExchange;

import java.lang.reflect.Method;
import java.net.InetSocketAddress;
import java.util.HashMap;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.springframework.http.HttpHeaders.UPGRADE;

/**
 * The Test Case For GlobalPlugin.
 */
@RunWith(MockitoJUnitRunner.class)
public final class GlobalPluginTest {

    private ShenyuPluginChain chain;

    private GlobalPlugin globalPlugin;

    private ServerWebExchange exchange;

    @Before
    public void setUp() {
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest.get("http://localhost:8080/http")
                .remoteAddress(new InetSocketAddress(8091))
                .header(UPGRADE, "Upgrade")
                .build());
        Map<String, ShenyuContextDecorator> decoratorMap = new HashMap<>();
        decoratorMap.put("http", new FixtureShenyuContextDecorator());
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

    @SneakyThrows
    @Test
    public void testTransformMap() {
        Class<GlobalPlugin> clazz = GlobalPlugin.class;
        Method method = clazz.getDeclaredMethod("transformMap", MultiValueMap.class);
        method.setAccessible(true);
        MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>(1);
        queryParams.add(Constants.MODULE, "module");
        queryParams.add(Constants.METHOD, "method");
        queryParams.add(Constants.RPC_TYPE, "RPC_TYPE");
        ShenyuContext shenyuContext = (ShenyuContext) method.invoke(this.globalPlugin, queryParams);
        assertEquals(shenyuContext.getModule(), "module");
        assertEquals(shenyuContext.getMethod(), "method");
        assertEquals(shenyuContext.getRpcType(), "RPC_TYPE");
    }

    @Test
    public void testGetOrder() {
        assertEquals(globalPlugin.getOrder(), 1);
    }

    @Test
    public void testNamed() {
        assertEquals(PluginEnum.GLOBAL.getName(), globalPlugin.named());
    }
}
