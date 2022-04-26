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

package org.apache.shenyu.web.handler;

import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.PluginHandlerEventEnums;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.base.cache.PluginHandlerEvent;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.mock;

/**
 * test for ShenyuWebHandler.
 *
 */
@ExtendWith(MockitoExtension.class)
public final class ShenyuWebHandlerTest {

    @Mock
    private ShenyuWebHandler shenyuWebHandler;
    
    private final List<ShenyuPlugin> listPlugins = new ArrayList<>();

    @BeforeEach
    public void setUp() {
        listPlugins.add(new Test1Plugin());
        listPlugins.add(new Test2Plugin());
        shenyuWebHandler = new ShenyuWebHandler(listPlugins, new ShenyuConfig());
    }

    @Test
    public void handle() {
        final ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost")
                .remoteAddress(new InetSocketAddress(8090))
                .build());
        exchange.getAttributes().put(Constants.CONTEXT, mock(ShenyuContext.class));
        exchange.getAttributes().put(Constants.PARAM_TRANSFORM, "{key:value}");
        Mono<Void> handle = shenyuWebHandler.handle(exchange);
        assertNotNull(handle);
    }

    @Test
    public void testOnApplicationEvent() {
        PluginData pluginData = PluginData.builder().id("1")
                .name("test1-plugin")
                .enabled(true)
                .config("config")
                .role("test")
                .sort(50)
                .build();
        PluginData pluginDataPlugin2 = PluginData.builder().id("2")
                .name("test2-plugin")
                .enabled(true)
                .config("config")
                .role("test")
                .sort(40)
                .build();
        PluginHandlerEvent enabledEvent = new PluginHandlerEvent(PluginHandlerEventEnums.ENABLED, pluginData);
        shenyuWebHandler.onApplicationEvent(enabledEvent);
        List<ShenyuPlugin> enabledEventPlugin = (List<ShenyuPlugin>) ReflectionTestUtils.getField(shenyuWebHandler, "plugins");
        Set<ShenyuPlugin> enabledPlugins = (Set<ShenyuPlugin>) ReflectionTestUtils.getField(shenyuWebHandler, "enabledPlugins");
        assertNotNull(enabledEventPlugin);
        assertNotNull(enabledPlugins);
        assertEquals(enabledPlugins.size(), enabledEventPlugin.size());

        PluginHandlerEvent disabledEvent = new PluginHandlerEvent(PluginHandlerEventEnums.DISABLED, pluginData);
        shenyuWebHandler.onApplicationEvent(disabledEvent);
        List<ShenyuPlugin> disabledEventPlugin = (List<ShenyuPlugin>) ReflectionTestUtils.getField(shenyuWebHandler, "plugins");
        Set<ShenyuPlugin> disabledPlugins = (Set<ShenyuPlugin>) ReflectionTestUtils.getField(shenyuWebHandler, "enabledPlugins");
        assertNotNull(disabledEventPlugin);
        assertNotNull(disabledPlugins);
        assertEquals(disabledEventPlugin.size(), disabledPlugins.size());

        PluginHandlerEvent deletedEvent = new PluginHandlerEvent(PluginHandlerEventEnums.DELETE, pluginData);
        shenyuWebHandler.onApplicationEvent(deletedEvent);
        List<ShenyuPlugin> deletedEventPlugin = (List<ShenyuPlugin>) ReflectionTestUtils.getField(shenyuWebHandler, "plugins");
        Set<ShenyuPlugin> deletedPlugins = (Set<ShenyuPlugin>) ReflectionTestUtils.getField(shenyuWebHandler, "enabledPlugins");
        assertNotNull(deletedEventPlugin);
        assertNotNull(deletedPlugins);
        assertEquals(deletedEventPlugin.size(), deletedPlugins.size());
    }

    static class Test1Plugin implements ShenyuPlugin {

        @Override
        public Mono<Void> execute(ServerWebExchange exchange, ShenyuPluginChain chain) {
            return chain.execute(exchange);
        }

        @Override
        public int getOrder() {
            return 1;
        }

        @Override
        public String named() {
            return "test1-plugin";
        }

        @Override
        public boolean skip(ServerWebExchange exchange) {
            return ShenyuPlugin.super.skip(exchange);
        }
    }

    static class Test2Plugin implements ShenyuPlugin {

        @Override
        public Mono<Void> execute(ServerWebExchange exchange, ShenyuPluginChain chain) {
            return chain.execute(exchange);
        }

        @Override
        public int getOrder() {
            return 2;
        }

        @Override
        public String named() {
            return "test2-plugin";
        }

        @Override
        public boolean skip(ServerWebExchange exchange) {
            return ShenyuPlugin.super.skip(exchange);
        }
    }
}
