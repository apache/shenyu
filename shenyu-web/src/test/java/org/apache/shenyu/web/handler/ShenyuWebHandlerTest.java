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
import org.apache.shenyu.common.enums.PluginHandlerEventEnum;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.base.cache.PluginHandlerEvent;
import org.apache.shenyu.web.loader.ShenyuLoaderService;
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
import reactor.test.StepVerifier;

import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

/**
 * test for ShenyuWebHandler.
 *
 */
@ExtendWith(MockitoExtension.class)
public final class ShenyuWebHandlerTest {

    @Mock
    private ShenyuWebHandler shenyuWebHandler;

    private ShenyuLoaderService shenyuLoaderService;

    private final List<ShenyuPlugin> listPlugins = new ArrayList<>();

    private final ShenyuPlugin plugin1 = new TestPlugin1();

    private final ShenyuPlugin plugin2 = new TestPlugin2();

    @BeforeEach
    public void setUp() {
        listPlugins.add(plugin1);
        listPlugins.add(plugin2);
        shenyuLoaderService = mock(ShenyuLoaderService.class);
        shenyuWebHandler = new ShenyuWebHandler(listPlugins, shenyuLoaderService, new ShenyuConfig());
    }

    @Test
    public void handle() {
        final ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost")
                .remoteAddress(new InetSocketAddress(8090))
                .build());
        exchange.getAttributes().put(Constants.CONTEXT, mock(ShenyuContext.class));
        exchange.getAttributes().put(Constants.PARAM_TRANSFORM, "{key:value}");
        Mono<Void> handle = shenyuWebHandler.handle(exchange);
        StepVerifier.create(handle).expectSubscription().verifyComplete();

    }

    @Test
    public void putExtPlugins() {
        shenyuWebHandler.putExtPlugins(Collections.emptyList());
        shenyuWebHandler.putExtPlugins(Collections.singletonList(new TestPlugin2()));
        shenyuWebHandler.putExtPlugins(Collections.singletonList(new TestPlugin3()));
    }

    @Test
    public void scheduledEnableTest() {
        final ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost")
                .remoteAddress(new InetSocketAddress(8090))
                .build());
        ShenyuConfig shenyuConfig = new ShenyuConfig();
        shenyuConfig.getScheduler().setEnabled(true);
        ShenyuWebHandler shenyuWebHandler1 = new ShenyuWebHandler(listPlugins, shenyuLoaderService, shenyuConfig);
        Mono<Void> handle = shenyuWebHandler1.handle(exchange);
        assertNotNull(handle);
        shenyuConfig.getScheduler().setType("elastic");
        ShenyuWebHandler shenyuWebHandler2 = new ShenyuWebHandler(listPlugins, shenyuLoaderService, shenyuConfig);
        Mono<Void> handle2 = shenyuWebHandler2.handle(exchange);
        assertNotNull(handle2);
    }

    @Test
    public void testOnApplicationEvent() {
        PluginData pluginData1 = PluginData.builder().id("1")
                .name("test-plugin1")
                .enabled(true)
                .config("config")
                .role("test")
                .sort(50)
                .build();
        PluginData pluginData2 = PluginData.builder().id("2")
                .name("test-plugin2")
                .enabled(false)
                .config("config")
                .role("test")
                .sort(60)
                .build();
        shenyuWebHandler.onApplicationEvent(new PluginHandlerEvent(PluginHandlerEventEnum.ENABLED, pluginData1));
        shenyuWebHandler.onApplicationEvent(new PluginHandlerEvent(PluginHandlerEventEnum.DISABLED, pluginData2));
        List<ShenyuPlugin> plugins = (List<ShenyuPlugin>) ReflectionTestUtils.getField(shenyuWebHandler, "plugins");
        assertNotNull(plugins);
        assertTrue(plugins.contains(plugin1) && !plugins.contains(plugin2));

        shenyuWebHandler.onApplicationEvent(new PluginHandlerEvent(PluginHandlerEventEnum.ENABLED, pluginData1));
        shenyuWebHandler.onApplicationEvent(new PluginHandlerEvent(PluginHandlerEventEnum.DELETE, pluginData2));
        List<ShenyuPlugin> pluginDelete = (List<ShenyuPlugin>) ReflectionTestUtils.getField(shenyuWebHandler, "plugins");
        assertNotNull(pluginDelete);
        assertTrue(pluginDelete.contains(plugin1) && !pluginDelete.contains(plugin2));

        pluginData1.setSort(70);
        pluginData2.setEnabled(true);
        BaseDataCache.getInstance().cachePluginData(pluginData1);
        BaseDataCache.getInstance().cachePluginData(pluginData2);
        shenyuWebHandler.onApplicationEvent(new PluginHandlerEvent(PluginHandlerEventEnum.ENABLED, pluginData1));
        shenyuWebHandler.onApplicationEvent(new PluginHandlerEvent(PluginHandlerEventEnum.ENABLED, pluginData2));
        List<ShenyuPlugin> pluginDataSorted = (List<ShenyuPlugin>) ReflectionTestUtils.getField(shenyuWebHandler, "plugins");
        assertNotNull(pluginDataSorted);
        assertEquals(pluginDataSorted.get(0), plugin2);
        assertEquals(pluginDataSorted.get(1), plugin1);

        shenyuWebHandler.onApplicationEvent(new PluginHandlerEvent(PluginHandlerEventEnum.SORTED, pluginData1));
        assertEquals(pluginDataSorted.get(0), plugin2);
    }

    static class TestPlugin1 implements ShenyuPlugin {

        @Override
        public Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
            return chain.execute(exchange);
        }

        @Override
        public int getOrder() {
            return 1;
        }

        @Override
        public String named() {
            return "test-plugin1";
        }

        @Override
        public boolean skip(final ServerWebExchange exchange) {
            return ShenyuPlugin.super.skip(exchange);
        }
    }

    static class TestPlugin2 implements ShenyuPlugin {

        @Override
        public Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
            return chain.execute(exchange);
        }

        @Override
        public int getOrder() {
            return 2;
        }

        @Override
        public String named() {
            return "test-plugin2";
        }

        @Override
        public boolean skip(final ServerWebExchange exchange) {
            return true;
        }
    }

    static class TestPlugin3 implements ShenyuPlugin {

        @Override
        public Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
            return chain.execute(exchange);
        }

        @Override
        public int getOrder() {
            return 3;
        }

        @Override
        public String named() {
            return "test-plugin3";
        }

        @Override
        public boolean skip(final ServerWebExchange exchange) {
            return ShenyuPlugin.super.skip(exchange);
        }
    }
}
