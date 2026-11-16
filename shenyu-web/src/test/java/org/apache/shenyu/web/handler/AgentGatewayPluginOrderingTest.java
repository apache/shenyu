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
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.PluginHandlerEventEnum;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.base.cache.PluginHandlerEvent;
import org.apache.shenyu.web.loader.ShenyuLoaderService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;

/**
 * Real handler/cache ordering with lightweight plugins; not protocol integration tests.
 */
class AgentGatewayPluginOrderingTest {

    private static final List<PluginEnum> PLUGINS = Arrays.asList(PluginEnum.AI_PROXY, PluginEnum.AI_TOKEN_LIMITER,
            PluginEnum.AI_PROMPT, PluginEnum.MCP_SERVER, PluginEnum.AGENT_GATEWAY, PluginEnum.KEY_AUTH);

    private final Map<String, PluginData> previous = new HashMap<>();

    private final List<String> executed = new ArrayList<>();

    @BeforeEach
    void isolatePluginCache() {
        PLUGINS.forEach(plugin -> {
            previous.put(plugin.getName(), BaseDataCache.getInstance().obtainPluginData(plugin.getName()));
            BaseDataCache.getInstance().removePluginDataByPluginName(plugin.getName());
        });
    }

    @AfterEach
    void restorePluginCache() {
        PLUGINS.forEach(plugin -> {
            BaseDataCache.getInstance().removePluginDataByPluginName(plugin.getName());
            BaseDataCache.getInstance().cachePluginData(previous.get(plugin.getName()));
        });
    }

    @Test
    void shouldUseEnumOrderWithoutCachedConfiguration() {
        assertOrder(handler(false), "keyAuth", "agentGateway", "aiPrompt", "aiTokenLimiter", "mcpServer", "aiProxy");
    }

    @Test
    void shouldUseCachedDefaultsWhenPluginsAreEnabled() {
        PLUGINS.forEach(plugin -> cache(plugin, plugin.getCode()));
        ShenyuWebHandler handler = handler(false);
        handler.onApplicationEvent(new PluginHandlerEvent(PluginHandlerEventEnum.ENABLED, data(PluginEnum.AGENT_GATEWAY, PluginEnum.AGENT_GATEWAY.getCode())));
        assertOrder(handler, "keyAuth", "agentGateway", "aiPrompt", "aiTokenLimiter", "mcpServer", "aiProxy");
    }

    @Test
    void shouldNotSilentlyRepairAnUnmigratedCachedGateway() {
        cache(PluginEnum.AGENT_GATEWAY, 198);
        assertOrder(handler(false), "keyAuth", "aiPrompt", "aiTokenLimiter", "mcpServer", "agentGateway", "aiProxy");
    }

    @Test
    void shouldRetainExplicitCustomOrdersEvenWhenUnsafe() {
        cache(PluginEnum.AGENT_GATEWAY, 149);
        cache(PluginEnum.AI_PROXY, 148);
        assertOrder(handler(false), "aiProxy", "agentGateway", "keyAuth", "aiPrompt", "aiTokenLimiter", "mcpServer");
    }

    @Test
    void shouldApplyAnAdminSortEventAndFallBackAfterCacheRemoval() {
        ShenyuWebHandler handler = handler(false);
        cache(PluginEnum.AGENT_GATEWAY, 198);
        handler.onApplicationEvent(new PluginHandlerEvent(PluginHandlerEventEnum.SORTED, data(PluginEnum.AGENT_GATEWAY, 198)));
        assertOrder(handler, "keyAuth", "aiPrompt", "aiTokenLimiter", "mcpServer", "agentGateway", "aiProxy");
        cache(PluginEnum.AGENT_GATEWAY, PluginEnum.AGENT_GATEWAY.getCode());
        handler.onApplicationEvent(new PluginHandlerEvent(PluginHandlerEventEnum.SORTED, data(PluginEnum.AGENT_GATEWAY, PluginEnum.AGENT_GATEWAY.getCode())));
        assertOrder(handler, "keyAuth", "agentGateway", "aiPrompt", "aiTokenLimiter", "mcpServer", "aiProxy");
        BaseDataCache.getInstance().removePluginDataByPluginName(PluginEnum.AGENT_GATEWAY.getName());
        handler.onApplicationEvent(new PluginHandlerEvent(PluginHandlerEventEnum.SORTED, data(PluginEnum.AGENT_GATEWAY, 198)));
        assertOrder(handler, "keyAuth", "agentGateway", "aiPrompt", "aiTokenLimiter", "mcpServer", "aiProxy");
    }

    @Test
    void shouldContinueFromGatewayThroughTheExistingAiChain() {
        verifyChain(handler(false));
        assertEquals(Arrays.asList("keyAuth", "agentGateway", "aiPrompt", "aiTokenLimiter", "mcpServer", "aiProxy"), executed);
    }

    @Test
    void shouldNotReachLaterPluginsWhenGatewayTerminates() {
        verifyChain(handler(true));
        assertEquals(Arrays.asList("keyAuth", "agentGateway"), executed);
    }

    @Test
    void shouldLeaveTheOriginalAiChainWhenGatewayIsDisabled() {
        ShenyuWebHandler handler = handler(false);
        handler.onApplicationEvent(new PluginHandlerEvent(PluginHandlerEventEnum.DISABLED, data(PluginEnum.AGENT_GATEWAY, PluginEnum.AGENT_GATEWAY.getCode())));
        verifyChain(handler);
        assertEquals(Arrays.asList("keyAuth", "aiPrompt", "aiTokenLimiter", "mcpServer", "aiProxy"), executed);
    }

    private ShenyuWebHandler handler(final boolean gatewayTerminates) {
        List<ShenyuPlugin> plugins = PLUGINS.stream().map(plugin -> new RecordingPlugin(plugin, gatewayTerminates))
                .collect(Collectors.toList());
        ShenyuWebHandler handler = new ShenyuWebHandler(plugins, mock(ShenyuLoaderService.class), new ShenyuConfig());
        handler.onApplicationEvent(new PluginHandlerEvent(PluginHandlerEventEnum.SORTED, data(PluginEnum.AGENT_GATEWAY, PluginEnum.AGENT_GATEWAY.getCode())));
        return handler;
    }

    private void assertOrder(final ShenyuWebHandler handler, final String... names) {
        assertEquals(Arrays.asList(names), handler.getPlugins().stream().map(ShenyuPlugin::named).collect(Collectors.toList()));
    }

    private void verifyChain(final ShenyuWebHandler handler) {
        StepVerifier.create(handler.handle(MockServerWebExchange.from(MockServerHttpRequest.post("/agent")))).verifyComplete();
    }

    private void cache(final PluginEnum plugin, final int sort) {
        BaseDataCache.getInstance().cachePluginData(data(plugin, sort));
    }

    private PluginData data(final PluginEnum plugin, final int sort) {
        return PluginData.builder().name(plugin.getName()).sort(sort).enabled(true).build();
    }

    private final class RecordingPlugin implements ShenyuPlugin {

        private final PluginEnum plugin;

        private final boolean gatewayTerminates;

        private RecordingPlugin(final PluginEnum plugin, final boolean gatewayTerminates) {
            this.plugin = plugin;
            this.gatewayTerminates = gatewayTerminates;
        }

        @Override
        public Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
            executed.add(named());
            if (gatewayTerminates && plugin == PluginEnum.AGENT_GATEWAY) {
                return Mono.empty();
            }
            return chain.execute(exchange);
        }

        @Override
        public int getOrder() {
            return plugin.getCode();
        }

        @Override
        public String named() {
            return plugin.getName();
        }
    }
}
