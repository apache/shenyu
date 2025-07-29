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

package org.apache.shenyu.plugin.ai.proxy;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.AiProxyHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.common.strategy.AiModel;
import org.apache.shenyu.plugin.ai.proxy.handler.AiProxyPluginHandler;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.http.codec.HttpMessageReader;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class AiProxyPluginTest {

    private AiProxyPlugin plugin;

    private List<HttpMessageReader<?>> messageReaders;

    @BeforeEach
    void setUp() {
        messageReaders = mock(List.class);
        plugin = new AiProxyPlugin(messageReaders);
    }

    @Test
    void testDoExecuteWithValidConfig() {

        ShenyuContext shenyuContext = new ShenyuContext();
        ServerWebExchange exchange = mock(ServerWebExchange.class);
        when(exchange.getAttribute(Constants.CONTEXT)).thenReturn(shenyuContext);

        AiCommonConfig config = new AiCommonConfig();
        config.setBaseUrl("https://api.example.com");
        config.setProvider("OPEN_AI");
        Singleton.INST.single(AiCommonConfig.class, config);

        AiModel aiModel = mock(AiModel.class);
        when(aiModel.invoke(any(), any(), any(), any())).thenReturn(Mono.empty());
        AiProxyPluginHandler.SELECTOR_CACHED_HANDLE.get().cachedHandle("selector-id_default_rule", new AiProxyHandle());

        SelectorData selector = mock(SelectorData.class);
        RuleData rule = mock(RuleData.class);
        ShenyuPluginChain chain = mock(ShenyuPluginChain.class);
        Mono<Void> result = plugin.doExecute(exchange, chain, selector, rule);

        assertNotNull(result);
        verify(aiModel, times(0)).invoke(any(), any(), any(), any());
    }

    @Test
    void testDoExecuteWithValidContext() {

        ShenyuContext shenyuContext = new ShenyuContext();
        ServerWebExchange exchange = mock(ServerWebExchange.class);
        when(exchange.getAttribute(Constants.CONTEXT)).thenReturn(shenyuContext);

        AiCommonConfig config = new AiCommonConfig();
        config.setBaseUrl("https://api.example.com");
        Singleton.INST.single(AiCommonConfig.class, config);

        AiModel aiModel = mock(AiModel.class);
        when(aiModel.invoke(any(), any(), any(), any())).thenReturn(Mono.empty());
        AiProxyPluginHandler.SELECTOR_CACHED_HANDLE.get().cachedHandle("selector-id_default_rule", new AiProxyHandle());

        ShenyuPluginChain chain = mock(ShenyuPluginChain.class);
        SelectorData selector = mock(SelectorData.class);
        RuleData rule = mock(RuleData.class);

        Mono<Void> result = plugin.doExecute(exchange, chain, selector, rule);

        assertNotNull(result);
        verify(chain, times(0)).execute(exchange);
    }

    @Test
    void testDoExecuteWithInvalidProvider() {
        ServerWebExchange exchange = mock(ServerWebExchange.class);
        ShenyuPluginChain chain = mock(ShenyuPluginChain.class);
        SelectorData selector = mock(SelectorData.class);
        RuleData rule = mock(RuleData.class);

        AiCommonConfig config = new AiCommonConfig();
        config.setProvider("INVALID_PROVIDER");
        Singleton.INST.single(AiCommonConfig.class, config);

        assertThrows(NullPointerException.class, () -> plugin.doExecute(exchange, chain, selector, rule));
    }

    @Test
    void testGetOrder() {
        assertEquals(PluginEnum.AI_PROXY.getCode(), plugin.getOrder());
    }

    @Test
    void testNamed() {
        assertEquals(PluginEnum.AI_PROXY.getName(), plugin.named());
    }
}
