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

package org.dromara.soul.plugin.sentinel;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import static org.junit.Assert.assertEquals;

import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.plugin.api.SoulPluginChain;
import org.dromara.soul.plugin.api.context.SoulContext;
import org.dromara.soul.plugin.sentinel.fallback.SentinelFallbackHandler;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.net.InetSocketAddress;

@RunWith(MockitoJUnitRunner.class)
public class SentinelPluginTest {

    private SentinelPlugin sentinelPlugin;

    private ServerWebExchange exchange;

    private SelectorData selectorData;

    @Mock
    private SentinelFallbackHandler sentinelFallbackHandler;

    @Mock
    private SoulPluginChain chain;

    @Before
    public void setUp() {
        this.chain = mock(SoulPluginChain.class);
        this.selectorData = mock(SelectorData.class);
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost")
                .remoteAddress(new InetSocketAddress(8090))
                .build());
        sentinelFallbackHandler = new SentinelFallbackHandler();
        SoulContext context = mock(SoulContext.class);
        context.setRpcType(RpcTypeEnum.HTTP.getName());
        exchange.getAttributes().put(Constants.CONTEXT, context);
        when(chain.execute(exchange)).thenReturn(Mono.empty());
        sentinelPlugin = new SentinelPlugin(sentinelFallbackHandler);
    }

    @Test
    public void testSentinelPlugin() {
        RuleData data = mock(RuleData.class);
        when(data.getSelectorId()).thenReturn("1");
        when(data.getName()).thenReturn("sentinel");

        when(chain.execute(exchange)).thenReturn(Mono.empty());
        StepVerifier.create(sentinelPlugin.doExecute(exchange, chain, selectorData, data))
                .expectSubscription()
                .verifyError();
    }

    @Test
    public void testNamed() {
        final String result = sentinelPlugin.named();
        assertEquals(PluginEnum.SENTINEL.getName(), result);
    }

    @Test
    public void testGetOrder() {
        final int result = sentinelPlugin.getOrder();
        assertEquals(PluginEnum.SENTINEL.getCode(), result);
    }
}
