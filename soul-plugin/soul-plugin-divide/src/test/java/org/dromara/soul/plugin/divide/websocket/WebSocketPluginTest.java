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

package org.dromara.soul.plugin.divide.websocket;

import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.dto.convert.DivideUpstream;
import org.dromara.soul.common.dto.convert.rule.RuleHandleFactory;
import org.dromara.soul.common.dto.convert.rule.impl.DivideRuleHandle;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.plugin.api.SoulPluginChain;
import org.dromara.soul.plugin.api.context.SoulContext;
import org.dromara.soul.plugin.divide.cache.UpstreamCacheManager;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.reactive.socket.client.WebSocketClient;
import org.springframework.web.reactive.socket.server.WebSocketService;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.net.InetSocketAddress;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.springframework.http.HttpHeaders.CONNECTION;
import static org.springframework.http.HttpHeaders.UPGRADE;

/**
 * The type websocket plugin test.
 *
 * @author zhanglei
 */
public class WebSocketPluginTest {

    private RuleData ruleData;

    private SoulPluginChain chain;

    private SelectorData selectorData;

    private ServerWebExchange exchange;

    private List<DivideUpstream> divideUpstreamList;

    private WebSocketPlugin webSocketPlugin;

    private WebSocketService webSocketService;

    @Before
    public void setup() {
        this.ruleData = mock(RuleData.class);
        this.chain = mock(SoulPluginChain.class);
        this.selectorData = mock(SelectorData.class);
        this.divideUpstreamList = Stream.of(3)
                .map(weight -> DivideUpstream.builder()
                        .upstreamUrl("mock-" + weight)
                        .build())
                .collect(Collectors.toList());
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost")
                .remoteAddress(new InetSocketAddress(8090))
                .header(UPGRADE, "WebSocket")
                .header(CONNECTION, "upgrade")
                .header(com.google.common.net.HttpHeaders.SEC_WEBSOCKET_KEY, "websocket")
                .build());
        WebSocketClient webSocketClient = mock(WebSocketClient.class);
        this.webSocketService = mock(WebSocketService.class);
        this.webSocketPlugin = new WebSocketPlugin(webSocketClient, webSocketService);
    }

    /**
     * Websocket plugin doExecute.
     */
    @Test
    public void doExecuteTest() {
        initMockInfo();
        when(webSocketService.handleRequest(any(), any())).thenReturn(Mono.empty());
        StepVerifier.create(Mono.defer(() -> webSocketPlugin.doExecute(exchange, chain, selectorData, ruleData))).expectSubscription().verifyComplete();
    }

    /**
     * Skip.
     */
    @Test
    public void skip() {
        initMockInfo();
        Assert.assertTrue(webSocketPlugin.skip(exchange));
    }

    /**
     * Named default value test case.
     */
    @Test
    public void namedTest() {
        Assert.assertEquals(PluginEnum.DIVIDE.getName(), webSocketPlugin.named());
    }

    /**
     * GetOrder default value test case.
     */
    @Test
    public void getOrderTest() {
        Assert.assertEquals(PluginEnum.WEB_SOCKET.getCode(), webSocketPlugin.getOrder());
    }

    /**
     * Init mock info.
     */
    private void initMockInfo() {
        SoulContext context = mock(SoulContext.class);
        context.setRpcType(RpcTypeEnum.HTTP.getName());
        DivideRuleHandle handle = (DivideRuleHandle) RuleHandleFactory.ruleHandle(RpcTypeEnum.HTTP, "");
        when(selectorData.getId()).thenReturn("mock");
        when(ruleData.getHandle()).thenReturn(GsonUtils.getGson().toJson(handle));
        when(selectorData.getHandle()).thenReturn(GsonUtils.getGson().toJson(divideUpstreamList));
        UpstreamCacheManager.getInstance().submit(selectorData);
        exchange.getAttributes().put(Constants.CONTEXT, context);
    }
}
