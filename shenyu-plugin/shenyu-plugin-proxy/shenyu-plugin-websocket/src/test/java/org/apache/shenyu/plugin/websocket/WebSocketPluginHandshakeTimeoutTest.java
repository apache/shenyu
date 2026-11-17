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

package org.apache.shenyu.plugin.websocket;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.WebSocketRuleHandle;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.UpstreamCheckUtils;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.websocket.handler.WebSocketUpstreamDataHandler;
import org.apache.shenyu.plugin.websocket.handler.WebSocketPluginDataHandler;

import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.reactive.socket.WebSocketHandler;
import org.springframework.web.reactive.socket.WebSocketSession;
import org.springframework.web.reactive.socket.client.WebSocketClient;
import org.springframework.web.reactive.socket.server.WebSocketService;
import org.springframework.web.server.ServerWebExchange;
import java.time.Duration;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.net.InetSocketAddress;
import java.util.concurrent.TimeoutException;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

/**
 * The rule's {@code timeout} bounds the upstream websocket handshake (#8474):
 * a dead upstream fails the upgrade in a rule-controlled time instead of
 * hanging. The established session is never bounded by this timer.
 */
public class WebSocketPluginHandshakeTimeoutTest {

    private RuleData ruleData;

    private SelectorData selectorData;

    private ServerWebExchange exchange;

    private WebSocketPlugin webSocketPlugin;

    private WebSocketClient webSocketClient;

    private WebSocketService webSocketService;

    private MockedStatic<UpstreamCheckUtils> mockCheckUtils;

    @BeforeEach
    public void setup() {
        this.ruleData = mock(RuleData.class);
        this.selectorData = mock(SelectorData.class);
        when(selectorData.getId()).thenReturn("mock-timeout");
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost")
                .remoteAddress(new InetSocketAddress(8090))
                .header("Upgrade", "WebSocket")
                .header("Connection", "upgrade")
                .header(com.google.common.net.HttpHeaders.SEC_WEBSOCKET_KEY, "websocket")
                .build());
        ShenyuContext context = mock(ShenyuContext.class);
        context.setRpcType(RpcTypeEnum.WEB_SOCKET.getName());
        exchange.getAttributes().put(Constants.CONTEXT, context);
        this.webSocketClient = mock(WebSocketClient.class);
        this.webSocketService = mock(WebSocketService.class);
        this.webSocketPlugin = new WebSocketPlugin(webSocketClient, webSocketService);
        mockCheckUtils = mockStatic(UpstreamCheckUtils.class);
        mockCheckUtils.when(() -> UpstreamCheckUtils.checkUrl(anyString(), anyInt())).thenReturn(true);
    }

    @AfterEach
    public void tearDown() {
        mockCheckUtils.close();
    }

    @Test
    public void stalledHandshakeFailsWithinTheRuleTimeout() {
        initRule(100L);
        when(webSocketClient.execute(any(), any(), any())).thenReturn(Mono.never());
        when(webSocketService.handleRequest(any(), any())).thenAnswer(call ->
                ((WebSocketHandler) call.getArgument(1)).handle(mock(WebSocketSession.class)));
        StepVerifier.create(Mono.defer(() -> webSocketPlugin.doExecute(exchange, mockChain(), selectorData, ruleData)))
                .expectError(TimeoutException.class)
                .verify(Duration.ofSeconds(2));
    }

    @Test
    public void nonPositiveTimeoutDisablesTheDeadline() {
        initRule(0L);
        when(webSocketClient.execute(any(), any(), any())).thenReturn(Mono.never());
        when(webSocketService.handleRequest(any(), any())).thenAnswer(call ->
                ((WebSocketHandler) call.getArgument(1)).handle(mock(WebSocketSession.class)));
        // no deadline: the stalled handshake must not error within the window
        StepVerifier.create(Mono.defer(() -> webSocketPlugin.doExecute(exchange, mockChain(), selectorData, ruleData)))
                .expectSubscription()
                .expectNoEvent(Duration.ofMillis(300))
                .thenCancel()
                .verify();
    }

    private void initRule(final long timeout) {
        WebSocketRuleHandle handle = new WebSocketRuleHandle();
        handle.setTimeout(timeout);
        when(ruleData.getHandle()).thenReturn(GsonUtils.getGson().toJson(handle));
        final DiscoverySyncData discoverySyncData = mock(DiscoverySyncData.class);
        when(discoverySyncData.getSelectorId()).thenReturn("mock-timeout");
        when(discoverySyncData.getUpstreamDataList()).thenReturn(Stream.of(3)
                .map(weight -> DiscoveryUpstreamData.builder().url("mock-" + weight).build())
                .collect(Collectors.toList()));
        final WebSocketUpstreamDataHandler upstreamDataHandler = new WebSocketUpstreamDataHandler();
        upstreamDataHandler.handlerDiscoveryUpstreamData(discoverySyncData);
        final WebSocketPluginDataHandler dataHandler = new WebSocketPluginDataHandler();
        dataHandler.handlerRule(ruleData);
        dataHandler.handlerSelector(selectorData);
    }

    private org.apache.shenyu.plugin.api.ShenyuPluginChain mockChain() {
        return mock(org.apache.shenyu.plugin.api.ShenyuPluginChain.class);
    }
}
