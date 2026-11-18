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
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.websocket.handler.WebSocketPluginDataHandler;
import org.apache.shenyu.plugin.websocket.handler.WebSocketUpstreamDataHandler;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedStatic;
import org.reactivestreams.Publisher;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.reactive.socket.CloseStatus;
import org.springframework.web.reactive.socket.WebSocketHandler;
import org.springframework.web.reactive.socket.WebSocketMessage;
import org.springframework.web.reactive.socket.WebSocketSession;
import org.springframework.web.reactive.socket.client.WebSocketClient;
import org.springframework.web.reactive.socket.server.WebSocketService;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.net.InetSocketAddress;
import java.time.Duration;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * The rule's {@code timeout} bounds the upstream websocket handshake and stops once the
 * upgrade succeeds; {@code idleTimeout} bounds an established session that moves no
 * message in either direction (#8474).
 */
public class WebSocketPluginHandshakeTimeoutTest {

    private static final String SELECTOR_ID = "mock-timeout";

    private static final Duration VERIFY_BOUND = Duration.ofSeconds(2);

    private static final long HANDSHAKE_DELAY_MILLIS = 50L;

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
        when(selectorData.getId()).thenReturn(SELECTOR_ID);
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
    public void stalledHandshakeIsClosedWithinTheRuleTimeout() {
        initRule(100L, 0L);
        when(webSocketClient.execute(any(), any(), any())).thenReturn(Mono.never());
        WebSocketSession downstream = mockSession();
        answerHandshakeWith(downstream);

        StepVerifier.create(execute())
                .expectComplete()
                .verify(VERIFY_BOUND);

        final CloseStatus status = captureClose(downstream);
        assertEquals(CloseStatus.SERVER_ERROR.getCode(), status.getCode());
        assertTrue(status.getReason().contains("handshake"));
    }

    @Test
    public void establishedIdleSessionOutlivesTheHandshakeTimeout() {
        // the upstream upgrade succeeds and the session stays silent: the handshake deadline
        // must be inert from then on, so the session is never closed at a fixed age
        initRule(500L, 0L);
        WebSocketSession upstream = mockSession();
        answerHandshakeAfterUpgrade(upstream);
        WebSocketSession downstream = mockSession();
        answerHandshakeWith(downstream);

        StepVerifier.create(execute())
                .expectSubscription()
                .expectNoEvent(Duration.ofMillis(1200))
                .thenCancel()
                .verify();

        verify(downstream, never()).close(any(CloseStatus.class));
    }

    @Test
    public void idleSessionIsClosedWithGoingAwayAfterTheIdleTimeout() {
        initRule(2000L, 200L);
        WebSocketSession upstream = mockSession();
        answerHandshakeAfterUpgrade(upstream);
        WebSocketSession downstream = mockSession();
        answerHandshakeWith(downstream);

        StepVerifier.create(execute())
                .expectComplete()
                .verify(VERIFY_BOUND);

        final CloseStatus status = captureClose(downstream);
        assertEquals(CloseStatus.GOING_AWAY.getCode(), status.getCode());
        assertTrue(status.getReason().contains("idle"));
    }

    @Test
    public void trafficInEitherDirectionKeepsTheSessionAlive() {
        initRule(2000L, 300L);
        WebSocketSession upstream = mockSession();
        answerHandshakeAfterUpgrade(upstream);
        WebSocketSession downstream = mockSession();
        when(downstream.receive()).thenReturn(Flux.interval(Duration.ofMillis(50))
                .map(tick -> mock(WebSocketMessage.class)));
        answerHandshakeWith(downstream);

        StepVerifier.create(execute())
                .expectSubscription()
                .expectNoEvent(Duration.ofMillis(800))
                .thenCancel()
                .verify();

        verify(downstream, never()).close(any(CloseStatus.class));
    }

    @Test
    public void zeroIdleTimeoutDisablesIdleExpiry() {
        initRule(2000L, 0L);
        WebSocketSession upstream = mockSession();
        answerHandshakeAfterUpgrade(upstream);
        WebSocketSession downstream = mockSession();
        answerHandshakeWith(downstream);

        StepVerifier.create(execute())
                .expectSubscription()
                .expectNoEvent(Duration.ofMillis(400))
                .thenCancel()
                .verify();

        verify(downstream, never()).close(any(CloseStatus.class));
    }

    @Test
    public void nonPositiveHandshakeTimeoutDisablesTheDeadline() {
        initRule(0L, 0L);
        when(webSocketClient.execute(any(), any(), any())).thenReturn(Mono.never());
        WebSocketSession downstream = mockSession();
        answerHandshakeWith(downstream);

        // no deadline: the stalled handshake must not error or close within the window
        StepVerifier.create(execute())
                .expectSubscription()
                .expectNoEvent(Duration.ofMillis(300))
                .thenCancel()
                .verify();

        verify(downstream, never()).close(any(CloseStatus.class));
    }

    private Mono<Void> execute() {
        return Mono.defer(() -> webSocketPlugin.doExecute(exchange, mockChain(), selectorData, ruleData));
    }

    private void answerHandshakeWith(final WebSocketSession downstream) {
        when(webSocketService.handleRequest(any(), any())).thenAnswer(call ->
                ((WebSocketHandler) call.getArgument(1)).handle(downstream));
    }

    private void answerHandshakeAfterUpgrade(final WebSocketSession upstream) {
        when(webSocketClient.execute(any(), any(), any())).thenAnswer(call -> {
            final WebSocketHandler inner = call.getArgument(2);
            return Mono.delay(Duration.ofMillis(HANDSHAKE_DELAY_MILLIS))
                    .then(Mono.defer(() -> inner.handle(upstream)));
        });
    }

    @SuppressWarnings("unchecked")
    private WebSocketSession mockSession() {
        final WebSocketSession session = mock(WebSocketSession.class);
        when(session.isOpen()).thenReturn(true);
        when(session.receive()).thenReturn(Flux.never());
        when(session.send(any())).thenAnswer(call ->
                Flux.from((Publisher<WebSocketMessage>) call.getArgument(0)).then());
        when(session.closeStatus()).thenReturn(Mono.never());
        when(session.close(any(CloseStatus.class))).thenReturn(Mono.empty());
        return session;
    }

    private CloseStatus captureClose(final WebSocketSession session) {
        final ArgumentCaptor<CloseStatus> captor = ArgumentCaptor.forClass(CloseStatus.class);
        verify(session).close(captor.capture());
        return captor.getValue();
    }

    private void initRule(final long timeout, final long idleTimeout) {
        WebSocketRuleHandle handle = new WebSocketRuleHandle();
        handle.setTimeout(timeout);
        handle.setIdleTimeout(idleTimeout);
        when(ruleData.getHandle()).thenReturn(GsonUtils.getGson().toJson(handle));
        final DiscoverySyncData discoverySyncData = mock(DiscoverySyncData.class);
        when(discoverySyncData.getSelectorId()).thenReturn(SELECTOR_ID);
        when(discoverySyncData.getUpstreamDataList()).thenReturn(Stream.of(3)
                .map(weight -> DiscoveryUpstreamData.builder().url("mock-" + weight).build())
                .collect(Collectors.toList()));
        final WebSocketUpstreamDataHandler upstreamDataHandler = new WebSocketUpstreamDataHandler();
        upstreamDataHandler.handlerDiscoveryUpstreamData(discoverySyncData);
        final WebSocketPluginDataHandler dataHandler = new WebSocketPluginDataHandler();
        dataHandler.handlerRule(ruleData);
        dataHandler.handlerSelector(selectorData);
    }

    private ShenyuPluginChain mockChain() {
        return mock(ShenyuPluginChain.class);
    }
}
