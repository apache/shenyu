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
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.WebSocketRuleHandle;
import org.apache.shenyu.common.dto.convert.selector.DivideUpstream;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.UpstreamCheckUtils;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.websocket.handler.WebSocketPluginDataHandler;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.springframework.http.HttpHeaders;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.reactive.socket.client.WebSocketClient;
import org.springframework.web.reactive.socket.server.WebSocketService;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.InetSocketAddress;
import java.net.URI;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;
import static org.springframework.http.HttpHeaders.CONNECTION;
import static org.springframework.http.HttpHeaders.UPGRADE;

/**
 * The type websocket plugin test.
 */
public class WebSocketPluginTest {

    private RuleData ruleData;

    private ShenyuPluginChain chain;

    private SelectorData selectorData;

    private ServerWebExchange exchange;

    private List<DivideUpstream> divideUpstreamList;

    private WebSocketPlugin webSocketPlugin;

    private WebSocketService webSocketService;

    private MockedStatic<UpstreamCheckUtils> mockCheckUtils;

    @BeforeEach
    public void setup() {
        this.ruleData = mock(RuleData.class);
        this.chain = mock(ShenyuPluginChain.class);
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

        // mock static
        mockCheckUtils = mockStatic(UpstreamCheckUtils.class);
        mockCheckUtils.when(() -> UpstreamCheckUtils.checkUrl(anyString(), anyInt())).thenReturn(true);
    }

    @AfterEach
    public void tearDown() {
        mockCheckUtils.close();
    }

    /**
     * Websocket plugin doExecute.
     */
    @Test
    public void doExecuteTest() {
        initMockInfo();
        when(webSocketService.handleRequest(any(), any())).thenReturn(Mono.empty());
        StepVerifier.create(Mono.defer(() -> webSocketPlugin.doExecute(exchange, chain, selectorData, ruleData))).expectSubscription().verifyComplete();
        SelectorData selectorData1 = new SelectorData();
        selectorData1.setId("1");
        assertEquals(webSocketPlugin.doExecute(exchange, chain, selectorData1, new RuleData()), chain.execute(exchange));
    }

    /**
     * Skip.
     */
    @Test
    public void skip() {
        initMockInfo();
        assertTrue(webSocketPlugin.skip(exchange));
    }

    /**
     * Named default value test case.
     */
    @Test
    public void namedTest() {
        assertEquals(PluginEnum.WEB_SOCKET.getName(), webSocketPlugin.named());
    }

    /**
     * GetOrder default value test case.
     */
    @Test
    public void getOrderTest() {
        assertEquals(PluginEnum.WEB_SOCKET.getCode(), webSocketPlugin.getOrder());
    }

    @Test
    public void getSubProtocolsTest() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException, InstantiationException {
        Class<?>[] clazz = webSocketPlugin.getClass().getDeclaredClasses();
        Class<?> cla = clazz[0];
        Method method = cla.getDeclaredMethod("getSubProtocols");
        method.setAccessible(true);
        Constructor declaredConstructor = cla.getDeclaredConstructor(URI.class, WebSocketClient.class, HttpHeaders.class, List.class);
        declaredConstructor.setAccessible(true);
        Object obj = declaredConstructor.newInstance(null, null, null, null);
        List<String> list = (List<String>) method.invoke(obj);
        assertEquals(list.isEmpty(), true);
    }

    /**
     * Init mock info.
     */
    private void initMockInfo() {
        ShenyuContext context = mock(ShenyuContext.class);
        context.setRpcType(RpcTypeEnum.WEB_SOCKET.getName());
        WebSocketRuleHandle handle = new WebSocketRuleHandle();
        when(selectorData.getId()).thenReturn("mock");
        when(selectorData.getHandle()).thenReturn(GsonUtils.getGson().toJson(divideUpstreamList));
        when(ruleData.getHandle()).thenReturn(GsonUtils.getGson().toJson(handle));
        WebSocketPluginDataHandler webSocketPluginDataHandler = new WebSocketPluginDataHandler();
        webSocketPluginDataHandler.handlerSelector(selectorData);
        webSocketPluginDataHandler.handlerRule(ruleData);
        exchange.getAttributes().put(Constants.CONTEXT, context);
    }
}
