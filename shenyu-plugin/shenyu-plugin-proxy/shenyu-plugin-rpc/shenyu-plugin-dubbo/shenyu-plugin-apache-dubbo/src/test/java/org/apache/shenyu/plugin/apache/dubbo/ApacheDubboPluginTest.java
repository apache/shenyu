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

package org.apache.shenyu.plugin.apache.dubbo;

import com.google.common.collect.Maps;
import org.apache.dubbo.common.constants.CommonConstants;
import org.apache.dubbo.rpc.RpcContext;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.DubboRuleHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.apache.dubbo.handler.ApacheDubboPluginDataHandler;
import org.apache.shenyu.plugin.apache.dubbo.proxy.ApacheDubboProxyService;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.net.InetSocketAddress;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * The Test Case For ApachDubboPlugin.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class ApacheDubboPluginTest {

    private ApacheDubboPlugin apacheDubboPlugin;

    private MetaData metaData;

    private ServerWebExchange exchange;

    @Mock
    private ShenyuPluginChain chain;

    @Mock
    private ApacheDubboProxyService apacheDubboProxyService;

    @BeforeEach
    public void setUp() {
        exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost").build());
        metaData = new MetaData();
        metaData.setId("1332017966661636096");
        metaData.setAppName("dubbo");
        metaData.setPath("/dubbo/findAll");
        metaData.setServiceName("org.apache.shenyu.test.dubbo.api.service.DubboTestService");
        metaData.setMethodName("findAll");
        metaData.setRpcType(RpcTypeEnum.DUBBO.getName());
        apacheDubboPlugin = new ApacheDubboPlugin(apacheDubboProxyService);
    }

    @Test
    public void doExecute() {
        ShenyuContext context = mock(ShenyuContext.class);
        exchange.getAttributes().put(Constants.CONTEXT, context);
        exchange.getAttributes().put(Constants.PARAM_TRANSFORM, "{key:value}");
        exchange.getAttributes().put(Constants.META_DATA, metaData);
        SelectorData selectorData = mock(SelectorData.class);
        RuleData data = mock(RuleData.class);
        assertThrows(NullPointerException.class, () -> StepVerifier.create(apacheDubboPlugin.doExecute(exchange, chain, selectorData, data)).expectSubscription().verifyComplete());
    }

    @Test
    public void testParameterNotNullExecute() {
        ShenyuContext context = mock(ShenyuContext.class);
        exchange.getAttributes().put(Constants.CONTEXT, context);
        metaData.setParameterTypes("parameterTypes");
        exchange.getAttributes().put(Constants.META_DATA, metaData);
        SelectorData selectorData = mock(SelectorData.class);
        RuleData data = mock(RuleData.class);
        assertThrows(NullPointerException.class, () -> StepVerifier.create(apacheDubboPlugin.doExecute(exchange, chain, selectorData, data)).expectSubscription().verifyComplete());
    }

    @Test
    public void testMethodIsNullExecute() {
        ShenyuContext context = mock(ShenyuContext.class);
        exchange.getAttributes().put(Constants.CONTEXT, context);
        exchange.getAttributes().put(Constants.PARAM_TRANSFORM, "{key:value}");
        MetaData metaData = MetaData.builder()
                .id("1332017966661636096")
                .appName("dubbo")
                .path("/dubbo/findAll")
                .serviceName("org.apache.shenyu.test.dubbo.api.service.DubboTestService")
                .rpcType(RpcTypeEnum.DUBBO.getName())
                .build();
        exchange.getAttributes().put(Constants.META_DATA, metaData);
        SelectorData selectorData = mock(SelectorData.class);
        RuleData data = mock(RuleData.class);
        assertThrows(NullPointerException.class, () -> StepVerifier.create(apacheDubboPlugin.doExecute(exchange, chain, selectorData, data)).expectSubscription().verifyComplete());
    }

    @Test
    public void testNamed() {
        final String result = apacheDubboPlugin.named();
        assertEquals(PluginEnum.DUBBO.getName(), result);
    }

    @Test
    public void skip() {
        final ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost").build());
        ShenyuContext context = mock(ShenyuContext.class);
        when(context.getRpcType()).thenReturn(RpcTypeEnum.DUBBO.getName());
        exchange.getAttributes().put(Constants.CONTEXT, context);
        exchange.getAttributes().put(Constants.META_DATA, metaData);
        final boolean result = apacheDubboPlugin.skip(exchange);
        assertFalse(result);
    }

    @Test
    public void getOrder() {
        final int result = apacheDubboPlugin.getOrder();
        assertEquals(PluginEnum.DUBBO.getCode(), result);
    }

    @Test
    public void testTransmitRpcContext() {
        Map<String, String> stringStringMap = Maps.newHashMapWithExpectedSize(1);
        stringStringMap.put("test", "test");
        apacheDubboPlugin.transmitRpcContext(stringStringMap);
        assertEquals(RpcContext.getContext().getAttachment("test"), "test");
    }

    @Test
    public void testDoDubboInvokerWithNullRuleHandle() {
        ShenyuContext context = mock(ShenyuContext.class);
        ServerWebExchange webExchange = MockServerWebExchange.from(
                MockServerHttpRequest.get("localhost")
                        .remoteAddress(new InetSocketAddress("127.0.0.1", 8080))
                        .build());
        webExchange.getAttributes().put(Constants.CONTEXT, context);
        webExchange.getAttributes().put(Constants.PARAM_TRANSFORM, "{key:value}");
        webExchange.getAttributes().put(Constants.META_DATA, metaData);
        SelectorData selectorData = mock(SelectorData.class);
        when(selectorData.getId()).thenReturn("selectorId");
        RuleData ruleData = mock(RuleData.class);
        when(ruleData.getId()).thenReturn("absentRuleId");
        when(chain.execute(webExchange)).thenReturn(Mono.empty());
        when(apacheDubboProxyService.genericInvoker(any(), any(), any(), any(), any())).thenReturn(Mono.empty());
        StepVerifier.create(apacheDubboPlugin.doExecute(webExchange, chain, selectorData, ruleData))
                .expectSubscription()
                .verifyComplete();
    }

    @Test
    public void testDoDubboInvokerWithRuleHandleTimeout() {
        DubboRuleHandle dubboRuleHandle = new DubboRuleHandle();
        dubboRuleHandle.setTimeout(5000);
        ApacheDubboPluginDataHandler.RULE_CACHED_HANDLE.get()
                .cachedHandle("ruleWithTimeout", dubboRuleHandle);
        try {
            ShenyuContext context = mock(ShenyuContext.class);
            ServerWebExchange webExchange = MockServerWebExchange.from(
                    MockServerHttpRequest.get("localhost")
                            .remoteAddress(new InetSocketAddress("127.0.0.1", 8080))
                            .build());
            webExchange.getAttributes().put(Constants.CONTEXT, context);
            webExchange.getAttributes().put(Constants.PARAM_TRANSFORM, "{key:value}");
            webExchange.getAttributes().put(Constants.META_DATA, metaData);
            SelectorData selectorData = mock(SelectorData.class);
            when(selectorData.getId()).thenReturn("selectorId");
            RuleData ruleData = mock(RuleData.class);
            when(ruleData.getId()).thenReturn("ruleWithTimeout");
            when(chain.execute(webExchange)).thenReturn(Mono.empty());
            when(apacheDubboProxyService.genericInvoker(any(), any(), any(), any(), any())).thenReturn(Mono.empty());
            StepVerifier.create(apacheDubboPlugin.doExecute(webExchange, chain, selectorData, ruleData))
                    .expectSubscription()
                    .verifyComplete();
            assertEquals(5000L, RpcContext.getClientAttachment().getObjectAttachment(CommonConstants.TIMEOUT_KEY));
        } finally {
            ApacheDubboPluginDataHandler.RULE_CACHED_HANDLE.get().removeHandle("ruleWithTimeout");
        }
    }
}
