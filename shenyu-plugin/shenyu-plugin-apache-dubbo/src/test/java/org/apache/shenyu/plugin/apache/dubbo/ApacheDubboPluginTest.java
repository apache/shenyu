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

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.apache.dubbo.proxy.ApacheDubboProxyService;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.test.StepVerifier;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * The Test Case For ApachDubboPlugin.
 */
@RunWith(MockitoJUnitRunner.class)
public final class ApacheDubboPluginTest {

    private ApacheDubboPlugin apacheDubboPlugin;

    private MetaData metaData;

    private ServerWebExchange exchange;

    @Mock
    private ShenyuPluginChain chain;

    @Before
    public void setUp() {
        exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost").build());
        metaData = new MetaData();
        metaData.setId("1332017966661636096");
        metaData.setAppName("dubbo");
        metaData.setPath("/dubbo/findAll");
        metaData.setServiceName("org.apache.shenyu.test.dubbo.api.service.DubboTestService");
        metaData.setMethodName("findAll");
        metaData.setRpcType(RpcTypeEnum.DUBBO.getName());
        ApacheDubboProxyService apacheDubboProxyService = mock(ApacheDubboProxyService.class);
        apacheDubboPlugin = new ApacheDubboPlugin(apacheDubboProxyService);
    }

    @Test(expected = NullPointerException.class)
    public void doExecute() {
        ShenyuContext context = mock(ShenyuContext.class);
        exchange.getAttributes().put(Constants.CONTEXT, context);
        exchange.getAttributes().put(Constants.PARAM_TRANSFORM, "{key:value}");
        exchange.getAttributes().put(Constants.META_DATA, metaData);
        SelectorData selectorData = mock(SelectorData.class);
        RuleData data = mock(RuleData.class);
        StepVerifier.create(apacheDubboPlugin.doExecute(exchange, chain, selectorData, data)).expectSubscription().verifyComplete();
    }

    @Test(expected = NullPointerException.class)
    public void testParameterNotNullExecute() {
        ShenyuContext context = mock(ShenyuContext.class);
        exchange.getAttributes().put(Constants.CONTEXT, context);
        metaData.setParameterTypes("parameterTypes");
        exchange.getAttributes().put(Constants.META_DATA, metaData);
        SelectorData selectorData = mock(SelectorData.class);
        RuleData data = mock(RuleData.class);
        StepVerifier.create(apacheDubboPlugin.doExecute(exchange, chain, selectorData, data)).expectSubscription().verifyComplete();
    }

    @Test(expected = NullPointerException.class)
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
        StepVerifier.create(apacheDubboPlugin.doExecute(exchange, chain, selectorData, data)).expectSubscription().verifyComplete();
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
}
