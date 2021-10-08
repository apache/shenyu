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

package org.apache.shenyu.plugin.waf;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.WafHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.waf.handler.WafPluginDataHandler;
import org.junit.Before;
import org.junit.Test;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link WafPlugin}.
 */
public final class WafPluginTest {

    private WafPlugin wafPluginUnderTest;

    private ServerWebExchange exchange;

    private ShenyuPluginChain chain;

    private SelectorData selectorData;

    private RuleData ruleData;

    @Before
    public void setUp() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        when(context.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult());
        SpringBeanUtils springBeanUtils = SpringBeanUtils.getInstance();
        springBeanUtils.setApplicationContext(context);

        final PluginData pluginData =
                new PluginData("pluginId", "pluginName", "{\"model\":\"mix\"}", "0", false);
        WafPluginDataHandler wafPluginDataHandler = new WafPluginDataHandler();
        wafPluginDataHandler.handlerPlugin(pluginData);

        exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost").build());
        chain = mock(ShenyuPluginChain.class);
        when(this.chain.execute(exchange)).thenReturn(Mono.empty());
        selectorData = mock(SelectorData.class);
        ruleData = mock(RuleData.class);
        wafPluginUnderTest = new WafPlugin();
    }

    @Test
    public void testNamed() {
        final String result = wafPluginUnderTest.named();
        assertEquals(PluginEnum.WAF.getName(), result);
    }

    @Test
    public void testGetOrder() {
        final int result = wafPluginUnderTest.getOrder();
        assertEquals(PluginEnum.WAF.getCode(), result);
    }

    @Test
    public void testWafPluginBlackWafModel() {
        final PluginData pluginData =
                new PluginData("pluginId", "pluginName", "{\"model\":\"black\"}", "0", false);
        WafPluginDataHandler wafPluginDataHandler = new WafPluginDataHandler();
        wafPluginDataHandler.handlerPlugin(pluginData);
        Mono<Void> execute = wafPluginUnderTest.doExecute(exchange, chain, null, null);
        StepVerifier.create(execute).expectSubscription().verifyComplete();
    }

    @Test
    public void testWafPluginForbidden() {
        Mono<Void> execute = wafPluginUnderTest.doExecute(exchange, chain, null, null);
        StepVerifier.create(execute).expectSubscription().verifyComplete();
    }

    @Test
    public void testWafPluginNotConfiguration() {
        Mono<Void> execute = wafPluginUnderTest.doExecute(exchange, chain, selectorData, ruleData);
        StepVerifier.create(execute).expectSubscription().verifyComplete();
    }

    @Test
    public void testWafPluginReject() {
        ruleData.setId("waf");
        ruleData.setSelectorId("waf");
        WafHandle handle = GsonUtils.getGson().fromJson("{\"permission\":\"reject\",\"statusCode\":\"0\"}", WafHandle.class);
        WafPluginDataHandler.CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(ruleData), handle);
        Mono<Void> execute = wafPluginUnderTest.doExecute(exchange, chain, selectorData, ruleData);
        StepVerifier.create(execute).expectSubscription().verifyComplete();
    }

    @Test
    public void testWafPluginAllow() {
        ruleData.setId("waf");
        ruleData.setSelectorId("waf");
        WafHandle handle = GsonUtils.getGson().fromJson("{\"permission\":\"allow\",\"statusCode\":\"0\"}", WafHandle.class);
        WafPluginDataHandler.CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(ruleData), handle);
        Mono<Void> execute = wafPluginUnderTest.doExecute(exchange, chain, selectorData, ruleData);
        StepVerifier.create(execute).expectSubscription().verifyComplete();
    }
}
