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

package org.dromara.soul.plugin.waf;

import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.dto.convert.WafHandle;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.plugin.api.SoulPluginChain;
import org.dromara.soul.plugin.api.result.DefaultSoulResult;
import org.dromara.soul.plugin.api.result.SoulResult;
import org.dromara.soul.plugin.api.utils.SpringBeanUtils;
import org.dromara.soul.plugin.waf.cache.WafRuleHandleCache;
import org.dromara.soul.plugin.waf.handler.WafPluginDataHandler;
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
 *
 * @author HoldDie
 */
public final class WafPluginTest {

    private WafPlugin wafPluginUnderTest;

    private ServerWebExchange exchange;

    private SoulPluginChain chain;

    private SelectorData selectorData;

    private RuleData ruleData;

    @Before
    public void setUp() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        when(context.getBean(SoulResult.class)).thenReturn(new DefaultSoulResult());
        SpringBeanUtils springBeanUtils = SpringBeanUtils.getInstance();
        springBeanUtils.setCfgContext(context);

        final PluginData pluginData =
                new PluginData("pluginId", "pluginName", "{\"model\":\"mix\"}", 0, false);
        WafPluginDataHandler wafPluginDataHandler = new WafPluginDataHandler();
        wafPluginDataHandler.handlerPlugin(pluginData);

        exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost").build());
        chain = mock(SoulPluginChain.class);
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
                new PluginData("pluginId", "pluginName", "{\"model\":\"black\"}", 0, false);
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
        WafRuleHandleCache.getInstance().cachedHandle(WafPluginDataHandler.getCacheKeyName(ruleData), handle);
        Mono<Void> execute = wafPluginUnderTest.doExecute(exchange, chain, selectorData, ruleData);
        StepVerifier.create(execute).expectSubscription().verifyComplete();
    }

    @Test
    public void testWafPluginAllow() {
        ruleData.setId("waf");
        ruleData.setSelectorId("waf");
        WafHandle handle = GsonUtils.getGson().fromJson("{\"permission\":\"allow\",\"statusCode\":\"0\"}", WafHandle.class);
        WafRuleHandleCache.getInstance().cachedHandle(WafPluginDataHandler.getCacheKeyName(ruleData), handle);
        Mono<Void> execute = wafPluginUnderTest.doExecute(exchange, chain, selectorData, ruleData);
        StepVerifier.create(execute).expectSubscription().verifyComplete();
    }
}
