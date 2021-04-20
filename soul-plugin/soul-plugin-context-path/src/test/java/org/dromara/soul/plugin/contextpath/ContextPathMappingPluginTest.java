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

package org.dromara.soul.plugin.contextpath;

import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.dto.convert.rule.impl.ContextMappingHandle;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.plugin.api.SoulPluginChain;
import org.dromara.soul.plugin.api.context.SoulContext;
import org.dromara.soul.plugin.api.result.DefaultSoulResult;
import org.dromara.soul.plugin.api.result.SoulResult;
import org.dromara.soul.plugin.api.utils.SpringBeanUtils;
import org.dromara.soul.plugin.contextpath.cache.ApplicationConfigCache;
import org.dromara.soul.plugin.contextpath.handler.ContextPathMappingPluginDataHandler;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * ContextPathMapping Plugin Test.
 *
 * @author zhanglei
 */
public final class ContextPathMappingPluginTest {

    private RuleData ruleData;

    private SoulPluginChain chain;

    private SoulContext soulContext;

    private SelectorData selectorData;

    private ServerWebExchange exchange;

    private ContextPathMappingPlugin contextPathMappingPlugin;

    @Before
    public void setup() {
        this.soulContext = new SoulContext();
        this.ruleData = mock(RuleData.class);
        this.chain = mock(SoulPluginChain.class);
        this.selectorData = mock(SelectorData.class);
        this.contextPathMappingPlugin = new ContextPathMappingPlugin();
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost").build());
        this.exchange.getAttributes().put(Constants.CONTEXT, soulContext);
        when(chain.execute(exchange)).thenReturn(Mono.empty());
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        final DefaultSoulResult soulResult = new DefaultSoulResult();
        when(context.getBean(SoulResult.class)).thenReturn(soulResult);
        SpringBeanUtils.getInstance().setCfgContext(context);
    }

    /**
     * The execute test.
     */
    @Test
    public void executeTest() {
        soulContext.setPath("/http/context/order/findById");
        ContextMappingHandle contextMappingHandle = new ContextMappingHandle();
        contextMappingHandle.setContextPath("/http/context");
        ApplicationConfigCache.getInstance().cachedHandle(ContextPathMappingPluginDataHandler.getCacheKeyName(ruleData), contextMappingHandle);
        when(ruleData.getHandle()).thenReturn(GsonUtils.getGson().toJson(contextMappingHandle));
        contextPathMappingPlugin.doExecute(exchange, chain, selectorData, ruleData);
        Assert.assertEquals(soulContext.getRealUrl(), "/order/findById");
    }

    /**
     * The execute real path test.
     */
    @Test
    public void executeRealPathTest() {
        soulContext.setPath("/http/context/order/findById");
        ContextMappingHandle contextMappingHandle = new ContextMappingHandle();
        contextMappingHandle.setContextPath("/http/context");
        contextMappingHandle.setRealUrl("/findById");
        ApplicationConfigCache.getInstance().cachedHandle(ContextPathMappingPluginDataHandler.getCacheKeyName(ruleData), contextMappingHandle);
        when(ruleData.getHandle()).thenReturn(GsonUtils.getGson().toJson(contextMappingHandle));
        contextPathMappingPlugin.doExecute(exchange, chain, selectorData, ruleData);
        Assert.assertEquals(soulContext.getRealUrl(), "/findById");
    }

    /**
     * Skip.
     */
    @Test
    public void skip() {
        soulContext.setRpcType(RpcTypeEnum.DUBBO.getName());
        this.exchange.getAttributes().put(Constants.CONTEXT, soulContext);
        Assert.assertTrue(contextPathMappingPlugin.skip(exchange));
    }

    /**
     * Named default value test case.
     */
    @Test
    public void namedTest() {
        Assert.assertEquals(PluginEnum.CONTEXTPATH_MAPPING.getName(), contextPathMappingPlugin.named());
    }

    /**
     * GetOrder default value test case.
     */
    @Test
    public void getOrderTest() {
        Assert.assertEquals(PluginEnum.CONTEXTPATH_MAPPING.getCode(), contextPathMappingPlugin.getOrder());
    }
}
