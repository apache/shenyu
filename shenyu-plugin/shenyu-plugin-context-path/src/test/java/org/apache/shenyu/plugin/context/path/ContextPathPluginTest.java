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

package org.apache.shenyu.plugin.context.path;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.ContextMappingRuleHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.cache.MetaDataCache;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.MockedStatic;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import static org.apache.shenyu.plugin.context.path.handler.ContextPathPluginDataHandler.CACHED_HANDLE;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * ContextPathMapping Plugin Test.
 */
public final class ContextPathPluginTest {

    private RuleData ruleData;

    private ShenyuPluginChain chain;

    private ShenyuContext shenyuContext;

    private SelectorData selectorData;

    private ServerWebExchange exchange;

    private ContextPathPlugin contextPathPlugin;

    @BeforeEach
    public void setup() {
        this.shenyuContext = new ShenyuContext();
        this.ruleData = mock(RuleData.class);
        this.chain = mock(ShenyuPluginChain.class);
        this.selectorData = mock(SelectorData.class);
        this.contextPathPlugin = new ContextPathPlugin();
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost").build());
        this.exchange.getAttributes().put(Constants.CONTEXT, shenyuContext);
        when(chain.execute(exchange)).thenReturn(Mono.empty());
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        final DefaultShenyuResult shenyuResult = new DefaultShenyuResult();
        when(context.getBean(ShenyuResult.class)).thenReturn(shenyuResult);
        SpringBeanUtils.getInstance().setApplicationContext(context);
    }

    /**
     * The execute test.
     */
    @Test
    public void executeTest() {
        shenyuContext.setPath("/http/context/order/findById");
        ContextMappingRuleHandle contextMappingRuleHandle = new ContextMappingRuleHandle();
        contextMappingRuleHandle.setContextPath("/http/context");
        when(ruleData.getId()).thenReturn("1");
        CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(ruleData), contextMappingRuleHandle);
        when(ruleData.getHandle()).thenReturn(GsonUtils.getGson().toJson(contextMappingRuleHandle));
        contextPathPlugin.doExecute(exchange, chain, selectorData, ruleData);
        assertEquals("/order/findById", shenyuContext.getRealUrl());

        Assertions.assertDoesNotThrow(() -> contextPathPlugin.doExecute(exchange, chain, selectorData, RuleData.builder().name("RuleData").build()));
        contextMappingRuleHandle.setAddPrefix("/addPrefix");
        Assertions.assertDoesNotThrow(() -> contextPathPlugin.doExecute(exchange, chain, selectorData, ruleData));
        contextMappingRuleHandle.setContextPath("/context");
        Assertions.assertDoesNotThrow(() -> contextPathPlugin.doExecute(exchange, chain, selectorData, ruleData));
        shenyuContext.setPath(null);
        contextMappingRuleHandle.setContextPath(null);
        Assertions.assertDoesNotThrow(() -> contextPathPlugin.doExecute(exchange, chain, selectorData, ruleData));
    }

    @ParameterizedTest
    @CsvSource({
        "/api, /api, /v2, /v2",
        "/api/, /api/, /v2, /v2",
        "/api, /api, /v2/, /v2/",
        "/api/, /api, /v2, /v2/",
        "/api/orders, /api, /v2, /v2/orders",
        "/api/a%2Fb, /api, /v2, /v2/a%2Fb",
        "/api/orders, , /v2, /v2/api/orders",
        "/api/orders, '', /v2, /v2/api/orders"
    })
    public void testAddPrefixAfterRemovingContextPath(final String path, final String contextPath,
                                                     final String addPrefix, final String expectedPath) {
        shenyuContext.setPath(path);
        ContextMappingRuleHandle handle = new ContextMappingRuleHandle();
        handle.setContextPath(contextPath);
        handle.setAddPrefix(addPrefix);
        when(ruleData.getId()).thenReturn("addPrefix");
        CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(ruleData), handle);

        contextPathPlugin.doExecute(exchange, chain, selectorData, ruleData).block();

        assertEquals(expectedPath, shenyuContext.getRealUrl());
        assertEquals(StringUtils.isBlank(contextPath) ? null : contextPath, exchange.getAttribute(Constants.CONTEXT_PATH));
        verify(chain).execute(exchange);
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    public void testExactContextPathWithPrefixAndMetadataRewrite(final boolean metadataExists) {
        shenyuContext.setPath("/api");
        MetaData original = new MetaData();
        MetaData rewritten = new MetaData();
        exchange.getAttributes().put(Constants.META_DATA, original);
        ContextMappingRuleHandle handle = new ContextMappingRuleHandle();
        handle.setContextPath("/api");
        handle.setAddPrefix("/v2");
        handle.setRewriteContextPath("/rewritten");
        when(ruleData.getId()).thenReturn("addPrefixWithMetadata");
        CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(ruleData), handle);
        MetaDataCache cache = mock(MetaDataCache.class);
        try (MockedStatic<MetaDataCache> mocked = mockStatic(MetaDataCache.class)) {
            mocked.when(MetaDataCache::getInstance).thenReturn(cache);
            when(cache.obtain("/rewritten")).thenReturn(metadataExists ? rewritten : null);

            contextPathPlugin.doExecute(exchange, chain, selectorData, ruleData).block();

            assertEquals("/v2", shenyuContext.getRealUrl());
            assertEquals("/rewritten", exchange.getAttribute(Constants.REWRITE_CONTEXT_PATH));
            assertSame(original, exchange.getAttribute(Constants.OLD_CONTEXT_PATH_META_DATA));
            assertSame(metadataExists ? rewritten : original, exchange.getAttribute(Constants.META_DATA));
            verify(cache).obtain("/rewritten");
            verify(chain).execute(exchange);
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"/foo", "/api/order/longer"})
    public void testNonMatchingContextPathPreservesExchange(final String configuredPath) {
        shenyuContext.setPath("/api/order");
        shenyuContext.setRealUrl("/previous/order");
        exchange.getAttributes().put(Constants.CONTEXT_PATH, "/previous");
        ContextMappingRuleHandle handle = new ContextMappingRuleHandle();
        handle.setContextPath(configuredPath);
        handle.setRewriteContextPath("/rewritten");
        handle.setAddPrefix("/prefix");
        when(ruleData.getId()).thenReturn("nonMatching");
        CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(ruleData), handle);

        contextPathPlugin.doExecute(exchange, chain, selectorData, ruleData).block();

        assertEquals("/previous/order", shenyuContext.getRealUrl());
        assertEquals("/previous", exchange.getAttribute(Constants.CONTEXT_PATH));
        assertFalse(exchange.getAttributes().containsKey(Constants.REWRITE_CONTEXT_PATH));
        verify(chain).execute(exchange);
    }

    /**
     * Skip.
     */
    @Test
    public void skip() {
        shenyuContext.setRpcType(RpcTypeEnum.DUBBO.getName());
        this.exchange.getAttributes().put(Constants.CONTEXT, shenyuContext);
        assertFalse(contextPathPlugin.skip(exchange));
    }

    /**
     * Named default value test case.
     */
    @Test
    public void namedTest() {
        assertEquals(PluginEnum.CONTEXT_PATH.getName(), contextPathPlugin.named());
    }

    /**
     * GetOrder default value test case.
     */
    @Test
    public void getOrderTest() {
        assertEquals(PluginEnum.CONTEXT_PATH.getCode(), contextPathPlugin.getOrder());
    }
}
