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

package org.apache.shenyu.plugin.divide;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.DivideRuleHandle;
import org.apache.shenyu.common.dto.convert.selector.DivideUpstream;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.UpstreamCheckUtils;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.factory.LoadBalancerFactory;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.divide.handler.DividePluginDataHandler;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.net.InetSocketAddress;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

/**
 * The type divide plugin test.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class DividePluginTest {

    private RuleData ruleData;

    private ShenyuPluginChain chain;

    private DividePlugin dividePlugin;

    private SelectorData selectorData;

    private ServerWebExchange exchange;

    private ServerWebExchange postExchange;

    private List<DivideUpstream> divideUpstreamList;

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
                .header("test", "test")
                .header("Content-Length", "50")
                .build());
        this.postExchange = MockServerWebExchange.from(MockServerHttpRequest.post("localhost?param=1")
                .remoteAddress(new InetSocketAddress(8090))
                .build());
        this.dividePlugin = new DividePlugin();

        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setApplicationContext(context);
        when(context.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult());

        // mock static
        mockCheckUtils = mockStatic(UpstreamCheckUtils.class);
        mockCheckUtils.when(() -> UpstreamCheckUtils.checkUrl(anyString(), anyInt())).thenReturn(true);
        initMockInfo();
    }

    @AfterEach
    public void tearDown() {
        mockCheckUtils.close();
    }

    /**
     * Divide plugin doExecute.
     */
    @Test
    public void doExecuteTest() {
        when(chain.execute(exchange)).thenReturn(Mono.empty());
        Mono<Void> result = dividePlugin.doExecute(exchange, chain, selectorData, ruleData);
        StepVerifier.create(result).expectSubscription().verifyComplete();
        DivideRuleHandle divideRuleHandle = DividePluginDataHandler.CACHED_HANDLE.get()
                .obtainHandle(CacheKeyUtils.INST.getKey(ruleData));
        divideRuleHandle.setHeaderMaxSize(1);
        // hit `ruleHandle.getHeaderMaxSize() > 0`
        dividePlugin.doExecute(exchange, chain, selectorData, ruleData);
        divideRuleHandle.setHeaderMaxSize(1);
        // hit `ruleHandle.getRequestMaxSize() > 0`
        divideRuleHandle.setHeaderMaxSize(0);
        divideRuleHandle.setRequestMaxSize(1);
        dividePlugin.doExecute(exchange, chain, selectorData, ruleData);
        // hit `CollectionUtils.isEmpty(upstreamList)`
        divideRuleHandle.setRequestMaxSize(0);
        UpstreamCacheManager.getInstance().removeByKey(selectorData.getId());
        when(selectorData.getHandle()).thenReturn(null);
        dividePlugin.doExecute(exchange, chain, selectorData, ruleData);
        // hit `Objects.isNull(upstream)`
        MockedStatic<LoadBalancerFactory> loadBalancerFactoryMockedStatic = mockStatic(LoadBalancerFactory.class);
        loadBalancerFactoryMockedStatic.when(() -> LoadBalancerFactory.selector(any(), any(), any()))
                .thenReturn(null);
        dividePlugin.doExecute(exchange, chain, selectorData, ruleData);
        // hit `assert shenyuContext != null`
        exchange.getAttributes().remove(Constants.CONTEXT);
        assertThrows(AssertionError.class, () -> dividePlugin.doExecute(exchange, chain, selectorData, ruleData));
    }

    /**
     * Divide plugin post doExecute.
     */
    @Test
    public void doPostExecuteTest() {
        when(chain.execute(postExchange)).thenReturn(Mono.empty());
        Mono<Void> result = dividePlugin.doExecute(postExchange, chain, selectorData, ruleData);
        StepVerifier.create(result).expectSubscription().verifyComplete();
    }

    /**
     * Skip.
     */
    @Test
    public void skip() {
        assertTrue(dividePlugin.skip(exchange));
    }

    /**
     * handleSelectorIfNull.
     */
    @Test
    public void handleSelectorIfNullTest() {
        assertTrue(dividePlugin.handleSelectorIfNull(PluginEnum.DIVIDE.getName(), exchange, chain) != null);
    }

    /**
     * handleRuleIfNull.
     */
    @Test
    public void handleRuleIfNullTest() {
        assertTrue(dividePlugin.handleRuleIfNull(PluginEnum.DIVIDE.getName(), exchange, chain) != null);
    }

    /**
     * Named default value test case.
     */
    @Test
    public void namedTest() {
        assertEquals(PluginEnum.DIVIDE.getName(), dividePlugin.named());
    }

    /**
     * GetOrder default value test case.
     */
    @Test
    public void getOrderTest() {
        assertEquals(PluginEnum.DIVIDE.getCode(), dividePlugin.getOrder());
    }

    /**
     * Init mock info.
     */
    private void initMockInfo() {
        ShenyuContext context = mock(ShenyuContext.class);
        context.setRpcType(RpcTypeEnum.HTTP.getName());
        DivideRuleHandle handle = new DivideRuleHandle();
        when(selectorData.getId()).thenReturn("mock");
        when(selectorData.getHandle()).thenReturn(GsonUtils.getGson().toJson(divideUpstreamList));
        when(ruleData.getHandle()).thenReturn(GsonUtils.getGson().toJson(handle));
        DividePluginDataHandler dividePluginDataHandler = new DividePluginDataHandler();
        dividePluginDataHandler.handlerRule(ruleData);
        dividePluginDataHandler.handlerSelector(selectorData);
        exchange.getAttributes().put(Constants.CONTEXT, context);
        when(chain.execute(exchange)).thenReturn(Mono.empty());
        postExchange.getAttributes().put(Constants.CONTEXT, context);
        when(chain.execute(postExchange)).thenReturn(Mono.empty());
    }
}
