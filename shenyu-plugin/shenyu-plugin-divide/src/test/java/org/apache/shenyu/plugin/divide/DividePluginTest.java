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
import org.apache.shenyu.common.dto.convert.selector.DivideUpstream;
import org.apache.shenyu.common.dto.convert.rule.RuleHandleFactory;
import org.apache.shenyu.common.dto.convert.rule.impl.DivideRuleHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.UpstreamCheckUtils;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.divide.handler.DividePluginDataHandler;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.MockedStatic;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.net.InetSocketAddress;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

/**
 * The type divide plugin test.
 */
@RunWith(MockitoJUnitRunner.class)
public final class DividePluginTest {

    private RuleData ruleData;

    private ShenyuPluginChain chain;

    private DividePlugin dividePlugin;

    private SelectorData selectorData;

    private ServerWebExchange exchange;

    private ServerWebExchange postExchange;

    private List<DivideUpstream> divideUpstreamList;

    private MockedStatic<UpstreamCheckUtils> mockCheckUtils;
    
    @Before
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
                .build());
        this.postExchange = MockServerWebExchange.from(MockServerHttpRequest.post("localhost?param=1")
                .remoteAddress(new InetSocketAddress(8090))
                .build());
        this.dividePlugin = new DividePlugin();

        // mock static
        mockCheckUtils = mockStatic(UpstreamCheckUtils.class);
        mockCheckUtils.when(() -> UpstreamCheckUtils.checkUrl(anyString(), anyInt())).thenReturn(true);
    }

    @After
    public void tearDown() {
        mockCheckUtils.close();
    }

    /**
     * Divide plugin doExecute.
     */
    @Test
    public void doExecuteTest() {
        initMockInfo();
        when(chain.execute(exchange)).thenReturn(Mono.empty());
        Mono<Void> result = dividePlugin.doExecute(exchange, chain, selectorData, ruleData);
        StepVerifier.create(result).expectSubscription().verifyComplete();
    }

    /**
     * Divide plugin post doExecute.
     */
    @Test
    public void doPostExecuteTest() {
        initMockInfo();
        when(chain.execute(postExchange)).thenReturn(Mono.empty());
        Mono<Void> result = dividePlugin.doExecute(postExchange, chain, selectorData, ruleData);
        StepVerifier.create(result).expectSubscription().verifyComplete();
    }

    /**
     * Skip.
     */
    @Test
    public void skip() {
        initMockInfo();
        Assert.assertTrue(dividePlugin.skip(exchange));
    }

    /**
     * Named default value test case.
     */
    @Test
    public void namedTest() {
        Assert.assertEquals(PluginEnum.DIVIDE.getName(), dividePlugin.named());
    }

    /**
     * GetOrder default value test case.
     */
    @Test
    public void getOrderTest() {
        Assert.assertEquals(PluginEnum.DIVIDE.getCode(), dividePlugin.getOrder());
    }

    /**
     * Init mock info.
     */
    private void initMockInfo() { 
        ShenyuContext context = mock(ShenyuContext.class);
        context.setRpcType(RpcTypeEnum.HTTP.getName());
        DivideRuleHandle handle = (DivideRuleHandle) RuleHandleFactory.ruleHandle(PluginEnum.DIVIDE.getName(), "", "");
        when(selectorData.getId()).thenReturn("mock");
        when(selectorData.getHandle()).thenReturn(GsonUtils.getGson().toJson(divideUpstreamList));
        when(ruleData.getHandle()).thenReturn(GsonUtils.getGson().toJson(handle));
        DividePluginDataHandler dividePluginDataHandler = new DividePluginDataHandler();
        dividePluginDataHandler.handlerRule(ruleData);
        dividePluginDataHandler.handlerSelector(selectorData);
        when(context.getRealUrl()).thenReturn("mock-real");
        exchange.getAttributes().put(Constants.CONTEXT, context);
        when(chain.execute(exchange)).thenReturn(Mono.empty());
        postExchange.getAttributes().put(Constants.CONTEXT, context);
        when(chain.execute(postExchange)).thenReturn(Mono.empty());
    }
}
