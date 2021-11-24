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

package org.apache.shenyu.plugin.rpc.context;

import com.google.common.collect.ImmutableMap;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.RequestHandle;
import org.apache.shenyu.common.dto.convert.rule.RequestHandle.ShenyuCookie;
import org.apache.shenyu.common.dto.convert.rule.RequestHandle.ShenyuRequestHeader;
import org.apache.shenyu.common.dto.convert.rule.RequestHandle.ShenyuRequestParameter;
import org.apache.shenyu.common.dto.convert.rule.RpcContextHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.rpc.context.handler.RpcContextPluginDataHandler;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.internal.util.collections.Sets;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.http.HttpCookie;
import org.springframework.http.HttpHeaders;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.util.MultiValueMap;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.Map;

import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

/**
 * Request plugin test.
 */
@RunWith(MockitoJUnitRunner.class)
public class RpcContextPluginTest {
    @Mock
    private ShenyuPluginChain chain;

    private ServerWebExchange exchange;

    private RpcContextPlugin rpcContextPlugin;

    private RuleData ruleData;

    @Before
    public void setup() {
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest
                .get("localhost")
                .header("transmitHeaderToRpcContextKey", "oldValue")
                .build());
        this.rpcContextPlugin = new RpcContextPlugin();
        this.ruleData = new RuleData();
        this.ruleData.setSelectorId("test-selectorId");
        this.ruleData.setName("test-rpc-context-plugin");

        RpcContextHandle requestHandle = new RpcContextHandle();
        requestHandle.setAddRpcContext(ImmutableMap.of("addRpcContextKey", "addRpcContextValue"));
        requestHandle.setTransmitHeaderToRpcContext(ImmutableMap.of("transmitHeaderToRpcContextKey", ""));

        RpcContextPluginDataHandler.CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(this.ruleData), requestHandle);
    }

    @Test
    public void testDoExecute() {
        SelectorData selectorData = mock(SelectorData.class);
        when(this.chain.execute(any())).thenReturn(Mono.empty());

        StepVerifier.create(rpcContextPlugin.doExecute(this.exchange, this.chain, selectorData, this.ruleData)).expectSubscription().verifyComplete();

        ArgumentCaptor<ServerWebExchange> newExchange = ArgumentCaptor.forClass(ServerWebExchange.class);
        Mockito.verify(this.chain, times(1)).execute(newExchange.capture());

        Map<String,String> shenyuRpcContext = (Map<String,String>)newExchange.getValue().getAttributes().get("shenyuRpcContext");

        assertTrue(shenyuRpcContext.containsKey("addRpcContextKey"));
        assertTrue(shenyuRpcContext.containsKey("transmitHeaderToRpcContextKey"));
    }

    @Test
    public void testGetOrder() {
        assertEquals(this.rpcContextPlugin.getOrder(), PluginEnum.RPC_CONTEXT.getCode());
    }

    @Test
    public void tesNamed() {
        assertEquals(this.rpcContextPlugin.named(), PluginEnum.RPC_CONTEXT.getName());
    }
}
