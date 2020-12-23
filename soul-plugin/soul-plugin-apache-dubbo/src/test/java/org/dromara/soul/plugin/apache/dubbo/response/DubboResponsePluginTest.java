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

package org.dromara.soul.plugin.apache.dubbo.response;

import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.plugin.api.SoulPluginChain;
import org.dromara.soul.plugin.api.context.SoulContext;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

/**
 * The Test Case For DubboResponsePlugin.
 *
 * @author nuo-promise
 **/
@RunWith(MockitoJUnitRunner.class)
public final class DubboResponsePluginTest {

    @Mock
    private SoulPluginChain chain;

    private ServerWebExchange exchange;

    private DubboResponsePlugin dubboResponsePlugin;

    @Before
    public void setUp() {
        dubboResponsePlugin = new DubboResponsePlugin();
        exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost").build());
    }

    @Test
    public void testNoResult() {
        Mockito.when(chain.execute(exchange)).thenReturn(Mono.empty());
        StepVerifier.create(dubboResponsePlugin.execute(exchange, chain)).expectSubscription().verifyError();
    }

    @Test
    public void testGetResult() {
        Mockito.when(chain.execute(exchange)).thenReturn(Mono.empty());
        String response = "{}";
        exchange.getAttributes().put(Constants.DUBBO_RPC_RESULT, response);
        StepVerifier.create(dubboResponsePlugin.execute(exchange, chain)).expectSubscription().verifyError(NullPointerException.class);
    }

    @Test
    public void skip() {
        ServerWebExchange exchange = MockServerWebExchange
                .from(MockServerHttpRequest.get("http://localhost:8888/test").build());
        SoulContext soulContext = new SoulContext();
        soulContext.setRpcType(RpcTypeEnum.DUBBO.getName());
        exchange.getAttributes().put(Constants.CONTEXT, soulContext);
        assertFalse(dubboResponsePlugin.skip(exchange));
        soulContext.setRpcType(RpcTypeEnum.HTTP.getName());
        exchange.getAttributes().put(Constants.CONTEXT, soulContext);
        assertTrue(dubboResponsePlugin.skip(exchange));
    }

    @Test
    public void getOrder() {
        assertThat(dubboResponsePlugin.getOrder(), is(PluginEnum.RESPONSE.getCode()));
    }

    @Test
    public void named() {
        assertThat(dubboResponsePlugin.named(), is(PluginEnum.RESPONSE.getName()));
    }
}
