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

package org.apache.shenyu.plugin.sofa.response;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.SoulPluginChain;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.mock.web.server.MockServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import static org.junit.Assert.assertEquals;

/**
 * SofaResponsePluginTest.
 *
 * @author tydhot
 */
@RunWith(MockitoJUnitRunner.class)
public final class SofaResponsePluginTest {
    @Mock
    private SoulPluginChain chain;

    private ServerWebExchange exchange;

    private SofaResponsePlugin sofaResponsePlugin;

    @Before
    public void setup() {
        exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost").build());
    }

    @Test
    public void testNoResult() {
        Mockito.when(chain.execute(exchange)).thenReturn(Mono.empty());
        sofaResponsePlugin = new SofaResponsePlugin();
        StepVerifier.create(sofaResponsePlugin.execute(exchange, chain)).expectSubscription().verifyError();
    }

    @Test
    public void testGetResult() {
        Mockito.when(chain.execute(exchange)).thenReturn(Mono.empty());
        sofaResponsePlugin = new SofaResponsePlugin();
        String response = "{}";
        exchange.getAttributes().put(Constants.SOFA_RPC_RESULT, response);
        StepVerifier.create(sofaResponsePlugin.execute(exchange, chain)).expectSubscription().verifyError(NullPointerException.class);
    }

    @Test
    public void testNamed() {
        sofaResponsePlugin = new SofaResponsePlugin();
        final String result = sofaResponsePlugin.named();
        assertEquals(PluginEnum.RESPONSE.getName(), result);
    }

    @Test
    public void testGetOrder() {
        sofaResponsePlugin = new SofaResponsePlugin();
        final int result = sofaResponsePlugin.getOrder();
        assertEquals(PluginEnum.RESPONSE.getCode(), result);
    }
}
