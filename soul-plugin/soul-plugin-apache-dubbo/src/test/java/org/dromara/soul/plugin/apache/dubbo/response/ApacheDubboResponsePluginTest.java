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
import org.dromara.soul.plugin.api.SoulPluginChain;
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

/**
 * ApacheDubboResponsePluginTest
 * @author kaito
 */

@RunWith(MockitoJUnitRunner.class)
public class ApacheDubboResponsePluginTest {
    @Mock
    private SoulPluginChain chain;
    
    private ServerWebExchange exchange;
    
    private DubboResponsePlugin dubboResponsePlugin;
    
    @Before
    public void setup() {
        exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost").build());
    }
    
    @Test
    public void testNoResult() {
        Mockito.when(chain.execute(exchange)).thenReturn(Mono.empty());
        dubboResponsePlugin = new DubboResponsePlugin();
        StepVerifier.create(dubboResponsePlugin.execute(exchange, chain)).expectSubscription().verifyError();
    }
    
    @Test
    public void testGetResult() {
        String response = "{}";
        Mockito.when(chain.execute(exchange)).thenReturn(Mono.empty());
        dubboResponsePlugin = new DubboResponsePlugin();
        exchange.getAttributes().put(Constants.DUBBO_RPC_RESULT, response);
        StepVerifier.create(dubboResponsePlugin.execute(exchange, chain)).expectSubscription().verifyError(NullPointerException.class);
    }
}
