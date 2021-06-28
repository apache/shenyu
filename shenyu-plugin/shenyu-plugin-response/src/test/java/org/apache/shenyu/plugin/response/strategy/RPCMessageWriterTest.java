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

package org.apache.shenyu.plugin.response.strategy;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * The Test Case For {@link RPCMessageWriter}.
 */
@RunWith(MockitoJUnitRunner.class)
public class RPCMessageWriterTest {

    @Mock
    private ShenyuPluginChain chain;

    private ServerWebExchange exchange;

    private RPCMessageWriter rpcMessageWriter;

    @Before
    public void setUp() {
        ConfigurableApplicationContext applicationContext = mock(ConfigurableApplicationContext.class);
        when(applicationContext.getBean(ShenyuResult.class)).thenReturn(mock(ShenyuResult.class));
        SpringBeanUtils springBeanUtils = SpringBeanUtils.getInstance();
        springBeanUtils.setCfgContext(applicationContext);
        exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost").build());
        rpcMessageWriter = new RPCMessageWriter();
        Mockito.when(chain.execute(exchange)).thenReturn(Mono.empty());
    }

    @Test
    public void testExecuteWithResult() {
        String response = "{}";
        exchange.getAttributes().put(Constants.RPC_RESULT, response);
        StepVerifier.create(rpcMessageWriter.writeWith(exchange, chain)).expectSubscription().verifyComplete();
    }

    @Test
    public void testExecuteWithNoResult() {
        StepVerifier.create(rpcMessageWriter.writeWith(exchange, chain)).expectSubscription().verifyComplete();
    }
}
