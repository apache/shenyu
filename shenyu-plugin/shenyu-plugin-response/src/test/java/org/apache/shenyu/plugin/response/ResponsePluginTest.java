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

package org.apache.shenyu.plugin.response;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.response.strategy.MessageWriter;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * The test case for {@link ResponsePlugin}.
 */
public class ResponsePluginTest {

    private ResponsePlugin responsePlugin;

    @Mock
    private ShenyuPluginChain chain;

    @BeforeEach
    public void setup() {
        ConfigurableApplicationContext applicationContext = mock(ConfigurableApplicationContext.class);
        when(applicationContext.getBean(ShenyuResult.class)).thenReturn(mock(ShenyuResult.class));
        SpringBeanUtils springBeanUtils = SpringBeanUtils.getInstance();
        springBeanUtils.setApplicationContext(applicationContext);
        Map<String, MessageWriter> writerMap = new HashMap<>();
        MessageWriter messageWriter = mock(MessageWriter.class);
        when(messageWriter.writeWith(any(), any())).thenReturn(Mono.empty());
        writerMap.put(RpcTypeEnum.HTTP.getName(), messageWriter);
        writerMap.put(RpcTypeEnum.SPRING_CLOUD.getName(), messageWriter);
        writerMap.put(RpcTypeEnum.DUBBO.getName(), messageWriter);
        writerMap.put(RpcTypeEnum.SOFA.getName(), messageWriter);
        writerMap.put(RpcTypeEnum.GRPC.getName(), messageWriter);
        writerMap.put(RpcTypeEnum.MOTAN.getName(), messageWriter);
        writerMap.put(RpcTypeEnum.TARS.getName(), messageWriter);
        responsePlugin = new ResponsePlugin(writerMap);
    }

    @Test
    public void testExecute() {
        ServerWebExchange httpExchange = generateServerWebExchange(RpcTypeEnum.HTTP.getName());
        StepVerifier.create(responsePlugin.execute(httpExchange, chain)).expectSubscription().verifyComplete();

        ServerWebExchange springCloudExchange = generateServerWebExchange(RpcTypeEnum.SPRING_CLOUD.getName());
        StepVerifier.create(responsePlugin.execute(springCloudExchange, chain)).expectSubscription().verifyComplete();

        ServerWebExchange dubboExchange = generateServerWebExchange(RpcTypeEnum.DUBBO.getName());
        StepVerifier.create(responsePlugin.execute(dubboExchange, chain)).expectSubscription().verifyComplete();

        ServerWebExchange sofaExchange = generateServerWebExchange(RpcTypeEnum.SOFA.getName());
        StepVerifier.create(responsePlugin.execute(sofaExchange, chain)).expectSubscription().verifyComplete();

        ServerWebExchange grpcExchange = generateServerWebExchange(RpcTypeEnum.GRPC.getName());
        StepVerifier.create(responsePlugin.execute(grpcExchange, chain)).expectSubscription().verifyComplete();

        ServerWebExchange motanExchange = generateServerWebExchange(RpcTypeEnum.MOTAN.getName());
        StepVerifier.create(responsePlugin.execute(motanExchange, chain)).expectSubscription().verifyComplete();

        ServerWebExchange tarsExchange = generateServerWebExchange(RpcTypeEnum.TARS.getName());
        StepVerifier.create(responsePlugin.execute(tarsExchange, chain)).expectSubscription().verifyComplete();
    }

    @Test
    public void testGetOrder() {
        assertEquals(responsePlugin.getOrder(), PluginEnum.RESPONSE.getCode());
    }

    @Test
    public void tesNamed() {
        assertEquals(responsePlugin.named(), PluginEnum.RESPONSE.getName());
    }

    private ServerWebExchange generateServerWebExchange(final String name) {
        ServerWebExchange exchange = MockServerWebExchange
                .from(MockServerHttpRequest.get("/test").build());
        ShenyuContext shenyuContext = new ShenyuContext();
        shenyuContext.setRpcType(name);
        exchange.getAttributes().put(Constants.CONTEXT, shenyuContext);
        return exchange;
    }
}
