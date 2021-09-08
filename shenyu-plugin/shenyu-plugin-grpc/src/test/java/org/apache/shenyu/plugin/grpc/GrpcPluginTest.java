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

package org.apache.shenyu.plugin.grpc;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.grpc.client.ShenyuGrpcClient;
import org.apache.shenyu.plugin.grpc.proto.ShenyuGrpcResponse;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.Spy;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;
import java.lang.reflect.Field;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * The Test Case For {@link GrpcPlugin}.
 */
@RunWith(MockitoJUnitRunner.class)
public class GrpcPluginTest {

    @Spy
    private GrpcPlugin grpcPlugin;

    @Mock
    private ShenyuPluginChain chain;

    @Mock
    private SelectorData selector;

    @Before
    public void setUp() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setApplicationContext(context);
        when(context.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult());

        when(selector.getName()).thenReturn("/grpc");
    }

    @Test
    public void testDoExecute() throws ClassNotFoundException, NoSuchFieldException, IllegalAccessException {
        ServerWebExchange exchange = getServerWebExchange();
        exchange.getAttributes().put(Constants.PARAM_TRANSFORM, "{message:1}");
        exchange.getAttributes().put(Constants.META_DATA, getMetaData());

        Class grpcClientCacheClass = Class.forName("org.apache.shenyu.plugin.grpc.cache.GrpcClientCache");
        Field clientCacheField = grpcClientCacheClass.getDeclaredField("CLIENT_CACHE");
        clientCacheField.setAccessible(true);
        Map<String, ShenyuGrpcClient> clientCacheMap = (Map<String, ShenyuGrpcClient>) clientCacheField.get(grpcClientCacheClass);
        ShenyuGrpcClient mockClient = mock(ShenyuGrpcClient.class);
        ShenyuGrpcResponse response = new ShenyuGrpcResponse();
        response.getResults().add("success");
        when(mockClient.call(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any()))
                .thenReturn(CompletableFuture.completedFuture(response));
        clientCacheMap.put("/grpc", mockClient);

        when(chain.execute(Mockito.any())).thenReturn(Mono.empty());
        RuleData data = mock(RuleData.class);
        StepVerifier.create(grpcPlugin.doExecute(exchange, chain, selector, data)).expectSubscription().verifyComplete();
    }

    @Test
    public void testDoExecuteMetaDataError() {
        ServerWebExchange exchange = getServerWebExchange();
        exchange.getAttributes().put(Constants.META_DATA, getMetaData());
        RuleData data = mock(RuleData.class);
        StepVerifier.create(grpcPlugin.doExecute(exchange, chain, selector, data)).expectSubscription().verifyComplete();
    }

    @Test
    public void testDoExecuteParaIsBlankError() {
        ServerWebExchange exchange = getServerWebExchange();
        exchange.getAttributes().put(Constants.META_DATA, new MetaData());
        RuleData data = mock(RuleData.class);
        StepVerifier.create(grpcPlugin.doExecute(exchange, chain, selector, data)).expectSubscription().verifyComplete();
    }

    @Test
    public void testGetOrder() {
        final int result = grpcPlugin.getOrder();
        assertEquals(PluginEnum.GRPC.getCode(), result);
    }

    @Test
    public void testNamed() {
        final String result = grpcPlugin.named();
        assertEquals(PluginEnum.GRPC.getName(), result);
    }

    @Test
    public void testSkip() {
        final boolean result = grpcPlugin.skip(getServerWebExchange());
        assertFalse(result);
    }

    private MetaData getMetaData() {
        return MetaData.builder()
                .id("1332017977771636096")
                .appName("grpc")
                .contextPath("/grpc").path("/grpc/echo")
                .serviceName("echo.EchoService")
                .methodName("echo")
                .rpcType(RpcTypeEnum.GRPC.getName())
                .rpcExt("{timeout:5000}")
                .parameterTypes("param")
                .enabled(true).build();
    }

    private ServerWebExchange getServerWebExchange() {
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("http://localhost/grpc/echo").build());
        ShenyuContext shenyuContext = mock(ShenyuContext.class);
        when(shenyuContext.getRpcType()).thenReturn(RpcTypeEnum.GRPC.getName());
        exchange.getAttributes().put(Constants.CONTEXT, shenyuContext);
        return exchange;
    }
}
