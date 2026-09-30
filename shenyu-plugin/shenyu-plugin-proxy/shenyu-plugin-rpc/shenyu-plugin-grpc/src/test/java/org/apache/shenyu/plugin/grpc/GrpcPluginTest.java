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

import io.grpc.MethodDescriptor;
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
import org.apache.shenyu.plugin.grpc.cache.GrpcClientCache;
import org.apache.shenyu.plugin.grpc.client.ShenyuGrpcClient;
import org.apache.shenyu.plugin.grpc.context.GrpcConstants;
import org.apache.shenyu.plugin.grpc.proto.ShenyuGrpcResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.lang.reflect.Field;
import java.net.InetSocketAddress;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * The Test Case For {@link GrpcPlugin}.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class GrpcPluginTest {

    @Spy
    private GrpcPlugin grpcPlugin;

    @Mock
    private ShenyuPluginChain chain;

    @Mock
    private SelectorData selector;

    @BeforeEach
    public void setUp() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setApplicationContext(context);
        when(context.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult());

        when(selector.getName()).thenReturn("/grpc");
        when(selector.getId()).thenReturn("grpcId");
    }

    @AfterEach
    public void tearDown() {
        GrpcClientCache.removeClient("grpcId");
    }

    @Test
    public void testDoExecute() throws ClassNotFoundException, NoSuchFieldException, IllegalAccessException {
        ServerWebExchange exchange = getServerWebExchange(new InetSocketAddress("127.0.0.1", 8090));
        executeRequest(exchange, "127.0.0.1", getMetaData(), MethodDescriptor.MethodType.SERVER_STREAMING);
    }

    @Test
    public void testDoExecuteWithNullRemoteAddress()
            throws ClassNotFoundException, NoSuchFieldException, IllegalAccessException {
        ServerWebExchange exchange = getServerWebExchange();
        executeRequest(exchange, "", getMetaData(), MethodDescriptor.MethodType.SERVER_STREAMING);
    }

    @Test
    public void testDoExecuteWithNullRpcExt()
            throws ClassNotFoundException, NoSuchFieldException, IllegalAccessException {
        ServerWebExchange exchange = getServerWebExchange();
        MetaData metaData = getMetaData();
        metaData.setRpcExt(null);
        executeRequest(exchange, "", metaData, MethodDescriptor.MethodType.UNARY);
    }

    @Test
    public void testDoExecuteWithEmptyRpcExt()
            throws ClassNotFoundException, NoSuchFieldException, IllegalAccessException {
        ServerWebExchange exchange = getServerWebExchange();
        MetaData metaData = getMetaData();
        metaData.setRpcExt("");
        executeRequest(exchange, "", metaData, MethodDescriptor.MethodType.UNARY);
    }

    @Test
    public void testDoExecuteWithBlankRpcExt()
            throws ClassNotFoundException, NoSuchFieldException, IllegalAccessException {
        ServerWebExchange exchange = getServerWebExchange();
        MetaData metaData = getMetaData();
        metaData.setRpcExt(" ");
        executeRequest(exchange, "", metaData, MethodDescriptor.MethodType.UNARY);
    }

    @SuppressWarnings("unchecked")
    private void executeRequest(final ServerWebExchange exchange, final String expectedRemoteAddress,
                                final MetaData metaData, final MethodDescriptor.MethodType expectedMethodType)
            throws ClassNotFoundException, NoSuchFieldException, IllegalAccessException {
        exchange.getAttributes().put(Constants.PARAM_TRANSFORM, "{message:1}");
        exchange.getAttributes().put(Constants.META_DATA, metaData);

        Class<?> grpcClientCacheClass = Class.forName("org.apache.shenyu.plugin.grpc.cache.GrpcClientCache");
        Field clientCacheField = grpcClientCacheClass.getDeclaredField("CLIENT_CACHE");
        clientCacheField.setAccessible(true);
        Map<String, ShenyuGrpcClient> clientCacheMap = (Map<String, ShenyuGrpcClient>) clientCacheField.get(grpcClientCacheClass);
        ShenyuGrpcClient mockClient = mock(ShenyuGrpcClient.class);
        ShenyuGrpcResponse response = new ShenyuGrpcResponse();
        response.getResults().add("success");
        when(mockClient.call(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any()))
                .thenAnswer(invocation -> {
                    assertEquals(expectedRemoteAddress, GrpcConstants.GRPC_REMOTE_ADDRESS.get());
                    assertEquals(expectedMethodType, invocation.getArgument(3));
                    return CompletableFuture.completedFuture(response);
                });
        clientCacheMap.put("grpcId", mockClient);

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
                .rpcExt("{\"timeout\":5000,\"methodType\":\"SERVER_STREAMING\"}")
                .parameterTypes("param")
                .enabled(true).build();
    }

    private ServerWebExchange getServerWebExchange() {
        return getServerWebExchange(null);
    }

    private ServerWebExchange getServerWebExchange(final InetSocketAddress remoteAddress) {
        MockServerHttpRequest.BaseBuilder<?> requestBuilder = MockServerHttpRequest.get("http://localhost/grpc/echo");
        if (Objects.nonNull(remoteAddress)) {
            requestBuilder.remoteAddress(remoteAddress);
        }
        ServerWebExchange exchange = MockServerWebExchange.from(requestBuilder.build());
        ShenyuContext shenyuContext = mock(ShenyuContext.class);
        when(shenyuContext.getRpcType()).thenReturn(RpcTypeEnum.GRPC.getName());
        exchange.getAttributes().put(Constants.CONTEXT, shenyuContext);
        return exchange;
    }
}
