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

package org.dromara.soul.plugin.global;

import lombok.SneakyThrows;
import org.dromara.soul.common.dto.MetaData;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.plugin.api.context.SoulContext;
import org.junit.Before;
import org.junit.Test;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;

import java.lang.reflect.Method;
import java.net.InetSocketAddress;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

/**
 * The Test Case For DefaultSoulContextBuilder.
 *
 * @author nuo-promise
 **/
public final class DefaultSoulContextBuilderTest {

    private DefaultSoulContextBuilder defaultSoulContextBuilder;

    @Before
    public void setUp() {
        defaultSoulContextBuilder = new DefaultSoulContextBuilder();
    }

    @Test
    public void testBuild() {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("http://localhost:8080/http")
                .remoteAddress(new InetSocketAddress(8092))
                .header("MetaDataCache", "Hello")
                .build());
        assertNotNull(defaultSoulContextBuilder.build(exchange));
    }

    @SneakyThrows
    @Test
    public void testTransform() {
        Class<DefaultSoulContextBuilder> clazz = DefaultSoulContextBuilder.class;
        Method method = clazz.getDeclaredMethod("transform", ServerHttpRequest.class, MetaData.class);
        method.setAccessible(true);
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("http://localhost:8080/http")
                .remoteAddress(new InetSocketAddress(8092))
                .header("MetaDataCache", "Hello")
                .build());
        SoulContext soulContextSpringCloud = (SoulContext) method.invoke(this.defaultSoulContextBuilder, exchange.getRequest(),
                MetaData.builder().rpcType(RpcTypeEnum.SPRING_CLOUD.getName()).enabled(Boolean.TRUE).build());
        assertEquals(soulContextSpringCloud.getRpcType(), RpcTypeEnum.SPRING_CLOUD.getName());
        SoulContext soulContextDubbo = (SoulContext) method.invoke(this.defaultSoulContextBuilder, exchange.getRequest(),
                MetaData.builder().rpcType(RpcTypeEnum.DUBBO.getName()).enabled(Boolean.TRUE).build());
        assertEquals(soulContextDubbo.getRpcType(), RpcTypeEnum.DUBBO.getName());
        SoulContext soulContextSofa = (SoulContext) method.invoke(this.defaultSoulContextBuilder, exchange.getRequest(),
                MetaData.builder().rpcType(RpcTypeEnum.SOFA.getName()).enabled(Boolean.TRUE).build());
        assertEquals(soulContextSofa.getRpcType(), RpcTypeEnum.SOFA.getName());
        SoulContext soulContextHttp = (SoulContext) method.invoke(this.defaultSoulContextBuilder, exchange.getRequest(),
                MetaData.builder().rpcType(RpcTypeEnum.HTTP.getName()).enabled(Boolean.TRUE).build());
        assertEquals(soulContextHttp.getRpcType(), RpcTypeEnum.HTTP.getName());
    }

    @SneakyThrows
    @Test
    public void testSetSoulContextByDubbo() {
        SoulContext soulContext = new SoulContext();
        soulContext.setPath("localhost");
        MetaData metaData = MetaData.builder()
                .appName("soul")
                .serviceName("soul")
                .rpcType("dubbo")
                .contextPath("contextPath")
                .build();
        testSetSoulContext("setSoulContextByDubbo", soulContext, metaData);
    }

    @SneakyThrows
    @Test
    public void testSetSoulContextBySofa() {
        SoulContext soulContext = new SoulContext();
        soulContext.setPath("localhost");
        MetaData metaData = MetaData.builder()
                .appName("soul")
                .serviceName("soul")
                .rpcType("sofa")
                .contextPath("contextPath")
                .build();
        testSetSoulContext("setSoulContextBySofa", soulContext, metaData);
    }

    @SneakyThrows
    private void testSetSoulContext(final String methodName, final SoulContext soulContext, final MetaData metaData) {
        Class<DefaultSoulContextBuilder> clazz = DefaultSoulContextBuilder.class;
        Method method = clazz.getDeclaredMethod(methodName, SoulContext.class, MetaData.class);
        method.setAccessible(true);
        method.invoke(this.defaultSoulContextBuilder, soulContext, metaData);
        assertEquals(soulContext.getModule(), metaData.getAppName());
        assertEquals(soulContext.getMethod(), metaData.getServiceName());
        assertEquals(soulContext.getRpcType(), metaData.getRpcType());
        assertEquals(soulContext.getContextPath(), metaData.getContextPath());
    }
}
