/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License,  Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,  software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,  either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.shenyu.plugin.base.condition.data;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.HttpMethodEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.api.RemoteAddressResolver;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.junit.Assert;
import org.junit.Test;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.http.HttpCookie;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;

import java.net.InetSocketAddress;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link ParameterDataFactory}.
 */
public class ParameterDataFactoryTest {

    @Test
    public void testNewInstance() {

        Map<String, Class> parameterInstance = new HashMap<>();
        parameterInstance.put("header", HeaderParameterData.class);
        parameterInstance.put("cookie", CookieParameterData.class);
        parameterInstance.put("ip", IpParameterData.class);
        parameterInstance.put("host", HostParameterData.class);
        parameterInstance.put("uri", URIParameterData.class);
        parameterInstance.put("query", QueryParameterData.class);
        parameterInstance.put("post", PostParameterData.class);
        parameterInstance.put("req_method", RequestMethodParameterData.class);

        parameterInstance.forEach((key, clazz) -> {
            Assert.assertEquals(ParameterDataFactory.newInstance(key).getClass(), clazz);
        });
    }

    @Test
    public void testBuildHeaderData() {
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/http")
                .remoteAddress(new InetSocketAddress("localhost", 8080))
                .header("shenyu", "shenyuHeader")
                .build());
        Assert.assertEquals(ParameterDataFactory.builderData("header", "shenyu", exchange), "shenyuHeader");
    }

    @Test
    public void testBuildCookieData() {
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/http")
                .remoteAddress(new InetSocketAddress("localhost", 8080))
                .cookie(new HttpCookie("cookie-name", "cookie-value"))
                .build());
        Assert.assertEquals(ParameterDataFactory.builderData("cookie", "cookie-name", exchange), "cookie-value");
    }

    @Test
    public void testBuildHostData() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setApplicationContext(context);
        RemoteAddressResolver remoteAddressResolver = new RemoteAddressResolver() {
        };
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/http")
                .remoteAddress(new InetSocketAddress("localhost", 8085))
                .build());
        when(context.getBean(RemoteAddressResolver.class)).thenReturn(remoteAddressResolver);

        Assert.assertEquals(ParameterDataFactory.builderData("host", null, exchange), "localhost");
    }

    @Test
    public void testBuildIPData() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setApplicationContext(context);
        RemoteAddressResolver remoteAddressResolver = new RemoteAddressResolver() {
        };
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/http")
                .remoteAddress(new InetSocketAddress("127.0.0.1", 8080))
                .build());
        when(context.getBean(RemoteAddressResolver.class)).thenReturn(remoteAddressResolver);

        Assert.assertEquals(ParameterDataFactory.builderData("ip", null, exchange), "127.0.0.1");
    }

    @Test
    public void testBuildURIData() {
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/uri/path")
                .remoteAddress(new InetSocketAddress("localhost", 8080))
                .build());
        Assert.assertEquals(ParameterDataFactory.builderData("uri", null, exchange), "/uri/path");
    }

    @Test
    public void testBuildQueryData() {
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/uri/path")
                .queryParam("key", "value")
                .remoteAddress(new InetSocketAddress("localhost", 8080))
                .build());
        Assert.assertEquals(ParameterDataFactory.builderData("query", "key", exchange), "value");
    }

    @Test
    public void testBuildPostData() {
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/uri/path")
                .remoteAddress(new InetSocketAddress("localhost", 8080))
                .build());
        ShenyuContext context = new ShenyuContext();
        context.setRpcType(RpcTypeEnum.HTTP.getName());
        context.setHttpMethod(HttpMethodEnum.POST.getName());
        exchange.getAttributes().put(Constants.CONTEXT, context);

        Assert.assertEquals(ParameterDataFactory.builderData("post", "httpMethod", exchange), "post");
        Assert.assertEquals(ParameterDataFactory.builderData("post", "rpcType", exchange), "http");
    }

    @Test
    public void testBuildRequestMethodData() {
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/uri/path")
                .remoteAddress(new InetSocketAddress("localhost", 8080))
                .build());

        Assert.assertEquals(ParameterDataFactory.builderData("req_method", null, exchange), "GET");
    }
}
