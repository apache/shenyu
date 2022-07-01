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

package org.apache.shenyu.plugin.logging.elasticsearch.body;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.plugin.api.RemoteAddressResolver;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.utils.HostAddressUtils;
import org.apache.shenyu.plugin.logging.elasticsearch.collector.DefaultLogCollector;
import org.apache.shenyu.plugin.logging.elasticsearch.constant.LoggingConstant;
import org.apache.shenyu.plugin.logging.elasticsearch.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.logging.elasticsearch.utils.LogCollectUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.http.server.reactive.MockServerHttpResponse;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.net.InetSocketAddress;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.ByteBuffer;
import java.time.LocalDateTime;

/**
 * The Test Case For LoggingElasticSearchServerHttpResponse.
 */
public final class LoggingElasticSearchServerHttpResponseTest {

    private ShenyuRequestLog requestInfo = new ShenyuRequestLog();

    private ServerWebExchange exchange;

    private LoggingElasticSearchServerResponse loggingElasticSearchServerResponse;

    private ServerHttpRequest serverHttpRequest;

    private String userAgent = "User-Agent";

    private String host = "Host";

    private LocalDateTime startDateTime = LocalDateTime.now();

    private MockServerHttpResponse response;

    @BeforeEach
    public void setUp() throws URISyntaxException {
        MockServerHttpRequest request = MockServerHttpRequest
                .post("localhost")
                .remoteAddress(new InetSocketAddress(8090))
                .header("X-source", "mock test")
                .queryParam("queryParam", "Hello,World")
                .body("hello");
        ConfigurableApplicationContext context = Mockito.mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setApplicationContext(context);
        RemoteAddressResolver remoteAddressResolver = new RemoteAddressResolver() {
        };
        ShenyuResult shenyuResult = new ShenyuResult() {
        };
        this.exchange = Mockito.spy(MockServerWebExchange.from(request));
        Mockito.lenient().when(context.getBean(RemoteAddressResolver.class)).thenReturn(remoteAddressResolver);
        Mockito.lenient().when(context.getBean(ShenyuResult.class)).thenReturn(shenyuResult);
        ShenyuContext shenyuContext1 = new ShenyuContext();
        shenyuContext1.setStartDateTime(startDateTime);
        shenyuContext1.setMethod("test");
        exchange.getAttributes().put(Constants.CONTEXT, shenyuContext1);
        exchange.getAttributes().put(LoggingConstant.SHENYU_AGENT_TRACE_ID, "shenyu-agent-trace-id");
        this.serverHttpRequest = exchange.getRequest();
        requestInfo.setRequestUri(serverHttpRequest.getURI().toString());
        requestInfo.setMethod(serverHttpRequest.getMethodValue());
        requestInfo.setRequestHeader(LogCollectUtils.getHeaders(serverHttpRequest.getHeaders()));
        requestInfo.setQueryParams(serverHttpRequest.getURI().getQuery());
        requestInfo.setClientIp(HostAddressUtils.acquireIp(exchange));
        requestInfo.setUserAgent(serverHttpRequest.getHeaders().getFirst(userAgent));
        requestInfo.setHost(serverHttpRequest.getHeaders().getFirst(host));
        requestInfo.setPath(serverHttpRequest.getURI().getPath());
        this.loggingElasticSearchServerResponse = new LoggingElasticSearchServerResponse(exchange.getResponse(), requestInfo, DefaultLogCollector.getInstance());
        response = new MockServerHttpResponse();
    }

    @Test
    public void testWriteWith() throws Exception {
        loggingElasticSearchServerResponse.setExchange(exchange);
        loggingElasticSearchServerResponse.writeWith(response.getBody());
        Field field = loggingElasticSearchServerResponse.getClass().getDeclaredField("logInfo");
        field.setAccessible(true);
        ShenyuRequestLog logInfo = (ShenyuRequestLog) field.get(loggingElasticSearchServerResponse);
        Assertions.assertEquals(logInfo.getResponseHeader(), "{}");
    }

    @Test
    public void testSetExchange() throws Exception {
        loggingElasticSearchServerResponse.setExchange(exchange);
        Field field = loggingElasticSearchServerResponse.getClass().getDeclaredField("exchange");
        field.setAccessible(true);
        Assertions.assertEquals(field.get(loggingElasticSearchServerResponse), exchange);
    }

    @Test
    public void testGetTraceId() throws Exception {
        loggingElasticSearchServerResponse.setExchange(exchange);
        exchange.getResponse().getHeaders();
        Method method = loggingElasticSearchServerResponse.getClass().getDeclaredMethod("getTraceId");
        method.setAccessible(true);
        String traceId = (String) method.invoke(loggingElasticSearchServerResponse);
        Assertions.assertEquals(traceId, "shenyu-agent-trace-id");
    }

    @Test
    public void testGetUpstreamIp() throws Exception {
        loggingElasticSearchServerResponse.setExchange(exchange);
        Method method1 = loggingElasticSearchServerResponse.getClass().getDeclaredMethod("getUpstreamIp");
        method1.setAccessible(true);
        String upstreamIp1 = (String) method1.invoke(loggingElasticSearchServerResponse);
        Assertions.assertEquals(upstreamIp1, "");
        exchange.getAttributes().put(Constants.HTTP_DOMAIN, "http://localhost:9195/http/order/path/123/name");
        loggingElasticSearchServerResponse.setExchange(exchange);
        Method method2 = loggingElasticSearchServerResponse.getClass().getDeclaredMethod("getUpstreamIp");
        method2.setAccessible(true);
        String upstreamIp2 = (String) method2.invoke(loggingElasticSearchServerResponse);
        Assertions.assertEquals(upstreamIp2, "localhost");
        ShenyuContext shenyuContext2 = new ShenyuContext();
        shenyuContext2.setRpcType("http");
        exchange.getAttributes().put(Constants.CONTEXT, shenyuContext2);
        loggingElasticSearchServerResponse.setExchange(exchange);
        Method method3 = loggingElasticSearchServerResponse.getClass().getDeclaredMethod("getUpstreamIp");
        method3.setAccessible(true);
        String upstreamIp3 = (String) method3.invoke(loggingElasticSearchServerResponse);
        Assertions.assertEquals(upstreamIp3, "localhost");
        exchange.getAttributes().put(Constants.HTTP_URI, new URI("test", "localhost", "/test", "test"));
        loggingElasticSearchServerResponse.setExchange(exchange);
        Method method4 = loggingElasticSearchServerResponse.getClass().getDeclaredMethod("getUpstreamIp");
        method4.setAccessible(true);
        String uri = (String) method4.invoke(loggingElasticSearchServerResponse);
        Assertions.assertEquals(uri, "localhost");
    }

    @Test
    public void testGetUpstreamIpFromHttpDomain() throws Exception {
        exchange.getAttributes().put(Constants.HTTP_DOMAIN, "http://localhost:9195/http/order/path/123/name");
        loggingElasticSearchServerResponse.setExchange(exchange);
        Method method1 = loggingElasticSearchServerResponse.getClass().getDeclaredMethod("getUpstreamIpFromHttpDomain");
        method1.setAccessible(true);
        String upstreamIpFromHttpDomain1 = (String) method1.invoke(loggingElasticSearchServerResponse);
        Assertions.assertEquals(upstreamIpFromHttpDomain1, "localhost");
        exchange = Mockito.mock(ServerWebExchange.class);
        ShenyuContext shenyuContext2 = new ShenyuContext();
        shenyuContext2.setRpcType("http");
        shenyuContext2.setStartDateTime(startDateTime);
        exchange.getAttributes().put(Constants.CONTEXT, shenyuContext2);
        loggingElasticSearchServerResponse.setExchange(exchange);
        Method method2 = loggingElasticSearchServerResponse.getClass().getDeclaredMethod("getUpstreamIpFromHttpDomain");
        method2.setAccessible(true);
        String upstreamIpFromHttpDomain2 = (String) method2.invoke(loggingElasticSearchServerResponse);
        Assertions.assertEquals(upstreamIpFromHttpDomain2, "");
    }

    @Test
    public void testLogError() throws NoSuchFieldException, IllegalAccessException {
        ShenyuContext shenyuContext2 = new ShenyuContext();
        shenyuContext2.setRpcType("http");
        shenyuContext2.setStartDateTime(startDateTime);
        exchange.getAttributes().put(Constants.CONTEXT, shenyuContext2);
        loggingElasticSearchServerResponse.setExchange(exchange);
        Throwable throwable = new Throwable("error");
        DefaultLogCollector.getInstance().start();
        loggingElasticSearchServerResponse.logError(throwable);
        Field field = loggingElasticSearchServerResponse.getClass().getDeclaredField("logInfo");
        field.setAccessible(true);
        ShenyuRequestLog shenyuRequestLog = (ShenyuRequestLog) field.get(loggingElasticSearchServerResponse);
        Assertions.assertEquals(shenyuRequestLog.getStatus(), 500);
    }

    @Test
    public void testLogResponse() throws Exception {
        DefaultLogCollector.getInstance().start();
        loggingElasticSearchServerResponse.setExchange(exchange);
        BodyWriter writer = new BodyWriter();
        String sendString = "hello, shenyu";
        ByteBuffer byteBuffer = ByteBuffer.wrap(sendString.getBytes("UTF-8"));
        writer.write(byteBuffer.asReadOnlyBuffer());
        Method method1 = loggingElasticSearchServerResponse.getClass().getDeclaredMethod("logResponse", ShenyuContext.class, BodyWriter.class);
        method1.setAccessible(true);
        method1.invoke(loggingElasticSearchServerResponse, exchange.getAttribute(Constants.CONTEXT), writer);
        Field field1 = loggingElasticSearchServerResponse.getClass().getDeclaredField("logInfo");
        field1.setAccessible(true);
        ShenyuRequestLog log1 = (ShenyuRequestLog) field1.get(loggingElasticSearchServerResponse);
        Assertions.assertEquals(log1.getResponseBody(), "hello, shenyu");
        ShenyuContext shenyuContext2 = new ShenyuContext();
        shenyuContext2.setRpcType("http");
        shenyuContext2.setStartDateTime(startDateTime);
        exchange.getAttributes().put(Constants.CONTEXT, shenyuContext2);
        exchange.getAttributes().put(Constants.HTTP_DOMAIN, "http://localhost:9195/http/order/path/123/name");
        loggingElasticSearchServerResponse.setExchange(exchange);
        Method method2 = loggingElasticSearchServerResponse.getClass().getDeclaredMethod("logResponse", ShenyuContext.class, BodyWriter.class);
        method2.setAccessible(true);
        method2.invoke(loggingElasticSearchServerResponse, exchange.getAttribute(Constants.CONTEXT), writer);
        Field field2 = loggingElasticSearchServerResponse.getClass().getDeclaredField("logInfo");
        field2.setAccessible(true);
        ShenyuRequestLog log2 = (ShenyuRequestLog) field2.get(loggingElasticSearchServerResponse);
        Assertions.assertEquals(log2.getUpstreamIp(), "localhost");
    }
}
