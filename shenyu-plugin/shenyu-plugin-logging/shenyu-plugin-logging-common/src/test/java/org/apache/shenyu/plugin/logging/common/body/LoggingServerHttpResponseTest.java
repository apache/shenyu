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

package org.apache.shenyu.plugin.logging.common.body;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.plugin.api.RemoteAddressResolver;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.utils.HostAddressUtils;
import org.apache.shenyu.plugin.logging.common.collector.LogCollector;
import org.apache.shenyu.plugin.logging.common.constant.GenericLoggingConstant;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.logging.common.utils.LogCollectUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.net.InetSocketAddress;
import java.net.URI;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;

import static org.mockito.Mockito.mock;

/**
 * The Test Case For LoggingServerHttpResponse.
 */
public class LoggingServerHttpResponseTest {

    private final ShenyuRequestLog requestInfo = new ShenyuRequestLog();
    
    private final LocalDateTime startDateTime = LocalDateTime.now();
    
    private ServerWebExchange exchange;
    
    private LoggingServerHttpResponse loggingServerHttpResponse;

    private LogCollector logCollector;

    @BeforeEach
    public void setUp() {
        logCollector = mock(LogCollector.class);
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
        exchange.getAttributes().put(GenericLoggingConstant.SHENYU_AGENT_TRACE_ID, "shenyu-agent-trace-id");
        ServerHttpRequest serverHttpRequest = exchange.getRequest();
        requestInfo.setRequestUri(serverHttpRequest.getURI().toString());
        requestInfo.setMethod(serverHttpRequest.getMethodValue());
        requestInfo.setRequestHeader(LogCollectUtils.getHeaders(serverHttpRequest.getHeaders()));
        requestInfo.setQueryParams(serverHttpRequest.getURI().getQuery());
        requestInfo.setClientIp(HostAddressUtils.acquireIp(exchange));
        requestInfo.setUserAgent(serverHttpRequest.getHeaders().getFirst(GenericLoggingConstant.USER_AGENT));
        requestInfo.setHost(serverHttpRequest.getHeaders().getFirst(GenericLoggingConstant.HOST));
        requestInfo.setPath(serverHttpRequest.getURI().getPath());
        this.loggingServerHttpResponse = new LoggingServerHttpResponse(exchange.getResponse(), requestInfo, logCollector);
    }

    @Test
    public void testSetExchange() throws NoSuchFieldException, IllegalAccessException {
        loggingServerHttpResponse.setExchange(exchange);
        Field field = loggingServerHttpResponse.getClass().getDeclaredField("exchange");
        field.setAccessible(true);
        Assertions.assertEquals(field.get(loggingServerHttpResponse), exchange);
    }

    @Test
    public void testGetTraceId() throws Exception {
        loggingServerHttpResponse.setExchange(exchange);
        exchange.getResponse().getHeaders();
        Method method = loggingServerHttpResponse.getClass().getDeclaredMethod("getTraceId");
        method.setAccessible(true);
        String traceId = (String) method.invoke(loggingServerHttpResponse);
        Assertions.assertEquals(traceId, "shenyu-agent-trace-id");
    }

    @Test
    public void testGetUpstreamIp() throws Exception {
        loggingServerHttpResponse.setExchange(exchange);
        Method method1 = loggingServerHttpResponse.getClass().getDeclaredMethod("getUpstreamIp");
        method1.setAccessible(true);
        String upstreamIp1 = (String) method1.invoke(loggingServerHttpResponse);
        Assertions.assertEquals(upstreamIp1, "");
        exchange.getAttributes().put(Constants.HTTP_DOMAIN, "http://localhost:9195/http/order/path/123/name");
        loggingServerHttpResponse.setExchange(exchange);
        Method method2 = loggingServerHttpResponse.getClass().getDeclaredMethod("getUpstreamIp");
        method2.setAccessible(true);
        String upstreamIp2 = (String) method2.invoke(loggingServerHttpResponse);
        Assertions.assertEquals(upstreamIp2, "localhost");
        ShenyuContext shenyuContext2 = new ShenyuContext();
        shenyuContext2.setRpcType("http");
        exchange.getAttributes().put(Constants.CONTEXT, shenyuContext2);
        loggingServerHttpResponse.setExchange(exchange);
        Method method3 = loggingServerHttpResponse.getClass().getDeclaredMethod("getUpstreamIp");
        method3.setAccessible(true);
        String upstreamIp3 = (String) method3.invoke(loggingServerHttpResponse);
        Assertions.assertEquals(upstreamIp3, "localhost");
        exchange.getAttributes().put(Constants.HTTP_URI, new URI("test", "localhost", "/test", "test"));
        loggingServerHttpResponse.setExchange(exchange);
        Method method4 = loggingServerHttpResponse.getClass().getDeclaredMethod("getUpstreamIp");
        method4.setAccessible(true);
        String uri = (String) method4.invoke(loggingServerHttpResponse);
        Assertions.assertEquals(uri, "localhost");
    }

    @Test
    public void testGetUpstreamIpFromHttpDomain() throws Exception {
        exchange.getAttributes().put(Constants.HTTP_DOMAIN, "http://localhost:9195/http/order/path/123/name");
        loggingServerHttpResponse.setExchange(exchange);
        Method method1 = loggingServerHttpResponse.getClass().getDeclaredMethod("getUpstreamIpFromHttpDomain");
        method1.setAccessible(true);
        String upstreamIpFromHttpDomain1 = (String) method1.invoke(loggingServerHttpResponse);
        Assertions.assertEquals(upstreamIpFromHttpDomain1, "localhost");
        exchange = Mockito.mock(ServerWebExchange.class);
        ShenyuContext shenyuContext2 = new ShenyuContext();
        shenyuContext2.setRpcType("http");
        shenyuContext2.setStartDateTime(startDateTime);
        exchange.getAttributes().put(Constants.CONTEXT, shenyuContext2);
        loggingServerHttpResponse.setExchange(exchange);
        Method method2 = loggingServerHttpResponse.getClass().getDeclaredMethod("getUpstreamIpFromHttpDomain");
        method2.setAccessible(true);
        String upstreamIpFromHttpDomain2 = (String) method2.invoke(loggingServerHttpResponse);
        Assertions.assertEquals(upstreamIpFromHttpDomain2, "");
    }

    @Test
    public void testLogError() throws NoSuchFieldException, IllegalAccessException {
        loggingServerHttpResponse.setExchange(exchange);
        Throwable throwable = new Throwable("error");
        logCollector.start();
        // DefaultLogCollector.getInstance().start();
        loggingServerHttpResponse.logError(throwable);
        Field field = loggingServerHttpResponse.getClass().getDeclaredField("logInfo");
        field.setAccessible(true);
        ShenyuRequestLog shenyuRequestLog = (ShenyuRequestLog) field.get(loggingServerHttpResponse);
        Assertions.assertEquals(shenyuRequestLog.getStatus(), 500);
    }

    @Test
    public void testLogResponse() throws Exception {
        logCollector.start();
        // DefaultLogCollector.getInstance().start();
        loggingServerHttpResponse.setExchange(exchange);
        BodyWriter writer = new BodyWriter();
        String sendString = "hello, shenyu";
        ByteBuffer byteBuffer = ByteBuffer.wrap(sendString.getBytes(StandardCharsets.UTF_8));
        writer.write(byteBuffer.asReadOnlyBuffer());
        Method method1 = loggingServerHttpResponse.getClass().getDeclaredMethod("logResponse", ShenyuContext.class, BodyWriter.class);
        method1.setAccessible(true);
        method1.invoke(loggingServerHttpResponse, exchange.getAttribute(Constants.CONTEXT), writer);
        Field field1 = loggingServerHttpResponse.getClass().getDeclaredField("logInfo");
        field1.setAccessible(true);
        ShenyuRequestLog log1 = (ShenyuRequestLog) field1.get(loggingServerHttpResponse);
        Assertions.assertEquals(log1.getResponseBody(), "hello, shenyu");
        ShenyuContext shenyuContext2 = new ShenyuContext();
        shenyuContext2.setRpcType("http");
        shenyuContext2.setStartDateTime(startDateTime);
        exchange.getAttributes().put(Constants.CONTEXT, shenyuContext2);
        exchange.getAttributes().put(Constants.HTTP_DOMAIN, "http://localhost:9195/http/order/path/123/name");
        loggingServerHttpResponse.setExchange(exchange);
        Method method2 = loggingServerHttpResponse.getClass().getDeclaredMethod("logResponse", ShenyuContext.class, BodyWriter.class);
        method2.setAccessible(true);
        method2.invoke(loggingServerHttpResponse, exchange.getAttribute(Constants.CONTEXT), writer);
        Field field2 = loggingServerHttpResponse.getClass().getDeclaredField("logInfo");
        field2.setAccessible(true);
        ShenyuRequestLog log2 = (ShenyuRequestLog) field2.get(loggingServerHttpResponse);
        Assertions.assertEquals(log2.getUpstreamIp(), "localhost");
    }
}
