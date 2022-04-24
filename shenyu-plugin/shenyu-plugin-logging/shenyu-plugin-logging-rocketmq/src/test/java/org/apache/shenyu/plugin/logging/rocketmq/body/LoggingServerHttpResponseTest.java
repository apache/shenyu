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

package org.apache.shenyu.plugin.logging.rocketmq.body;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.plugin.api.RemoteAddressResolver;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.utils.HostAddressUtils;
import org.apache.shenyu.plugin.logging.rocketmq.DefaultLogCollector;
import org.apache.shenyu.plugin.logging.rocketmq.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.logging.rocketmq.constant.LoggingConstant;
import org.apache.shenyu.plugin.logging.rocketmq.utils.LogCollectUtils;
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
import java.nio.ByteBuffer;
import java.time.LocalDateTime;

/**
 * The Test Case For LoggingServerHttpResponse.
 */
public class LoggingServerHttpResponseTest {

    private ShenyuRequestLog requestInfo = new ShenyuRequestLog();

    private ServerWebExchange exchange;

    private LoggingServerHttpResponse loggingServerHttpResponse;

    private ServerHttpRequest serverHttpRequest;

    private String userAgent = "User-Agent";

    private String host = "Host";

    private LocalDateTime startDateTime = LocalDateTime.now();

    @BeforeEach
    public void setUp() {
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
        ShenyuContext shenyuContext = new ShenyuContext();
        shenyuContext.setStartDateTime(startDateTime);
        exchange.getAttributes().put(Constants.CONTEXT, shenyuContext);
        exchange.getAttributes().put(LoggingConstant.SHENYU_AGENT_TRACE_ID, "shenyu-agent-trace-id");
        exchange.getAttributes().put(Constants.HTTP_DOMAIN, "http://localhost:9195/http/order/path/123/name");
        this.serverHttpRequest = exchange.getRequest();
        requestInfo.setRequestUri(serverHttpRequest.getURI().toString());
        requestInfo.setMethod(serverHttpRequest.getMethodValue());
        requestInfo.setRequestHeader(LogCollectUtils.getHeaders(serverHttpRequest.getHeaders()));
        requestInfo.setQueryParams(serverHttpRequest.getURI().getQuery());
        requestInfo.setClientIp(HostAddressUtils.acquireIp(exchange));
        requestInfo.setUserAgent(serverHttpRequest.getHeaders().getFirst(userAgent));
        requestInfo.setHost(serverHttpRequest.getHeaders().getFirst(host));
        requestInfo.setPath(serverHttpRequest.getURI().getPath());
        this.loggingServerHttpResponse = new LoggingServerHttpResponse(exchange.getResponse(), requestInfo, DefaultLogCollector.getInstance());
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
        Method method = loggingServerHttpResponse.getClass().getDeclaredMethod("getUpstreamIp");
        method.setAccessible(true);
        String upstreamIp = (String) method.invoke(loggingServerHttpResponse);
        Assertions.assertEquals(upstreamIp, "localhost");
    }

    @Test
    public void testGetUpstreamIpFromHttpDomain() throws Exception {
        loggingServerHttpResponse.setExchange(exchange);
        Method method = loggingServerHttpResponse.getClass().getDeclaredMethod("getUpstreamIpFromHttpDomain");
        method.setAccessible(true);
        String upstreamIpFromHttpDomain = (String) method.invoke(loggingServerHttpResponse);
        Assertions.assertEquals(upstreamIpFromHttpDomain, "localhost");
    }

    @Test
    public void testLogError() throws NoSuchFieldException, IllegalAccessException {
        loggingServerHttpResponse.setExchange(exchange);
        Throwable throwable = new Throwable("error");
        DefaultLogCollector.getInstance().start();
        loggingServerHttpResponse.logError(throwable);
        Field field = loggingServerHttpResponse.getClass().getDeclaredField("logInfo");
        field.setAccessible(true);
        ShenyuRequestLog shenyuRequestLog = (ShenyuRequestLog) field.get(loggingServerHttpResponse);
        Assertions.assertEquals(shenyuRequestLog.getStatus(), 500);
    }

    @Test
    public void testLogResponse() throws Exception {
        DefaultLogCollector.getInstance().start();
        loggingServerHttpResponse.setExchange(exchange);
        BodyWriter writer = new BodyWriter();
        String sendString = "hello, shenyu";
        ByteBuffer byteBuffer = ByteBuffer.wrap(sendString.getBytes("UTF-8"));
        writer.write(byteBuffer.asReadOnlyBuffer());
        Method method = loggingServerHttpResponse.getClass().getDeclaredMethod("logResponse", ShenyuContext.class, BodyWriter.class);
        method.setAccessible(true);
        method.invoke(loggingServerHttpResponse, exchange.getAttribute(Constants.CONTEXT), writer);
        Field field = loggingServerHttpResponse.getClass().getDeclaredField("logInfo");
        field.setAccessible(true);
        ShenyuRequestLog log = (ShenyuRequestLog) field.get(loggingServerHttpResponse);
        Assertions.assertEquals(log.getResponseBody(), "hello, shenyu");
    }
}
