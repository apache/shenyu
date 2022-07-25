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

package org.apache.shenyu.plugin.logging.common.entity;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.time.LocalDateTime;

/**
 * The Test Case For ShenyuRequestLog.
 */
public class ShenyuRequestLogTest {

    private final ShenyuRequestLog shenyuRequestLog = new ShenyuRequestLog();

    @Test
    public void testGetModule() {
        shenyuRequestLog.setModule("test");
        Assertions.assertEquals(shenyuRequestLog.getModule(), "test");
    }

    @Test
    public void testResponseContentLength() {
        shenyuRequestLog.setResponseContentLength(5);
        Assertions.assertEquals(shenyuRequestLog.getResponseContentLength(), 5);
    }

    @Test
    public void testGetUserAgent() {
        shenyuRequestLog.setUserAgent("test");
        Assertions.assertEquals(shenyuRequestLog.getUserAgent(), "test");
    }

    @Test
    public void testGetHost() {
        shenyuRequestLog.setHost("test");
        Assertions.assertEquals(shenyuRequestLog.getHost(), "test");
    }

    @Test
    public void testGetClientIp() {
        shenyuRequestLog.setClientIp("0.0.0.0");
        Assertions.assertEquals(shenyuRequestLog.getClientIp(), "0.0.0.0");
    }

    @Test
    public void testGetTimeLocal() {
        LocalDateTime timeLocal = LocalDateTime.now();
        shenyuRequestLog.setTimeLocal(timeLocal.toString());
        Assertions.assertEquals(shenyuRequestLog.getTimeLocal(), timeLocal.toString());
    }

    @Test
    public void testGetMethod() {
        shenyuRequestLog.setMethod("test");
        Assertions.assertEquals(shenyuRequestLog.getMethod(), "test");
    }

    @Test
    public void testGetRequestBody() {
        shenyuRequestLog.setRequestBody("hello");
        Assertions.assertEquals(shenyuRequestLog.getRequestBody(), "hello");
    }

    @Test
    public void testGetUpstreamIp() {
        shenyuRequestLog.setUpstreamIp("0.0.0.0");
        Assertions.assertEquals(shenyuRequestLog.getUpstreamIp(), "0.0.0.0");
    }

    @Test
    public void testRequestHeader() {
        shenyuRequestLog.setRequestHeader("{}");
        Assertions.assertEquals(shenyuRequestLog.getRequestHeader(), "{}");
    }

    @Test
    public void testQueryParams() {
        shenyuRequestLog.setQueryParams("{\"id\"=\"1\"}");
        Assertions.assertEquals(shenyuRequestLog.getQueryParams(), "{\"id\"=\"1\"}");
    }

    @Test
    public void testRequestUri() {
        shenyuRequestLog.setRequestUri("/http/post/hi");
        Assertions.assertEquals(shenyuRequestLog.getRequestUri(), "/http/post/hi");
    }

    @Test
    public void testRpcType() {
        shenyuRequestLog.setRpcType("Dubbo");
        Assertions.assertEquals(shenyuRequestLog.getRpcType(), "Dubbo");
    }

    @Test
    public void testUpstreamResponseTime() {
        shenyuRequestLog.setUpstreamResponseTime(111111);
        Assertions.assertEquals(shenyuRequestLog.getUpstreamResponseTime(), 111111);
    }

    @Test
    public void testTraceId() {
        shenyuRequestLog.setTraceId("shenyu-agent-trace-id");
        Assertions.assertEquals(shenyuRequestLog.getTraceId(), "shenyu-agent-trace-id");
    }

    @Test
    public void testPath() {
        shenyuRequestLog.setPath("/shenyu/plugin");
        Assertions.assertEquals(shenyuRequestLog.getPath(), "/shenyu/plugin");
    }
}
