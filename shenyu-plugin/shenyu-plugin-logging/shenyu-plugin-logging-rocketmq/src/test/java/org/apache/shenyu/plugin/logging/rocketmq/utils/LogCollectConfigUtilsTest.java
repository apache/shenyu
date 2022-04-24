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

package org.apache.shenyu.plugin.logging.rocketmq.utils;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.logging.rocketmq.config.LogCollectConfig.GlobalLogConfig;
import org.apache.shenyu.plugin.logging.rocketmq.sampler.Sampler;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;

import java.lang.reflect.Field;
import java.net.InetSocketAddress;
import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * The Test Case For LogCollectConfigUtils.
 */

public class LogCollectConfigUtilsTest {

    private GlobalLogConfig config = new GlobalLogConfig();

    private ServerWebExchange exchange;

    private ServerHttpRequest request;

    private Map<String, String> uriSampleMap = new HashMap<>();

    private Map<String, String> apiTopicMap = new HashMap<>();

    @BeforeEach
    public void setUp() {
        config.setBufferQueueSize(5000);
        LogCollectConfigUtils.setGlobalConfig(config);
        uriSampleMap.put("const", "1");
        apiTopicMap.put("topic", "shenyu-access-logging");
        MockServerHttpRequest request = MockServerHttpRequest
                .get("localhost")
                .remoteAddress(new InetSocketAddress(8090))
                .header("X-source", "mock test")
                .queryParam("queryParam", "Hello,World")
                .build();
        this.exchange = Mockito.spy(MockServerWebExchange.from(request));
        ShenyuContext shenyuContext = Mockito.mock(ShenyuContext.class);
        exchange.getAttributes().put(Constants.CONTEXT, shenyuContext);
        this.request = exchange.getRequest();
    }

    @Test
    public void testGetGlobalConfig() {
        GlobalLogConfig globalLogConfig = LogCollectConfigUtils.getGlobalLogConfig();
        assertEquals(globalLogConfig.getClass(), GlobalLogConfig.class);
    }

    @Test
    public void testSetGlobalConfig() {
        assertEquals(LogCollectConfigUtils.getGlobalLogConfig().getBufferQueueSize(), 5000);
    }

    @Test
    public void testSetSampler() throws IllegalAccessException, NoSuchFieldException {
        LogCollectConfigUtils.setSampler(uriSampleMap);
        Field field = LogCollectConfigUtils.class.getDeclaredField("apiSamplerMap");
        field.setAccessible(true);
        Assertions.assertEquals(field.get("const").toString(), "{const=" + Sampler.ALWAYS_SAMPLE + "}");
    }

    @Test
    public void testIsSampled() {
        assertEquals(LogCollectConfigUtils.isSampled(request), false);
    }

    @Test
    public void testIsRequestBodyTooLarge() {
        assertEquals(LogCollectConfigUtils.isRequestBodyTooLarge(524289), true);
        assertEquals(LogCollectConfigUtils.isRequestBodyTooLarge(524288), false);
    }

    @Test
    public void testIsResponseBodyTooLarge() {
        assertEquals(LogCollectConfigUtils.isResponseBodyTooLarge(524289), true);
        assertEquals(LogCollectConfigUtils.isResponseBodyTooLarge(524288), false);
    }

    @Test
    public void testGetTopic() {
        assertEquals(LogCollectConfigUtils.getTopic("topic"), "");
    }

    @Test
    public void testSetTopic() {
        LogCollectConfigUtils.setTopic(apiTopicMap);
        assertEquals(LogCollectConfigUtils.getTopic("topic"), "shenyu-access-logging");
    }

    @Test
    public void testSetGlobalSampler() throws NoSuchFieldException, IllegalAccessException {
        LogCollectConfigUtils.setGlobalSampler("0");
        Field field = LogCollectConfigUtils.class.getDeclaredField("globalSampler");
        field.setAccessible(true);
        assertEquals(field.get("const"), Sampler.NEVER_SAMPLE);
    }
}
