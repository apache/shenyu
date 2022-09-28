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

package org.apache.shenyu.plugin.logging.common.utils;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.logging.common.config.GenericGlobalConfig;
import org.apache.shenyu.plugin.logging.common.sampler.Sampler;
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
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * The Test Case For LogCollectConfigUtils.
 */
public class LogCollectConfigUtilsTest {

    private final GenericGlobalConfig config = new GenericGlobalConfig();
    
    private ServerHttpRequest request;

    @BeforeEach
    public void setUp() {
        config.setBufferQueueSize(5000);
        MockServerHttpRequest request = MockServerHttpRequest
                .get("localhost")
                .remoteAddress(new InetSocketAddress(8090))
                .header("X-source", "mock test")
                .queryParam("queryParam", "Hello,World")
                .build();
        ServerWebExchange exchange = Mockito.spy(MockServerWebExchange.from(request));
        ShenyuContext shenyuContext = Mockito.mock(ShenyuContext.class);
        exchange.getAttributes().put(Constants.CONTEXT, shenyuContext);
        this.request = exchange.getRequest();
    }

    @Test
    public void testGetGenericGlobalConfig() {
        GenericGlobalConfig globalLogConfig = LogCollectConfigUtils.getGenericGlobalConfig();
        assertEquals(globalLogConfig.getClass(), GenericGlobalConfig.class);
    }

    @Test
    public void testSetGenericGlobalConfig() {
        assertEquals(LogCollectConfigUtils.getGenericGlobalConfig().getBufferQueueSize(), 5000);
    }

    @Test
    public void testSetSampler() throws IllegalAccessException, NoSuchFieldException {
        Map<String, String> uriSampleMap = new HashMap<>();
        uriSampleMap.put("const", "");
        LogCollectConfigUtils.setSampler(uriSampleMap);
        Field field1 = LogCollectConfigUtils.class.getDeclaredField("apiSamplerMap");
        field1.setAccessible(true);
        Assertions.assertEquals(field1.get("const").toString(), "{const=" + Sampler.ALWAYS_SAMPLE + "}");
        uriSampleMap.put("const", "1");
        LogCollectConfigUtils.setSampler(uriSampleMap);
        Field field2 = LogCollectConfigUtils.class.getDeclaredField("apiSamplerMap");
        field2.setAccessible(true);
        Assertions.assertEquals(field2.get("const").toString(), "{const=" + Sampler.ALWAYS_SAMPLE + "}");
    }

    @Test
    public void testIsSampled() {
        assertTrue(LogCollectConfigUtils.isSampled(request));
        Map<String, String> uriSampleMap = new HashMap<>();
        uriSampleMap.put("localhost", "1");
        LogCollectConfigUtils.setSampler(uriSampleMap);
        assertTrue(LogCollectConfigUtils.isSampled(request));
    }

    @Test
    public void testIsRequestBodyTooLarge() {
        LogCollectConfigUtils.setGenericGlobalConfig(null);
        assertFalse(LogCollectConfigUtils.isRequestBodyTooLarge(524289));
        assertFalse(LogCollectConfigUtils.isRequestBodyTooLarge(524288));
        LogCollectConfigUtils.setGenericGlobalConfig(config);
        assertTrue(LogCollectConfigUtils.isRequestBodyTooLarge(524289));
        assertFalse(LogCollectConfigUtils.isRequestBodyTooLarge(524288));
    }

    @Test
    public void testIsResponseBodyTooLarge() {
        LogCollectConfigUtils.setGenericGlobalConfig(null);
        assertFalse(LogCollectConfigUtils.isResponseBodyTooLarge(524289));
        assertFalse(LogCollectConfigUtils.isResponseBodyTooLarge(524288));
        LogCollectConfigUtils.setGenericGlobalConfig(config);
        assertTrue(LogCollectConfigUtils.isResponseBodyTooLarge(524289));
        assertFalse(LogCollectConfigUtils.isResponseBodyTooLarge(524288));
    }

    @Test
    public void testSetGlobalSampler() throws NoSuchFieldException, IllegalAccessException {
        LogCollectConfigUtils.setGlobalSampler("1");
        Field field = LogCollectConfigUtils.class.getDeclaredField("globalSampler");
        field.setAccessible(true);
        assertEquals(field.get("const"), Sampler.ALWAYS_SAMPLE);
    }
}
