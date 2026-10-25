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
import org.apache.shenyu.plugin.logging.common.handler.AbstractLogPluginDataHandler;
import org.apache.shenyu.plugin.logging.common.sampler.Sampler;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;

import java.lang.reflect.Field;
import java.net.InetSocketAddress;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
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
    public void testSetGlobalSampler() throws NoSuchFieldException, IllegalAccessException {
        LogCollectConfigUtils.setGlobalSampler("1");
        Field field = LogCollectConfigUtils.class.getDeclaredField("globalSampler");
        field.setAccessible(true);
        assertEquals(field.get("const"), Sampler.ALWAYS_SAMPLE);
    }

    @Test
    public void testGetGlobalConfigUsesPluginId() {
        String pluginId = "config-utils-body-limit-test";
        GenericGlobalConfig pluginConfig = new GenericGlobalConfig();
        pluginConfig.setMaxRequestBody(8);
        pluginConfig.setMaxResponseBody(16);
        AbstractLogPluginDataHandler.getPluginGlobalConfigMap().put(pluginId, pluginConfig);
        try {
            assertSame(pluginConfig, LogCollectConfigUtils.getGlobalConfig(pluginId));
        } finally {
            AbstractLogPluginDataHandler.getPluginGlobalConfigMap().remove(pluginId);
        }
    }

    @Test
    public void testMissingConfigDoesNotShareMutableDefaults() {
        String pluginId = "missing-body-limit-test";
        GenericGlobalConfig missing = LogCollectConfigUtils.getGlobalConfig(pluginId);
        assertEquals(524288, missing.getMaxRequestBody());
        assertEquals(524288, missing.getMaxResponseBody());
        missing.setMaxRequestBody(1);
        missing.setMaxResponseBody(1);
        GenericGlobalConfig next = LogCollectConfigUtils.getGlobalConfig(pluginId);
        assertEquals(524288, next.getMaxRequestBody());
        assertEquals(524288, next.getMaxResponseBody());
    }

    @Test
    public void testNullPluginIdUsesDefaults() {
        GenericGlobalConfig missing = LogCollectConfigUtils.getGlobalConfig(null);
        assertEquals(524288, missing.getMaxRequestBody());
        assertEquals(524288, missing.getMaxResponseBody());
    }

    @Test
    public void testLegacySizeChecksUseDefaultLimits() {
        assertFalse(LogCollectConfigUtils.isRequestBodyTooLarge(524288));
        assertTrue(LogCollectConfigUtils.isRequestBodyTooLarge(524289));
        assertFalse(LogCollectConfigUtils.isResponseBodyTooLarge(524288));
        assertTrue(LogCollectConfigUtils.isResponseBodyTooLarge(524289));
    }
}
