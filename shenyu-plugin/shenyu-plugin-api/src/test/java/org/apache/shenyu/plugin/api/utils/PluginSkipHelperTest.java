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

package org.apache.shenyu.plugin.api.utils;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test for PluginSkipHelper.
 */
public final class PluginSkipHelperTest {

    private ServerWebExchange exchange;

    @BeforeEach
    public void setUp() {
        exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost").build());
    }

    @Test
    public void testSkip() {
        ShenyuContext context = mock(ShenyuContext.class);
        exchange.getAttributes().put(Constants.CONTEXT, context);

        // Match
        when(context.getRpcType()).thenReturn(RpcTypeEnum.HTTP.getName());
        assertTrue(PluginSkipHelper.skip(exchange, RpcTypeEnum.HTTP));

        // No match
        when(context.getRpcType()).thenReturn(RpcTypeEnum.DUBBO.getName());
        assertFalse(PluginSkipHelper.skip(exchange, RpcTypeEnum.HTTP));

        // Empty types
        assertFalse(PluginSkipHelper.skip(exchange));
    }

    @Test
    public void testSkipExcept() {
        ShenyuContext context = mock(ShenyuContext.class);
        exchange.getAttributes().put(Constants.CONTEXT, context);

        // Match (should not skip)
        when(context.getRpcType()).thenReturn(RpcTypeEnum.HTTP.getName());
        assertFalse(PluginSkipHelper.skipExcept(exchange, RpcTypeEnum.HTTP));

        // No match (should skip)
        when(context.getRpcType()).thenReturn(RpcTypeEnum.DUBBO.getName());
        assertTrue(PluginSkipHelper.skipExcept(exchange, RpcTypeEnum.HTTP));
    }

    @Test
    public void testSkipExceptHttpLike() {
        ShenyuContext context = mock(ShenyuContext.class);
        exchange.getAttributes().put(Constants.CONTEXT, context);

        // HTTP (should not skip)
        when(context.getRpcType()).thenReturn(RpcTypeEnum.HTTP.getName());
        assertFalse(PluginSkipHelper.skipExceptHttpLike(exchange));

        // Spring Cloud (should not skip)
        when(context.getRpcType()).thenReturn(RpcTypeEnum.SPRING_CLOUD.getName());
        assertFalse(PluginSkipHelper.skipExceptHttpLike(exchange));

        // Dubbo (should skip)
        when(context.getRpcType()).thenReturn(RpcTypeEnum.DUBBO.getName());
        assertTrue(PluginSkipHelper.skipExceptHttpLike(exchange));
    }
}
