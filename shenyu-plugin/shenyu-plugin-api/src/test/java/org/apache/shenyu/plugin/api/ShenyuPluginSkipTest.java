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

package org.apache.shenyu.plugin.api;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

/**
 * Tests the default plugin RPC type skip behavior.
 */
class ShenyuPluginSkipTest {

    private final ShenyuPlugin plugin = new ShenyuPlugin() {
        @Override
        public Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
            return Mono.empty();
        }

        @Override
        public int getOrder() {
            return 0;
        }
    };

    private final ServerWebExchange exchange = mock(ServerWebExchange.class);

    private final ShenyuContext context = mock(ShenyuContext.class);

    @BeforeEach
    void setUp() {
        when(exchange.getAttribute(Constants.CONTEXT)).thenReturn(context);
    }

    @ParameterizedTest
    @ValueSource(ints = {0, 1, 2})
    void testSkipMatchesAtEachPosition(final int position) {
        final RpcTypeEnum[] rpcTypes = {RpcTypeEnum.DUBBO, RpcTypeEnum.GRPC, RpcTypeEnum.SOFA};
        rpcTypes[position] = RpcTypeEnum.HTTP;
        when(context.getRpcType()).thenReturn(RpcTypeEnum.HTTP.getName());

        assertTrue(plugin.skip(exchange, rpcTypes));
        verifyRpcTypeReads(position + 1);
    }

    @Test
    void testSkipWithNoMatch() {
        when(context.getRpcType()).thenReturn(RpcTypeEnum.HTTP.getName());

        assertFalse(plugin.skip(exchange, RpcTypeEnum.DUBBO, RpcTypeEnum.GRPC, RpcTypeEnum.SOFA));
        verifyRpcTypeReads(3);
    }

    @Test
    void testSkipWithEmptyTypesDoesNotReadExchange() {
        assertFalse(plugin.skip(exchange, new RpcTypeEnum[0]));
        assertFalse(plugin.skip(null, new RpcTypeEnum[0]));
        verifyNoInteractions(exchange, context);
    }

    @Test
    void testSkipWithNullTypesDoesNotReadExchange() {
        assertFalse(plugin.skip(exchange, (RpcTypeEnum[]) null));
        assertFalse(plugin.skip(null, (RpcTypeEnum[]) null));
        verifyNoInteractions(exchange, context);
    }

    @Test
    void testSkipWithMissingContextThrows() {
        when(exchange.getAttribute(Constants.CONTEXT)).thenReturn(null);

        assertThrows(NullPointerException.class, () -> plugin.skip(exchange, RpcTypeEnum.HTTP));
        verify(exchange).getAttribute(Constants.CONTEXT);
        verifyNoMoreInteractions(exchange);
        verifyNoInteractions(context);
    }

    @Test
    void testSkipWithNullExchangeAndTypesPresentThrows() {
        assertThrows(NullPointerException.class, () -> plugin.skip(null, RpcTypeEnum.HTTP));
        verifyNoInteractions(exchange, context);
    }

    @Test
    void testSkipWithNullRpcTypeDoesNotMatch() {
        assertFalse(plugin.skip(exchange, RpcTypeEnum.HTTP, RpcTypeEnum.SPRING_CLOUD, RpcTypeEnum.AI));
        verifyRpcTypeReads(3);
    }

    @Test
    void testSkipDoesNotExamineNullAfterMatch() {
        when(context.getRpcType()).thenReturn(RpcTypeEnum.HTTP.getName());

        assertTrue(plugin.skip(exchange, RpcTypeEnum.HTTP, null));
        verifyRpcTypeReads(1);
    }

    @Test
    void testSkipWithNullBeforeMatchThrowsAfterReadingRpcType() {
        when(context.getRpcType()).thenReturn(RpcTypeEnum.HTTP.getName());

        assertThrows(NullPointerException.class, () -> plugin.skip(exchange, RpcTypeEnum.DUBBO, null, RpcTypeEnum.HTTP));
        verifyRpcTypeReads(2);
    }

    @Test
    void testSkipReadsRpcTypeBeforeDereferencingNullElement() {
        final IllegalStateException failure = new IllegalStateException("rpc type unavailable");
        when(context.getRpcType()).thenThrow(failure);

        assertSame(failure, assertThrows(IllegalStateException.class, () -> plugin.skip(exchange, (RpcTypeEnum) null)));
        verifyRpcTypeReads(1);
    }

    @Test
    void testSkipReadsRpcTypeAgainForEachElement() {
        when(context.getRpcType()).thenReturn(RpcTypeEnum.DUBBO.getName(), RpcTypeEnum.HTTP.getName());

        assertTrue(plugin.skip(exchange, RpcTypeEnum.HTTP, RpcTypeEnum.HTTP, null));
        verifyRpcTypeReads(2);
    }

    @Test
    void testSkipCapturesElementBeforeReadingRpcType() {
        final RpcTypeEnum[] rpcTypes = {RpcTypeEnum.HTTP};
        when(context.getRpcType()).thenAnswer(invocation -> {
            rpcTypes[0] = null;
            return RpcTypeEnum.HTTP.getName();
        });

        assertTrue(plugin.skip(exchange, rpcTypes));
        verifyRpcTypeReads(1);
    }

    @ParameterizedTest
    @CsvSource({"HTTP, false", "SPRING_CLOUD, false", "AI, false", "DUBBO, true"})
    void testSkipExcept(final RpcTypeEnum rpcType, final boolean expected) {
        when(context.getRpcType()).thenReturn(rpcType.getName());

        assertEquals(expected, plugin.skipExcept(exchange, RpcTypeEnum.HTTP, RpcTypeEnum.SPRING_CLOUD, RpcTypeEnum.AI));
    }

    @ParameterizedTest
    @CsvSource({"HTTP, false", "SPRING_CLOUD, false", "AI, false", "DUBBO, true"})
    void testSkipExceptHttpLike(final RpcTypeEnum rpcType, final boolean expected) {
        when(context.getRpcType()).thenReturn(rpcType.getName());

        assertEquals(expected, plugin.skipExceptHttpLike(exchange));
    }

    private void verifyRpcTypeReads(final int count) {
        verify(exchange).getAttribute(Constants.CONTEXT);
        verify(context, times(count)).getRpcType();
        verifyNoMoreInteractions(exchange, context);
    }
}
