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
import org.junit.jupiter.api.Test;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Behavior pin for the varargs {@link ShenyuPlugin#skip} overloads (#8424).
 */
public class ShenyuPluginTest {

    private final ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/x").build());

    @Test
    public void skipMatchesNoRpcType() {
        assertSkipping(RpcTypeEnum.DUBBO, RpcTypeEnum.GRPC, false, RpcTypeEnum.HTTP, RpcTypeEnum.SPRING_CLOUD);
        assertSkipping(RpcTypeEnum.DUBBO, RpcTypeEnum.GRPC, false, RpcTypeEnum.SOFA);
    }

    @Test
    public void skipMatchesRpcType() {
        assertSkipping(RpcTypeEnum.DUBBO, RpcTypeEnum.GRPC, true, RpcTypeEnum.DUBBO);
        assertSkipping(RpcTypeEnum.SOFA, RpcTypeEnum.GRPC, true, RpcTypeEnum.SOFA, RpcTypeEnum.GRPC);
    }

    @Test
    public void skipWithEmptyVarargsIsFalse() {
        assertSkipping(RpcTypeEnum.DUBBO, RpcTypeEnum.GRPC, false);
    }

    @Test
    public void skipExceptIsTheNegation() {
        ShenyuPlugin plugin = stubPlugin();
        withContext(RpcTypeEnum.DUBBO);
        assertFalse(plugin.skipExcept(exchange, RpcTypeEnum.DUBBO));
        assertTrue(plugin.skipExcept(exchange, RpcTypeEnum.HTTP));
        assertTrue(plugin.skipExceptHttpLike(exchange));
        assertFalse(plugin.skip(exchange));
    }

    private void assertSkipping(final RpcTypeEnum exchangeType, final RpcTypeEnum ignored,
                                final boolean expected, final RpcTypeEnum... rpcTypes) {
        withContext(exchangeType);
        ShenyuPlugin plugin = stubPlugin();
        assertEquals(expected, plugin.skip(exchange, rpcTypes));
    }

    private ShenyuPlugin stubPlugin() {
        return new ShenyuPlugin() {
            @Override
            public Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
                return chain.execute(exchange);
            }

            @Override
            public int getOrder() {
                return 0;
            }
        };
    }

    private void withContext(final RpcTypeEnum exchangeType) {
        ShenyuContext shenyuContext = new ShenyuContext();
        shenyuContext.setRpcType(exchangeType.getName());
        exchange.getAttributes().put(Constants.CONTEXT, shenyuContext);
    }
}
