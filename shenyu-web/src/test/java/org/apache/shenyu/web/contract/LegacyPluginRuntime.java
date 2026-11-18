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

package org.apache.shenyu.web.contract;

import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.web.handler.ShenyuWebHandler;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Arrays;

/**
 * Drives the real legacy {@link ShenyuWebHandler} for plugin contract tests.
 *
 * <p>The handler owns the mutable chain cursor and the deferred root subscription. The fixture
 * deliberately delegates to it instead of copying the cursor implementation, so a test observes
 * the same construction and subscription boundaries as the running gateway.</p>
 */
public final class LegacyPluginRuntime {

    private final ShenyuWebHandler handler;

    /**
     * Creates a runtime with the supplied plugin order.
     *
     * <p>The loader service is not consulted while handling a request; it is only used for
     * runtime extension-plugin events, which this fixture does not exercise.</p>
     *
     * @param plugins plugins in their declared order
     */
    public LegacyPluginRuntime(final ShenyuPlugin... plugins) {
        this.handler = new ShenyuWebHandler(Arrays.asList(plugins), null, new ShenyuConfig());
    }

    /**
     * Creates an exchange suitable for the fixture.
     *
     * @return a mock exchange with a minimal HTTP context
     */
    public static ServerWebExchange newExchange() {
        final ServerWebExchange exchange = MockServerWebExchange.from(
                MockServerHttpRequest.get("http://localhost/contract").build());
        final ShenyuContext context = new ShenyuContext();
        context.setPath("/contract");
        context.setRpcType(RpcTypeEnum.HTTP.getName());
        exchange.getAttributes().put(Constants.CONTEXT, context);
        return exchange;
    }

    /**
     * Handles an exchange through the real legacy chain.
     *
     * @param exchange request exchange
     * @return the deferred handler publisher
     */
    public Mono<Void> handle(final ServerWebExchange exchange) {
        return handler.handle(exchange);
    }
}
