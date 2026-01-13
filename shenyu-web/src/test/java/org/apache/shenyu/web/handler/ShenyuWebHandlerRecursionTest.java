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

package org.apache.shenyu.web.handler;

import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.web.loader.ShenyuLoaderService;
import org.junit.jupiter.api.Test;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.List;

import static org.mockito.Mockito.mock;

public class ShenyuWebHandlerRecursionTest {

    @Test
    public void testManySkippedPlugins() {
        int pluginCount = 10000;
        List<ShenyuPlugin> plugins = new ArrayList<>(pluginCount);

        // Add many skipped plugins
        for (int i = 0; i < pluginCount; i++) {
            plugins.add(new SkippedPlugin(i));
        }

        // Add one final executed plugin
        plugins.add(new ExecutedPlugin(pluginCount));

        ShenyuLoaderService shenyuLoaderService = mock(ShenyuLoaderService.class);
        ShenyuWebHandler handler = new ShenyuWebHandler(plugins, shenyuLoaderService, new ShenyuConfig());

        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost")
                .remoteAddress(new InetSocketAddress(8090))
                .build());
        exchange.getAttributes().put(Constants.CONTEXT, mock(ShenyuContext.class));

        Mono<Void> result = handler.handle(exchange);

        StepVerifier.create(result)
                .expectSubscription()
                .verifyComplete();
    }

    static class SkippedPlugin implements ShenyuPlugin {
        private final int order;

        SkippedPlugin(final int order) {
            this.order = order;
        }

        @Override
        public Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
            return chain.execute(exchange);
        }

        @Override
        public int getOrder() {
            return order;
        }

        @Override
        public String named() {
            return "skipped-" + order;
        }

        @Override
        public boolean skip(final ServerWebExchange exchange) {
            return true;
        }
    }

    static class ExecutedPlugin implements ShenyuPlugin {
        private final int order;

        ExecutedPlugin(final int order) {
            this.order = order;
        }

        @Override
        public Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
            return chain.execute(exchange);
        }

        @Override
        public int getOrder() {
            return order;
        }

        @Override
        public String named() {
            return "executed-" + order;
        }

        @Override
        public boolean skip(final ServerWebExchange exchange) {
            return false;
        }
    }
}
