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
import org.apache.shenyu.web.loader.ShenyuLoaderService;
import org.junit.jupiter.api.Test;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.List;
import java.util.function.Function;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

/** Tests default plugin timing through the production chain, including failure and cancellation. */
public final class ShenyuPluginTimingLifecycleTest {

    private static final String NAME = "timing-review";

    private static final String KEY = Constants.PLUGIN_START_TIME + NAME;

    @Test
    public void completionCleansDefaultTiming() {
        ServerWebExchange exchange = exchange();
        StepVerifier.create(handler(plugin(request -> Mono.empty())).handle(exchange)).verifyComplete();
        assertCleaned(exchange);
    }

    @Test
    public void synchronousFailurePreservesErrorAndCleansDefaultTiming() {
        ServerWebExchange exchange = exchange();
        IllegalStateException failure = new IllegalStateException("synchronous");
        ShenyuPlugin plugin = plugin(request -> {
            throw failure;
        });
        StepVerifier.create(handler(plugin).handle(exchange)).expectErrorMatches(error -> error == failure).verify();
        assertCleaned(exchange);
    }

    @Test
    public void asynchronousFailurePreservesErrorAndCleansDefaultTiming() {
        ServerWebExchange exchange = exchange();
        IllegalStateException failure = new IllegalStateException("asynchronous");
        StepVerifier.create(handler(plugin(request -> Mono.error(failure))).handle(exchange))
                .expectErrorMatches(error -> error == failure).verify();
        assertCleaned(exchange);
    }

    @Test
    public void pendingBodyKeepsAssemblyTimingScopeAndCancellationCleansChainTiming() {
        ServerWebExchange exchange = exchange();
        StepVerifier.create(handler(plugin(request -> Mono.never())).handle(exchange))
                .then(() -> {
                    assertFalse(exchange.getAttributes().containsKey(KEY));
                    assertTrue(exchange.getAttributes().containsKey(Constants.CHAIN_START_TIME));
                }).thenCancel().verify();
        assertCleaned(exchange);
    }

    @Test
    public void skippedPluginDoesNotEnterDefaultTiming() {
        ServerWebExchange exchange = exchange();
        ShenyuPlugin skipped = new ShenyuPlugin() {
            @Override
            public Mono<Void> execute(final ServerWebExchange request, final ShenyuPluginChain chain) {
                throw new AssertionError("Skipped plugin must not execute");
            }

            @Override
            public int getOrder() {
                return 0;
            }

            @Override
            public String named() {
                return NAME;
            }

            @Override
            public boolean skip(final ServerWebExchange request) {
                return true;
            }
        };
        StepVerifier.create(handler(skipped).handle(exchange)).verifyComplete();
        assertCleaned(exchange);
    }

    private ShenyuPlugin plugin(final Function<ServerWebExchange, Mono<Void>> action) {
        return new ShenyuPlugin() {
            @Override
            public Mono<Void> execute(final ServerWebExchange request, final ShenyuPluginChain chain) {
                assertNotNull(request.getAttribute(KEY));
                return action.apply(request);
            }

            @Override
            public int getOrder() {
                return 0;
            }

            @Override
            public String named() {
                return NAME;
            }
        };
    }

    private ShenyuWebHandler handler(final ShenyuPlugin plugin) {
        ShenyuConfig config = new ShenyuConfig();
        config.getScheduler().setEnabled(false);
        return new ShenyuWebHandler(List.of(plugin), mock(ShenyuLoaderService.class), config);
    }

    private ServerWebExchange exchange() {
        return MockServerWebExchange.from(MockServerHttpRequest.get("/timing-review").build());
    }

    private void assertCleaned(final ServerWebExchange exchange) {
        assertFalse(exchange.getAttributes().containsKey(KEY));
        assertFalse(exchange.getAttributes().containsKey(Constants.CHAIN_START_TIME));
    }
}
