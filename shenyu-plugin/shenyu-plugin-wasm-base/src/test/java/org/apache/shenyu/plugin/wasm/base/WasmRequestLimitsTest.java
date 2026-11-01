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

package org.apache.shenyu.plugin.wasm.base;

import com.dylibso.chicory.runtime.HostFunction;
import com.dylibso.chicory.wasm.types.FunctionType;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.wasm.api.exception.ShenyuWasmException;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.time.Duration;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.awaitility.Awaitility.await;

/**
 * Deferred request execution propagates cancellation and cleans invocation arguments.
 */
class WasmRequestLimitsTest {

    private static final String PREFIX = "shenyu.wasm." + Plugin.class.getName() + ".";

    private final CountDownLatch entered = new CountDownLatch(1);

    @BeforeEach
    void configure() {
        System.setProperty(PREFIX + "execution-timeout-millis", "1000");
    }

    @AfterEach
    void clear() {
        System.clearProperty(PREFIX + "execution-timeout-millis");
    }

    @Test
    void timeoutCleansArgumentsAndPublisherConstructionIsLazy() {
        try (Plugin plugin = new Plugin(entered)) {
            Mono<Void> result = plugin.invoke();
            assertTrue(Plugin.argumentsEmpty());
            StepVerifier.create(result).expectErrorMatches(error -> error instanceof ShenyuWasmException
                    && error.getMessage().contains("timed out")).verify(Duration.ofSeconds(5));
            assertTrue(Plugin.argumentsEmpty());
        }
    }

    @Test
    void cancellationCleansArguments() {
        try (Plugin plugin = new Plugin(entered)) {
            StepVerifier.create(plugin.invoke()).then(() -> {
                try {
                    assertTrue(entered.await(5, TimeUnit.SECONDS));
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    throw new IllegalStateException(e);
                }
            }).thenCancel().verify(Duration.ofSeconds(5));
            await().atMost(Duration.ofSeconds(5)).until(Plugin::argumentsEmpty);
        }
    }

    private static final class Plugin extends AbstractShenyuWasmPlugin {

        private static final AtomicLong IDS = new AtomicLong();

        private Plugin(final CountDownLatch entered) {
            super(store -> store.addFunction(new HostFunction("test", "entered", FunctionType.of(List.of(), List.of()),
                    (instance, args) -> {
                        entered.countDown();
                        return new long[0];
                    })));
        }

        private Mono<Void> invoke() {
            ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/wasm"));
            return super.doExecute(exchange, ignored -> Mono.empty(), new SelectorData(), new RuleData());
        }

        private static boolean argumentsEmpty() {
            return ARGUMENTS.isEmpty();
        }

        @Override
        protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain,
                                       final SelectorData selector, final RuleData rule, final Long argumentId) {
            return chain.execute(exchange);
        }

        @Override
        protected Long getArgumentId(final ServerWebExchange exchange, final ShenyuPluginChain chain,
                                     final SelectorData selector, final RuleData rule) {
            return IDS.incrementAndGet();
        }

        @Override
        public int getOrder() {
            return 1;
        }

        @Override
        public String named() {
            return "wasm-request-limits";
        }
    }
}
