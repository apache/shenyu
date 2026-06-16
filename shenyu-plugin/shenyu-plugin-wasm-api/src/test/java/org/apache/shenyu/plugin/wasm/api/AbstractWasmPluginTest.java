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

package org.apache.shenyu.plugin.wasm.api;

import com.dylibso.chicory.runtime.Memory;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class AbstractWasmPluginTest {
    
    private ServerWebExchange exchange;

    private ShenyuPluginChain shenyuPluginChain;
    
    @BeforeEach
    public void setUp() {
        mockShenyuConfig();
        this.shenyuPluginChain = mock(ShenyuPluginChain.class);
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/http/SHENYU/SHENYU")
                .build());
        when(shenyuPluginChain.execute(exchange)).thenReturn(Mono.empty());
    }

    /**
     * Test with Go-compiled .wasm — verifies end-to-end shared memory communication.
     */
    @Test
    public void executeGoWasmPluginTest() {
        final GoWasmPlugin goPlugin = new GoWasmPlugin();
        StepVerifier.create(goPlugin.execute(exchange, shenyuPluginChain)).expectSubscription().verifyComplete();
        verify(shenyuPluginChain).execute(exchange);
    }
    
    /**
     * Test with Rust-compiled .wasm — verifies end-to-end shared memory communication.
     */
    @Test
    public void executeRustWasmPluginTest() {
        final RustWasmPlugin rustPlugin = new RustWasmPlugin();
        StepVerifier.create(rustPlugin.execute(exchange, shenyuPluginChain)).expectSubscription().verifyComplete();
        verify(shenyuPluginChain).execute(exchange);
    }
    
    private void mockShenyuConfig() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        when(context.getBean(ShenyuConfig.class)).thenReturn(new ShenyuConfig());
        SpringBeanUtils.getInstance().setApplicationContext(context);
    }
    
    /**
     * Base class for WASM plugin tests — shared get_args / put_result impl.
     */
    abstract static class WasmTestPlugin extends AbstractWasmPlugin {

        private static final java.util.concurrent.atomic.AtomicLong ID =
                new java.util.concurrent.atomic.AtomicLong();

        private final Map<Long, String> results = new ConcurrentHashMap<>();

        private final String expectedResult;

        WasmTestPlugin(final String expectedResult) {
            this.expectedResult = expectedResult;
        }

        @Override
        protected Long getArgumentId(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
            return ID.incrementAndGet();
        }
        
        @Override
        protected long onGetArgs(final long argId, final long addr, final int len) {
            String config = "hello from java " + argId;
            LOG.info("java side->{}", config);
            assertTrue(config.startsWith("hello from java "));
            Memory memory = super.getMemory();
            byte[] data = config.getBytes(StandardCharsets.UTF_8);
            int writeLen = Math.min(len, data.length);
            memory.write((int) addr, data, 0, writeLen);
            return writeLen;
        }
        
        @Override
        protected long onPutResult(final long argId, final long addr, final int len) {
            Memory memory = super.getMemory();
            byte[] bytes = memory.readBytes((int) addr, len);
            String result = new String(bytes, StandardCharsets.UTF_8);
            assertEquals(expectedResult, result);
            results.put(argId, result);
            LOG.info("java side->{}", result);
            return 0;
        }
        
        @Override
        protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final Long argumentId) {
            final String result = results.remove(argumentId);
            assertEquals(expectedResult, result);
            return chain.execute(exchange);
        }

        @Override
        public int getOrder() {
            return 0;
        }
    }

    /** Loads $GoWasmPlugin.wasm. */
    static class GoWasmPlugin extends WasmTestPlugin {
        GoWasmPlugin() {
            super("go result");
        }
    }

    /** Loads $RustWasmPlugin.wasm. */
    static class RustWasmPlugin extends WasmTestPlugin {
        RustWasmPlugin() {
            super("rust result");
        }
    }
}
