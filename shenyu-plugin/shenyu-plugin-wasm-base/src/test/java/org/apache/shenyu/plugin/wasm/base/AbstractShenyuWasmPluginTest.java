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

import io.github.kawamuray.wasmtime.Func;
import io.github.kawamuray.wasmtime.Store;
import io.github.kawamuray.wasmtime.WasmFunctions;
import io.github.kawamuray.wasmtime.WasmValType;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
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

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;

public class AbstractShenyuWasmPluginTest {
    private ServerWebExchange exchange;

    private RustShenyuWasmPlugin rustShenyuWasmPlugin;

    private ShenyuPluginChain shenyuPluginChain;

    private SelectorData selector;

    private RuleData rule;

    @BeforeEach
    public void setUp() {
        mockShenyuConfig();
        this.shenyuPluginChain = mock(ShenyuPluginChain.class);
        this.rustShenyuWasmPlugin = spy(new RustShenyuWasmPlugin());
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/http/SHENYU/SHENYU")
                .build());
        this.selector = SelectorData.builder()
                .id("selector123")
                .pluginId("plugin123")
                .pluginName("RustShenyuWasmPlugin")
                .name("testSelector")
                .matchMode(1)
                .type(1)
                .sort(1)
                .enabled(true)
                .logged(true)
                .continued(true)
                .handle("selectorHandle")
                .matchRestful(true)
                .build();
        this.rule = RuleData.builder()
                .id("rule123")
                .name("testRule")
                .pluginName("RustShenyuWasmPlugin")
                .selectorId("ruleSelector123")
                .matchMode(1)
                .sort(1)
                .enabled(true)
                .loged(true)
                .handle("ruleHandle")
                .matchRestful(true)
                .build();
        when(shenyuPluginChain.doExecute(exchange, selector, rule)).thenReturn(Mono.empty());
    }

    /**
     * The plugin is null test.
     */
    @Test
    public void doExecutePluginTest() {
        StepVerifier.create(rustShenyuWasmPlugin.doExecute(exchange, shenyuPluginChain, selector, rule)).expectSubscription().verifyComplete();
        verify(shenyuPluginChain).doExecute(exchange, selector, rule);
    }

    private void mockShenyuConfig() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        when(context.getBean(ShenyuConfig.class)).thenReturn(new ShenyuConfig());
        SpringBeanUtils.getInstance().setApplicationContext(context);
    }

    static class RustShenyuWasmPlugin extends AbstractShenyuWasmPlugin {

        private static final Map<Long, String> RESULTS = new ConcurrentHashMap<>();

        @Override
        public int getOrder() {
            return 0;
        }

        @Override
        protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule, final Long argumentId) {
            final String result = RESULTS.get(argumentId);
            assertEquals("rust result", result);
            return chain.execute(exchange);
        }

        @Override
        protected Long getArgumentId(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
            return 0L;
        }

        @Override
        protected Map<String, Func> initWasmCallJavaFunc(final Store<Void> store) {
            Map<String, Func> funcMap = new HashMap<>();
            funcMap.put("get_args", WasmFunctions.wrap(store, WasmValType.I64, WasmValType.I64, WasmValType.I32, WasmValType.I32,
                (argId, addr, len) -> {
                    String config = "hello from java " + argId;
                    LOG.info("java side->" + config);
                    assertEquals("hello from java 0", config);
                    ByteBuffer buf = getWasmLoader().getBuffer();
                    for (int i = 0; i < len && i < config.length(); i++) {
                        buf.put(addr.intValue() + i, (byte) config.charAt(i));
                    }
                    return Math.min(config.length(), len);
                }));
            funcMap.put("put_result", WasmFunctions.wrap(store, WasmValType.I64, WasmValType.I64, WasmValType.I32, WasmValType.I32,
                (argId, addr, len) -> {
                    ByteBuffer buf = getWasmLoader().getBuffer();
                    byte[] bytes = new byte[len];
                    for (int i = 0; i < len; i++) {
                        bytes[i] = buf.get(addr.intValue() + i);
                    }
                    String result = new String(bytes, StandardCharsets.UTF_8);
                    assertEquals("rust result", result);
                    RESULTS.put(argId, result);
                    LOG.info("java side->" + result);
                    return 0;
                }));
            return funcMap;
        }
    }
}
