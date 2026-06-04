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
import com.dylibso.chicory.runtime.Instance;
import com.dylibso.chicory.runtime.Store;
import com.dylibso.chicory.wasm.types.FunctionType;
import com.dylibso.chicory.wasm.types.ValType;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.base.cache.MatchDataCache;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * The Abstract shenyu plugin test.
 */
public final class AbstractShenyuWasmPluginTest {
    
    private RuleData ruleData;
    
    private PluginData pluginData;
    
    private SelectorData selectorData;
    
    private ConditionData conditionData;
    
    private ServerWebExchange exchange;

    private ShenyuPluginChain shenyuPluginChain;
    
    @BeforeEach
    public void setUp() {
        mockShenyuConfig();
        this.ruleData = RuleData.builder()
                .id("1").pluginName("SHENYU").selectorId("1")
                .enabled(true).loged(true).matchRestful(false).sort(1).build();
        this.conditionData = new ConditionData();
        this.conditionData.setOperator("match");
        this.conditionData.setParamName("/");
        this.conditionData.setParamType("uri");
        this.conditionData.setParamValue("/http/**");
        this.shenyuPluginChain = mock(ShenyuPluginChain.class);
        this.pluginData = PluginData.builder().name("SHENYU").enabled(true).build();
        this.selectorData = SelectorData.builder()
                .id("1").pluginName("SHENYU").enabled(true)
                .matchRestful(false).type(SelectorTypeEnum.CUSTOM_FLOW.getCode()).build();
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/http/SHENYU/SHENYU").build());
        ShenyuContext context = mock(ShenyuContext.class);
        exchange.getAttributes().put(Constants.CONTEXT, context);
        clearCache();
        when(shenyuPluginChain.execute(exchange)).thenReturn(Mono.empty());
    }

    /** Go WASM test with selector/rule matching. */
    @Test
    public void executeGoWasmPluginTest() {
        setupCacheData();
        final TestGoShenyuWasmPlugin goPlugin = new TestGoShenyuWasmPlugin("go result");
        StepVerifier.create(goPlugin.execute(exchange, shenyuPluginChain)).expectSubscription().verifyComplete();
        verify(shenyuPluginChain).execute(exchange);
    }

    /** Rust WASM test with selector/rule matching. */
    @Test
    public void executeRustWasmPluginTest() {
        setupCacheData();
        final TestShenyuWasmPlugin rustPlugin = new TestShenyuWasmPlugin("rust result");
        StepVerifier.create(rustPlugin.execute(exchange, shenyuPluginChain)).expectSubscription().verifyComplete();
        verify(shenyuPluginChain).execute(exchange);
    }

    /** Plugin not cached → should skip to next chain. */
    @Test
    public void executePluginNotCachedTest() {
        final TestGoShenyuWasmPlugin plugin = new TestGoShenyuWasmPlugin("unused");
        StepVerifier.create(plugin.execute(exchange, shenyuPluginChain)).expectSubscription().verifyComplete();
        verify(shenyuPluginChain).execute(exchange);
    }

    /** Plugin cached but no selector data → handleSelectorIfNull → skip. */
    @Test
    public void executeSelectorNotCachedTest() {
        BaseDataCache.getInstance().cachePluginData(pluginData);
        final TestGoShenyuWasmPlugin plugin = new TestGoShenyuWasmPlugin("unused");
        StepVerifier.create(plugin.execute(exchange, shenyuPluginChain)).expectSubscription().verifyComplete();
        verify(shenyuPluginChain).execute(exchange);
    }

    /** Plugin + selector cached but no rules → handleRuleIfNull → skip. */
    @Test
    public void executeRuleNotCachedTest() {
        List<ConditionData> condList = Collections.singletonList(conditionData);
        selectorData.setMatchMode(0);
        selectorData.setLogged(true);
        selectorData.setConditionList(condList);
        BaseDataCache.getInstance().cachePluginData(pluginData);
        BaseDataCache.getInstance().cacheSelectData(selectorData);
        final TestGoShenyuWasmPlugin plugin = new TestGoShenyuWasmPlugin("unused");
        StepVerifier.create(plugin.execute(exchange, shenyuPluginChain)).expectSubscription().verifyComplete();
        verify(shenyuPluginChain).execute(exchange);
    }

    /** FULL_FLOW selector with multiple rules → last rule is used. */
    @Test
    public void executeFullFlowMultiRuleTest() {
        List<ConditionData> condList = Collections.singletonList(conditionData);
        selectorData.setType(SelectorTypeEnum.FULL_FLOW.getCode());
        selectorData.setMatchMode(0);
        selectorData.setLogged(true);
        selectorData.setConditionList(condList);
        BaseDataCache.getInstance().cachePluginData(pluginData);
        BaseDataCache.getInstance().cacheSelectData(selectorData);

        RuleData rule1 = RuleData.builder()
                .id("r1").pluginName("SHENYU").selectorId("1")
                .enabled(true).loged(true).matchRestful(false).sort(1)
                .conditionDataList(condList).matchMode(0).build();
        RuleData rule2 = RuleData.builder()
                .id("r2").pluginName("SHENYU").selectorId("1")
                .enabled(true).loged(true).matchRestful(false).sort(2)
                .conditionDataList(condList).matchMode(0).build();
        BaseDataCache.getInstance().cacheRuleData(rule1);
        BaseDataCache.getInstance().cacheRuleData(rule2);

        final TestGoShenyuWasmPlugin plugin = new TestGoShenyuWasmPlugin("go result");
        StepVerifier.create(plugin.execute(exchange, shenyuPluginChain)).expectSubscription().verifyComplete();
        verify(shenyuPluginChain).execute(exchange);
    }

    @AfterEach
    public void clear() {
        MatchDataCache.getInstance().cleanSelectorData();
        MatchDataCache.getInstance().cleanRuleDataData();
    }

    private void setupCacheData() {
        List<ConditionData> condList = Collections.singletonList(conditionData);
        this.ruleData.setConditionDataList(condList);
        this.ruleData.setMatchMode(0);
        this.selectorData.setMatchMode(0);
        this.selectorData.setLogged(true);
        this.selectorData.setConditionList(condList);
        BaseDataCache.getInstance().cachePluginData(pluginData);
        BaseDataCache.getInstance().cacheSelectData(selectorData);
        BaseDataCache.getInstance().cacheRuleData(ruleData);
    }

    private void clearCache() {
        BaseDataCache.getInstance().cleanPluginData();
        BaseDataCache.getInstance().cleanSelectorData();
        BaseDataCache.getInstance().cleanRuleData();
    }
    
    private void mockShenyuConfig() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        when(context.getBean(ShenyuConfig.class)).thenReturn(new ShenyuConfig());
        SpringBeanUtils.getInstance().setApplicationContext(context);
    }

    /** Base: registers get_args/put_result via Consumer. */
    abstract static class WasmTestPlugin extends AbstractShenyuWasmPlugin {

        static final Map<Long, String> RESULTS = new ConcurrentHashMap<>();

        private static final java.util.concurrent.atomic.AtomicLong ID = new java.util.concurrent.atomic.AtomicLong();

        private final String expectedResult;

        WasmTestPlugin(final String expectedResult) {
            super(store -> registerHostFunctions(store, expectedResult));
            this.expectedResult = expectedResult;
        }

        private static void registerHostFunctions(final Store store, final String expected) {
            store.addFunction(new HostFunction("shenyu", "get_args",
                    FunctionType.of(List.of(ValType.I64, ValType.I64, ValType.I32), List.of(ValType.I32)),
                    (Instance instance, long... args) -> {
                        long argId = args[0];
                        int addr = (int) args[1];
                        int len = (int) args[2];
                        String config = "hello from java " + argId;
                        assertTrue(config.startsWith("hello from java "));
                        byte[] data = config.getBytes(StandardCharsets.UTF_8);
                        int writeLen = Math.min(len, data.length);
                        instance.memory().write(addr, data, 0, writeLen);
                        return new long[]{writeLen};
                    }));
            store.addFunction(new HostFunction("shenyu", "put_result",
                    FunctionType.of(List.of(ValType.I64, ValType.I64, ValType.I32), List.of(ValType.I32)),
                    (Instance instance, long... args) -> {
                        long argId = args[0];
                        int addr = (int) args[1];
                        int len = (int) args[2];
                        byte[] bytes = instance.memory().readBytes(addr, len);
                        String result = new String(bytes, StandardCharsets.UTF_8);
                        assertEquals(expected, result);
                        RESULTS.put(argId, result);
                        return new long[]{0};
                    }));
        }

        @Override
        protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain,
                                       final SelectorData selector, final RuleData rule, final Long argumentId) {
            final String result = RESULTS.remove(argumentId);
            assertEquals(expectedResult, result);
            return chain.execute(exchange);
        }

        @Override
        protected Long getArgumentId(final ServerWebExchange exchange, final ShenyuPluginChain chain,
                                     final SelectorData selector, final RuleData rule) {
            return ID.incrementAndGet();
        }

        @Override
        public int getOrder() {
            return 0;
        }
        
        @Override
        public String named() {
            return "SHENYU";
        }
    }

    /** Loads $TestGoShenyuWasmPlugin.wasm. */
    static class TestGoShenyuWasmPlugin extends WasmTestPlugin {
        TestGoShenyuWasmPlugin(final String expected) {
            super(expected);
        }
    }

    /** Loads $TestShenyuWasmPlugin.wasm. */
    static class TestShenyuWasmPlugin extends WasmTestPlugin {
        TestShenyuWasmPlugin(final String expected) {
            super(expected);
        }
    }
}
