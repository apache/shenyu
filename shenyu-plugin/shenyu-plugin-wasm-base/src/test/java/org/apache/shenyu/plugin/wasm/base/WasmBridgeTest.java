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

import com.dylibso.chicory.runtime.Instance;
import com.dylibso.chicory.runtime.Memory;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.base.cache.CommonPluginDataSubscriber;
import org.apache.shenyu.plugin.base.cache.MatchDataCache;
import org.apache.shenyu.plugin.wasm.base.handler.AbstractWasmPluginDataHandler;
import org.apache.shenyu.plugin.wasm.api.loader.WasmLoader;
import org.springframework.test.util.ReflectionTestUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.http.HttpStatus;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.Disposable;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.BlockingQueue;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/**
 * End-to-end tests using the default bridge, real WASM imports and gateway DTOs.
 */
final class WasmBridgeTest {

    private static final String CONTINUE = "{\"version\":1,\"action\":\"CONTINUE\"}";

    private BridgePlugin plugin;

    private BridgeHandler handler;

    private CommonPluginDataSubscriber subscriber;

    private PluginData pluginData;

    private SelectorData selector;

    private RuleData rule;

    private ShenyuPluginChain chain;

    @BeforeEach
    void setUp() {
        clearCaches();
        ShenyuConfig config = new ShenyuConfig();
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        when(context.getBean(ShenyuConfig.class)).thenReturn(config);
        SpringBeanUtils.getInstance().setApplicationContext(context);
        handler = new BridgeHandler();
        subscriber = new CommonPluginDataSubscriber(List.of(handler), mock(ApplicationEventPublisher.class),
                config.getSelectorMatchCache(), config.getRuleMatchCache());
        ConditionData condition = new ConditionData();
        condition.setParamType("uri");
        condition.setParamName("/");
        condition.setOperator("match");
        condition.setParamValue("/bridge/**");
        pluginData = PluginData.builder().id("plugin-id").name("WASM_BRIDGE").enabled(true).config("{\"mode\":\"first\"}").build();
        selector = SelectorData.builder().id("selector-id").pluginName("WASM_BRIDGE").enabled(true).continued(true).logged(false).matchRestful(false)
                .type(SelectorTypeEnum.FULL_FLOW.getCode()).sort(1).matchMode(0).conditionList(List.of(condition)).handle("selector-first").build();
        rule = RuleData.builder().id("rule-id").selectorId("selector-id").pluginName("WASM_BRIDGE")
                .enabled(true).logged(false).matchRestful(false).sort(1).handle("rule-first").conditionDataList(List.of(condition)).build();
        subscriber.onSubscribe(pluginData);
        subscriber.onSelectorSubscribe(selector);
        subscriber.onRuleSubscribe(rule);
        plugin = new BridgePlugin();
        plugin.setResult(CONTINUE);
        chain = mock(ShenyuPluginChain.class);
        when(chain.execute(any())).thenReturn(Mono.empty());
    }

    @AfterEach
    void tearDown() {
        plugin.close();
        handler.close();
        clearCaches();
    }

    @Test
    void serializesRealRequestAndMatchedConfiguration() throws IOException {
        MockServerWebExchange exchange = request();
        complete(plugin.execute(exchange, chain));
        JsonNode payload = plugin.input();
        assertEquals(1, payload.path("version").intValue());
        assertEquals("doExecute", payload.path("phase").textValue());
        assertEquals("POST", payload.path("request").path("method").textValue());
        assertEquals("/bridge/items", payload.path("request").path("path").textValue());
        assertEquals("/bridge/items?tag=one&tag=two", payload.path("request").path("uri").textValue());
        assertEquals("你好", payload.path("request").path("headers").path("X-Input").get(0).textValue());
        assertEquals("two", payload.path("request").path("queryParams").path("tag").get(1).textValue());
        assertEquals(pluginData.getConfig(), payload.path("plugin").path("config").textValue());
        assertEquals("selector-first", payload.path("selector").path("handle").textValue());
        assertEquals("rule-first", payload.path("rule").path("handle").textValue());
        assertEquals("/bridge/**", payload.path("rule").path("conditionDataList").get(0).path("paramValue").textValue());
        assertEquals(-plugin.fixtureMemory().readInt(0), plugin.fixtureMemory().readInt(32));
        verify(chain).execute(exchange);
        StepVerifier.create(exchange.getRequest().getBody().map(buffer -> buffer.toString(StandardCharsets.UTF_8)))
                .expectNext("request body").verifyComplete();
    }

    @Test
    void appliesRequestAndResponseHeadersBeforeContinuing() {
        plugin.setResult("{\"version\":1,\"action\":\"CONTINUE\","
                + "\"requestHeaders\":{\"X-Input\":[\"changed\",\"second\"],\"X-Remove\":[]},"
                + "\"responseHeaders\":{\"X-Bridge\":[\"applied\"]}}");
        MockServerWebExchange exchange = request();
        complete(plugin.execute(exchange, chain));
        ArgumentCaptor<ServerWebExchange> argument = ArgumentCaptor.forClass(ServerWebExchange.class);
        verify(chain).execute(argument.capture());
        assertNotSame(exchange, argument.getValue());
        assertEquals(List.of("changed", "second"), argument.getValue().getRequest().getHeaders().get("X-Input"));
        assertNull(argument.getValue().getRequest().getHeaders().get("X-Remove"));
        assertEquals("你好", exchange.getRequest().getHeaders().getFirst("X-Input"));
        assertEquals("applied", exchange.getResponse().getHeaders().getFirst("X-Bridge"));
    }

    @Test
    void rejectsWithStatusHeadersAndUtf8Body() {
        plugin.setResult("{\"version\":1,\"action\":\"REJECT\",\"status\":403,\"body\":\"禁止访问\","
                + "\"responseHeaders\":{\"Content-Type\":[\"text/plain;charset=UTF-8\"],\"X-Reason\":[\"policy\"]}}");
        MockServerWebExchange exchange = request();
        complete(plugin.execute(exchange, chain));
        assertEquals(HttpStatus.FORBIDDEN, exchange.getResponse().getStatusCode());
        assertEquals("policy", exchange.getResponse().getHeaders().getFirst("X-Reason"));
        StepVerifier.create(exchange.getResponse().getBodyAsString()).expectNext("禁止访问").verifyComplete();
        verifyNoInteractions(chain);
    }

    @Test
    void missingResultContinues() {
        plugin.writeControl(4, -1);
        MockServerWebExchange exchange = request();
        complete(plugin.execute(exchange, chain));
        verify(chain).execute(exchange);
    }

    @Test
    void independentHandlerUpdatesAreVisibleAndDeletedRulesStopGuestExecution() throws IOException {
        assertNotSame(handler.fixtureMemory(), plugin.fixtureMemory());
        complete(plugin.execute(request(), chain));
        final long firstId = plugin.fixtureMemory().readLong(8);
        pluginData.setConfig("{\"mode\":\"updated\"}");
        selector.setHandle("selector-updated");
        rule.setHandle("rule-updated");
        subscriber.onSubscribe(pluginData);
        subscriber.onSelectorSubscribe(selector);
        subscriber.onRuleSubscribe(rule);
        complete(plugin.execute(request(), chain));
        JsonNode payload = plugin.input();
        assertEquals(pluginData.getConfig(), payload.path("plugin").path("config").textValue());
        assertEquals("selector-updated", payload.path("selector").path("handle").textValue());
        assertEquals("rule-updated", payload.path("rule").path("handle").textValue());
        final long secondId = plugin.fixtureMemory().readLong(8);
        assertNotEquals(firstId, secondId);
        assertEquals(6, fixtureMemories(handler).stream().mapToInt(memory -> memory.readInt(28)).sum());
        subscriber.unRuleSubscribe(rule);
        complete(plugin.execute(request(), chain));
        assertEquals(secondId, plugin.fixtureMemory().readLong(8));
        subscriber.unSelectorSubscribe(selector);
        complete(plugin.execute(request(), chain));
        subscriber.unSubscribe(pluginData);
        complete(plugin.execute(request(), chain));
        assertEquals(secondId, plugin.fixtureMemory().readLong(8));
        verify(chain, times(5)).execute(any());
    }

    @Test
    void guestDecisionsReflectUpdatedRuleConfiguration() {
        plugin.writeControl(16, 3);
        MockServerWebExchange allowed = request();
        complete(plugin.execute(allowed, chain));
        verify(chain).execute(allowed);
        rule.setHandle("rule-deny");
        subscriber.onRuleSubscribe(rule);
        MockServerWebExchange denied = request();
        complete(plugin.execute(denied, chain));
        assertEquals(HttpStatus.FORBIDDEN, denied.getResponse().getStatusCode());
        StepVerifier.create(denied.getResponse().getBodyAsString()).expectNext("configured denial").verifyComplete();
        rule.setHandle("rule-allow");
        subscriber.onRuleSubscribe(rule);
        MockServerWebExchange allowedAgain = request();
        complete(plugin.execute(allowedAgain, chain));
        verify(chain).execute(allowedAgain);
        verify(chain, times(2)).execute(any());
    }

    @Test
    void malformedResultFailsClosedAndNextInvocationRecovers() {
        plugin.setResult("not JSON");
        MockServerWebExchange failed = request();
        complete(plugin.execute(failed, chain));
        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, failed.getResponse().getStatusCode());
        verifyNoInteractions(chain);
        plugin.setResult(CONTINUE);
        MockServerWebExchange recovered = request();
        complete(plugin.execute(recovered, chain));
        verify(chain).execute(recovered);
    }

    @Test
    void guestTrapDiscardsAnAlreadySubmittedResult() {
        plugin.setResult("{\"version\":1,\"action\":\"REJECT\",\"status\":403,\"responseHeaders\":{\"X-Invalid\":[\"unused\"]}}");
        plugin.writeControl(16, 1);
        MockServerWebExchange failed = request();
        complete(plugin.execute(failed, chain));
        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, failed.getResponse().getStatusCode());
        assertNull(failed.getResponse().getHeaders().getFirst("X-Invalid"));
        verifyNoInteractions(chain);
        plugin.writeControl(16, 0);
        plugin.setResult(CONTINUE);
        complete(plugin.execute(request(), chain));
        verify(chain).execute(any());
    }

    @Test
    void invalidGuestAddressFailsClosed() {
        plugin.writeControl(16, 2);
        MockServerWebExchange exchange = request();
        complete(plugin.execute(exchange, chain));
        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, exchange.getResponse().getStatusCode());
        verifyNoInteractions(chain);
    }

    @Test
    void oversizedArgumentsFailWithoutRetainingInvocation() {
        pluginData.setConfig("x".repeat(WasmRequestBridge.MAX_PAYLOAD_BYTES));
        MockServerWebExchange failed = request();
        complete(plugin.execute(failed, chain));
        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, failed.getResponse().getStatusCode());
        verifyNoInteractions(chain);
        pluginData.setConfig("{}");
        complete(plugin.execute(request(), chain));
        verify(chain).execute(any());
    }

    @Test
    void optionalHooksCanBeOmitted() {
        try (MinimalPlugin minimal = new MinimalPlugin()) {
            minimal.setResult(CONTINUE);
            MockServerWebExchange exchange = request();
            minimal.before(exchange);
            complete(minimal.execute(exchange, chain));
            minimal.after(exchange);
            verify(chain).execute(exchange);
        }
    }

    @Test
    void hooksReceiveMetadataWithNullMatchedConfiguration() throws IOException {
        plugin.before(request());
        assertEquals("before", plugin.input().path("phase").textValue());
        assertTrue(plugin.input().path("selector").isNull());
        assertTrue(plugin.input().path("rule").isNull());
        plugin.after(request());
        assertEquals("after", plugin.input().path("phase").textValue());
        assertEquals(1, plugin.counter(20));
        assertEquals(1, plugin.counter(24));
        complete(plugin.execute(request(), chain));
        verify(chain).execute(any());
    }

    @Test
    void invocationIsLazyAndCancellationDoesNotRetainState() {
        when(chain.execute(any())).thenReturn(Mono.never());
        Mono<Void> execution = plugin.execute(request(), chain);
        assertEquals(0, plugin.fixtureMemory().readLong(8));
        Disposable subscription = execution.subscribe();
        long firstId = plugin.fixtureMemory().readLong(8);
        assertTrue(firstId > 0);
        subscription.dispose();
        when(chain.execute(any())).thenReturn(Mono.empty());
        complete(execution);
        assertNotEquals(firstId, plugin.fixtureMemory().readLong(8));
    }

    @Test
    void concurrentRequestsUseSerializedGuestCalls() {
        List<CompletableFuture<Void>> calls = new ArrayList<>();
        for (int i = 0; i < 20; i++) {
            calls.add(CompletableFuture.runAsync(() -> {
                MockServerWebExchange exchange = request();
                complete(plugin.execute(exchange, chain));
                assertNull(exchange.getResponse().getStatusCode());
            }));
        }
        CompletableFuture.allOf(calls.toArray(new CompletableFuture<?>[0])).join();
        verify(chain, times(20)).execute(any());
    }

    @Test
    void closedBridgeRejectsFurtherExecution() {
        plugin.close();
        plugin.close();
        MockServerWebExchange exchange = request();
        complete(plugin.execute(exchange, chain));
        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, exchange.getResponse().getStatusCode());
        verifyNoInteractions(chain);
    }

    private MockServerWebExchange request() {
        return MockServerWebExchange.from(MockServerHttpRequest.post("/bridge/items?tag=one&tag=two")
                .header("X-Input", "你好", "second").header("X-Remove", "remove-me").body("request body"));
    }

    private void complete(final Mono<Void> execution) {
        StepVerifier.create(execution).verifyComplete();
    }

    private void clearCaches() {
        BaseDataCache.getInstance().cleanPluginData();
        BaseDataCache.getInstance().cleanSelectorData();
        BaseDataCache.getInstance().cleanRuleData();
        MatchDataCache.getInstance().cleanSelectorData();
        MatchDataCache.getInstance().cleanRuleDataData();
    }

    // Fixtures are configured and inspected only while all guest calls are idle. Seed every
    // pooled instance and inspect the last returned one; getMemory() remains restricted to calls.
    @SuppressWarnings("unchecked")
    private static List<Memory> fixtureMemories(final WasmLoader loader) {
        BlockingQueue<Instance> instances = (BlockingQueue<Instance>) ReflectionTestUtils.getField(loader, "freeInstances");
        assertEquals(ReflectionTestUtils.getField(loader, "poolSize"), instances.size());
        return instances.stream().map(Instance::memory).collect(java.util.stream.Collectors.toList());
    }

    static class BridgePlugin extends AbstractShenyuWasmPlugin {

        void setResult(final String json) {
            byte[] bytes = json.getBytes(StandardCharsets.UTF_8);
            for (Memory memory : memories()) {
                memory.write(1200000, bytes);
                memory.writeI32(4, bytes.length);
            }
        }

        JsonNode input() throws IOException {
            return new ObjectMapper().readTree(fixtureMemory().readBytes(4096, fixtureMemory().readInt(0)));
        }

        private List<Memory> memories() {
            return fixtureMemories((WasmLoader) ReflectionTestUtils.getField(this, "wasmLoader"));
        }

        Memory fixtureMemory() {
            List<Memory> memories = memories();
            return memories.get(memories.size() - 1);
        }

        void writeControl(final int address, final int value) {
            memories().forEach(memory -> memory.writeI32(address, value));
        }

        int counter(final int address) {
            return memories().stream().mapToInt(memory -> memory.readInt(address)).sum();
        }

        @Override
        public int getOrder() {
            return 0;
        }

        @Override
        public String named() {
            return "WASM_BRIDGE";
        }
    }

    static final class MinimalPlugin extends BridgePlugin {
    }

    static final class BridgeHandler extends AbstractWasmPluginDataHandler {

        Memory fixtureMemory() {
            List<Memory> memories = fixtureMemories(this);
            return memories.get(memories.size() - 1);
        }

        @Override
        public String pluginNamed() {
            return "WASM_BRIDGE";
        }
    }
}
