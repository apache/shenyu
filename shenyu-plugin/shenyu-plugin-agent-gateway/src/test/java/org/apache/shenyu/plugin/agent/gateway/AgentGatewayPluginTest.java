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

package org.apache.shenyu.plugin.agent.gateway;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.agent.gateway.handler.AgentGatewayPluginDataHandler;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpDispatcher;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpExecutionContext;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpHttpHandler;
import org.apache.shenyu.plugin.agent.gateway.security.AgentMcpIdentity;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolProvider;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolRegistry;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.context.ApplicationContext;
import org.springframework.http.HttpStatus;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Scheduler;
import reactor.core.scheduler.Schedulers;
import reactor.test.StepVerifier;

import java.time.Duration;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.ArgumentMatchers.any;

class AgentGatewayPluginTest {

    private final AgentGatewayPlugin plugin = new AgentGatewayPlugin();

    @BeforeEach
    void initResult() {
        ApplicationContext applicationContext = mock(ApplicationContext.class);
        when(applicationContext.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult());
        SpringBeanUtils.getInstance().setApplicationContext(applicationContext);
    }

    @AfterEach
    void clearCache() {
        AgentGatewayPluginDataHandler.CACHED_HANDLE.get().getAllCache().clear();
    }

    @Test
    void shouldExposePluginIdentity() {
        assertEquals(PluginEnum.AGENT_GATEWAY.getName(), plugin.named());
        assertEquals(PluginEnum.AGENT_GATEWAY.getCode(), plugin.getOrder());
    }

    @Test
    void shouldPropagateRequestContextThroughReactorContext() {
        ServerWebExchange exchange = newExchange();
        AtomicReference<AgentTrafficContext> reactorContext = new AtomicReference<>();
        AtomicReference<AgentTrafficContext> exchangeContext = new AtomicReference<>();
        ShenyuPluginChain chain = next -> Mono.deferContextual(context -> {
            reactorContext.set(context.get(AgentGatewayConstants.REACTOR_CONTEXT_KEY));
            exchangeContext.set(next.getAttribute(AgentGatewayConstants.REQUEST_CONTEXT_ATTRIBUTE));
            return Mono.empty();
        });

        StepVerifier.create(plugin.doExecute(exchange, chain, selector(), rule("rule-1")))
                .verifyComplete();

        assertNotNull(exchangeContext.get());
        assertEquals(exchangeContext.get(), reactorContext.get());
        assertEquals("LLM", exchangeContext.get().getTrafficType());
        assertEquals("selector-1", exchangeContext.get().getSelectorId());
        assertEquals("rule-1", exchangeContext.get().getRuleId());
        assertTrue(Objects.isNull(exchange.getAttribute(AgentGatewayConstants.REQUEST_CONTEXT_ATTRIBUTE)));
    }

    @Test
    void shouldPreserveExistingContextAcrossAsyncBoundary() {
        ServerWebExchange exchange = newExchange();
        AtomicReference<String> existingValue = new AtomicReference<>();
        AtomicReference<AgentTrafficContext> contextValue = new AtomicReference<>();
        Scheduler scheduler = Schedulers.newSingle("agent-gateway-context-test");
        ShenyuPluginChain chain = next -> Mono.delay(Duration.ofMillis(10), scheduler)
                .then(Mono.deferContextual(context -> {
                    existingValue.set(context.get("existing-key"));
                    contextValue.set(context.get(AgentGatewayConstants.REACTOR_CONTEXT_KEY));
                    return Mono.empty();
                }));

        try {
            StepVerifier.create(plugin.doExecute(exchange, chain, selector(), rule("rule-1"))
                            .contextWrite(context -> context.put("existing-key", "existing-value")))
                    .verifyComplete();
        } finally {
            scheduler.dispose();
        }

        assertEquals("existing-value", existingValue.get());
        assertNotNull(contextValue.get());
        assertTrue(Objects.isNull(exchange.getAttribute(AgentGatewayConstants.REQUEST_CONTEXT_ATTRIBUTE)));
    }

    @Test
    void shouldGenerateIndependentContextForConcurrentRequests() {
        Set<String> requestIds = new HashSet<>();
        for (int i = 0; i < 12; i++) {
            ServerWebExchange exchange = newExchange();
            AtomicReference<String> requestId = new AtomicReference<>();
            ShenyuPluginChain chain = next -> {
                captureRequestId(next, requestId);
                return Mono.empty();
            };
            StepVerifier.create(plugin.doExecute(exchange, chain, selector(), rule("rule-" + i)))
                    .verifyComplete();
            assertNotNull(requestId.get());
            assertTrue(requestIds.add(requestId.get()));
        }
        assertEquals(12, requestIds.size());
    }

    @Test
    void shouldNotTrustClientRequestId() {
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/agent")
                .header(AgentGatewayConstants.REQUEST_ID_HEADER, "client-request-id")
                .build());

        AtomicReference<String> requestId = new AtomicReference<>();
        ShenyuPluginChain chain = next -> {
            captureRequestId(next, requestId);
            return Mono.empty();
        };
        StepVerifier.create(plugin.doExecute(exchange, chain, selector(), rule("rule-1")))
                .verifyComplete();

        assertNotNull(requestId.get());
        assertNotEquals("client-request-id", requestId.get());
    }

    @Test
    void shouldAddResponseRequestIdOnlyWhenConfigured() {
        ServerWebExchange exchange = newExchange();
        AtomicReference<String> requestId = new AtomicReference<>();
        ShenyuPluginChain chain = next -> {
            captureRequestId(next, requestId);
            return exchange.getResponse().setComplete();
        };
        StepVerifier.create(plugin.doExecute(exchange, chain, selector(),
                        rule("{\"trafficType\":\"LLM\",\"responseRequestId\":true}")))
                .verifyComplete();

        assertEquals(requestId.get(), exchange.getResponse().getHeaders()
                .getFirst(AgentGatewayConstants.REQUEST_ID_HEADER));
    }

    @Test
    void shouldNotAddResponseRequestIdWhenDisabled() {
        ServerWebExchange exchange = newExchange();
        ShenyuPluginChain chain = next -> exchange.getResponse().setComplete();

        StepVerifier.create(plugin.doExecute(exchange, chain, selector(), rule("rule-1")))
                .verifyComplete();

        assertTrue(Objects.isNull(exchange.getResponse().getHeaders().getFirst(AgentGatewayConstants.REQUEST_ID_HEADER)));
    }

    @Test
    void shouldUseCurrentRuleWhenCachedSnapshotIsStale() {
        ServerWebExchange exchange = newExchange();
        RuleData oldRule = RuleData.builder().id("rule-1").selectorId("selector-1")
                .handle("{\"trafficType\":\"LLM\"}").build();
        AgentGatewayPluginDataHandler.CACHED_HANDLE.get().cachedHandle("selector-1_rule-1",
                new org.apache.shenyu.plugin.agent.gateway.handle.AgentGatewayRuleHandleParser().parse(
                        oldRule.getHandle()));
        RuleData currentRule = RuleData.builder().id("rule-1").selectorId("selector-1")
                .handle("{\"trafficType\":\"LLM\",\"responseRequestId\":true}").build();

        ShenyuPluginChain chain = next -> exchange.getResponse().setComplete();
        StepVerifier.create(plugin.doExecute(exchange, chain, selector(), currentRule))
                .verifyComplete();

        assertNotNull(exchange.getResponse().getHeaders().getFirst(AgentGatewayConstants.REQUEST_ID_HEADER));
    }

    @Test
    void shouldRejectInvalidHandleBeforeCallingDownstream() {
        ServerWebExchange exchange = newExchange();
        AtomicReference<Integer> calls = new AtomicReference<>(0);
        RuleData invalidRule = RuleData.builder()
                .id("rule-1")
                .selectorId("selector-1")
                .handle("{\"trafficType\":\"MCP\"}")
                .build();

        StepVerifier.create(plugin.doExecute(exchange, next -> {
            calls.set(calls.get() + 1);
            return Mono.empty();
        }, selector(), invalidRule)).verifyComplete();

        assertEquals(0, calls.get());
        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, exchange.getResponse().getStatusCode());
        assertNotNull(AgentGatewayPluginDataHandler.CACHED_HANDLE.get().obtainHandle("selector-1_rule-1"));
        assertTrue(!AgentGatewayPluginDataHandler.CACHED_HANDLE.get()
                .obtainHandle("selector-1_rule-1").isValid());
    }

    @Test
    void shouldIgnoreUnknownRuleHandleFieldOnRequestPath() {
        ServerWebExchange exchange = newExchange();
        AtomicBoolean called = new AtomicBoolean();

        StepVerifier.create(plugin.doExecute(exchange, next -> {
            called.set(true);
            return Mono.empty();
        }, selector(), rule("{\"trafficType\":\"LLM\",\"futureOption\":true}")))
                .verifyComplete();

        assertTrue(called.get());
    }

    @Test
    void shouldCreateFreshContextForEachSubscription() {
        ServerWebExchange exchange = newExchange();
        AtomicReference<Integer> calls = new AtomicReference<>(0);
        Set<String> requestIds = new HashSet<>();
        Mono<Void> execution = plugin.doExecute(exchange, next -> {
            calls.set(calls.get() + 1);
            requestIds.add(next.<AgentTrafficContext>getAttribute(
                    AgentGatewayConstants.REQUEST_CONTEXT_ATTRIBUTE).getRequestId());
            return Mono.empty();
        }, selector(), rule("rule-1"));

        StepVerifier.create(execution).verifyComplete();
        StepVerifier.create(execution).verifyComplete();

        assertEquals(2, calls.get());
        assertEquals(2, requestIds.size());
        assertTrue(Objects.isNull(exchange.getAttribute(AgentGatewayConstants.REQUEST_CONTEXT_ATTRIBUTE)));
    }

    @Test
    void shouldPropagateCancellationAndCleanOnlyCurrentExchange() {
        ServerWebExchange exchange = newExchange();
        AtomicBoolean cancelled = new AtomicBoolean();
        ShenyuPluginChain chain = next -> Mono.<Void>never().doOnCancel(() -> cancelled.set(true));

        StepVerifier.create(plugin.doExecute(exchange, chain, selector(), rule("rule-1")))
                .thenCancel()
                .verify();

        assertTrue(cancelled.get());
        assertTrue(Objects.isNull(exchange.getAttribute(AgentGatewayConstants.REQUEST_CONTEXT_ATTRIBUTE)));
    }

    @Test
    void shouldPassThroughContinuedFalseSelector() {
        ServerWebExchange exchange = newExchange();
        SelectorData selector = SelectorData.builder().id("selector-1").continued(false).build();
        AtomicBoolean called = new AtomicBoolean();

        StepVerifier.create(plugin.doExecute(exchange, next -> {
            called.set(true);
            assertTrue(Objects.isNull(next.getAttribute(AgentGatewayConstants.REQUEST_CONTEXT_ATTRIBUTE)));
            return Mono.empty();
        }, selector, rule("rule-1")))
                .verifyComplete();

        assertTrue(called.get());
        assertTrue(Objects.isNull(exchange.getResponse().getStatusCode()));
    }

    @Test
    void shouldCleanContextWhenChainThrowsSynchronously() {
        ServerWebExchange exchange = newExchange();
        StepVerifier.create(plugin.doExecute(exchange, next -> {
            throw new IllegalStateException("chain failed before returning a publisher");
        }, selector(), rule("rule-1"))).expectError(IllegalStateException.class).verify();
        assertTrue(Objects.isNull(exchange.getAttribute(AgentGatewayConstants.REQUEST_CONTEXT_ATTRIBUTE)));
    }

    @Test
    void shouldDenyDefaultMcpAndNotContinueToBusinessChain() {
        MockServerWebExchange exchange = mcpExchange();
        StepVerifier.create(plugin.doExecute(exchange, next -> Mono.error(new AssertionError("MCP must terminate here")), selector(),
                rule("{\"trafficType\":\"mcp\",\"mcp\":{}}"))).verifyComplete();
        assertEquals(HttpStatus.UNAUTHORIZED, exchange.getResponse().getStatusCode());
        assertTrue(Objects.isNull(exchange.getAttribute(AgentGatewayConstants.REQUEST_CONTEXT_ATTRIBUTE)));
    }

    @Test
    void shouldSelectConfigurationAtSubscriptionNotAssembly() {
        MockServerWebExchange exchange = mcpExchange();
        RuleData data = rule("rule-1");
        Mono<Void> execution = plugin.doExecute(exchange, next -> Mono.error(new AssertionError("Stale LLM configuration")), selector(), data);
        data.setHandle("{\"trafficType\":\"mcp\",\"mcp\":{}}");
        StepVerifier.create(execution).verifyComplete();
        assertEquals(HttpStatus.UNAUTHORIZED, exchange.getResponse().getStatusCode());
    }

    @Test
    void shouldKeepInflightPermissionSnapshotWhileNewRequestsSeeRevocation() {
        AtomicReference<AgentMcpExecutionContext> captured = new AtomicReference<>();
        reactor.core.publisher.Sinks.One<JsonObject> result = reactor.core.publisher.Sinks.one();
        AgentToolProvider tool = mock(AgentToolProvider.class);
        when(tool.getName()).thenReturn("read");
        when(tool.getDescription()).thenReturn("Read one resource");
        when(tool.getInputSchema()).thenReturn(JsonParser.parseString("{\"type\":\"object\"}").getAsJsonObject());
        when(tool.getRequiredClientCapabilities()).thenReturn(new JsonObject());
        when(tool.invoke(any())).thenReturn(Mono.deferContextual(context -> {
            captured.set(context.get(AgentMcpExecutionContext.class));
            assertEquals(context.<AgentTrafficContext>get(AgentGatewayConstants.REACTOR_CONTEXT_KEY).getRequestId(), captured.get().getRequestId());
            return result.asMono();
        }));
        AgentGatewayPlugin configured = new AgentGatewayPlugin(new AgentMcpHttpHandler(
                new AgentMcpDispatcher(new AgentToolRegistry(List.of(tool)), "server", "1"),
                next -> Mono.just(new AgentMcpIdentity("trusted", Set.of("read")))));
        RuleData data = rule("{\"trafficType\":\"mcp\",\"responseRequestId\":true,\"mcp\":{\"allowedTools\":[\"read\"]}}");
        MockServerWebExchange first = mcpExchange();
        MockServerWebExchange second = mcpExchange();
        StepVerifier.create(configured.doExecute(first, next -> Mono.error(new AssertionError("Unexpected chain")), selector(), data))
                .then(() -> {
                    assertEquals(Set.of("read"), captured.get().getAllowedTools());
                    data.setHandle("{\"trafficType\":\"mcp\",\"mcp\":{}}");
                    new AgentGatewayPluginDataHandler().handlerRule(data);
                    StepVerifier.create(configured.doExecute(second, next -> Mono.error(new AssertionError("Unexpected chain")), selector(), data)).verifyComplete();
                    assertEquals(HttpStatus.FORBIDDEN, second.getResponse().getStatusCode());
                    assertEquals(reactor.core.publisher.Sinks.EmitResult.OK, result.tryEmitValue(new JsonObject()));
                }).verifyComplete();
        assertEquals(HttpStatus.OK, first.getResponse().getStatusCode());
        assertEquals(captured.get().getRequestId(), first.getResponse().getHeaders().getFirst(AgentGatewayConstants.REQUEST_ID_HEADER));
        assertTrue(Objects.isNull(first.getAttribute(AgentGatewayConstants.REQUEST_CONTEXT_ATTRIBUTE)));
        assertTrue(Objects.isNull(second.getAttribute(AgentGatewayConstants.REQUEST_CONTEXT_ATTRIBUTE)));
    }

    private MockServerWebExchange mcpExchange() {
        return MockServerWebExchange.from(MockServerHttpRequest.post("/agent/mcp")
                .header("Content-Type", "application/json").header("Accept", "application/json, text/event-stream")
                .header("MCP-Protocol-Version", "2026-07-28").header("Mcp-Method", "tools/call").header("Mcp-Name", "read")
                .body("{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/call\",\"params\":{\"name\":\"read\",\"_meta\":{"
                        + "\"io.modelcontextprotocol/protocolVersion\":\"2026-07-28\",\"io.modelcontextprotocol/clientCapabilities\":{}}}}"));
    }

    private ServerWebExchange newExchange() {
        return MockServerWebExchange.from(MockServerHttpRequest.get("/agent").build());
    }

    private void captureRequestId(final ServerWebExchange exchange, final AtomicReference<String> requestId) {
        requestId.set(exchange.<AgentTrafficContext>getAttribute(
                AgentGatewayConstants.REQUEST_CONTEXT_ATTRIBUTE).getRequestId());
    }

    private SelectorData selector() {
        return SelectorData.builder().id("selector-1").continued(true).build();
    }

    private RuleData rule(final String ruleHandleOrId) {
        final boolean isHandle = ruleHandleOrId.startsWith("{");
        return RuleData.builder()
                .id(isHandle ? "rule-1" : ruleHandleOrId)
                .selectorId("selector-1")
                .handle(isHandle ? ruleHandleOrId : "{\"trafficType\":\"LLM\"}")
                .build();
    }
}
