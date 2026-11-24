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

package org.apache.shenyu.plugin.ai.requestid;

import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.AgentRequestContext;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.ai.requestid.handler.AiRequestIdPluginDataHandler;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.context.ApplicationContext;
import org.springframework.http.HttpStatus;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Scheduler;
import reactor.core.scheduler.Schedulers;
import reactor.test.StepVerifier;

import java.time.Duration;
import java.util.Set;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class AiRequestIdPluginTest {

    private final AiRequestIdPlugin plugin = new AiRequestIdPlugin();

    @BeforeEach
    void initResult() {
        ApplicationContext applicationContext = mock(ApplicationContext.class);
        when(applicationContext.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult());
        SpringBeanUtils.getInstance().setApplicationContext(applicationContext);
    }

    @AfterEach
    void clearCache() {
        AiRequestIdPluginDataHandler.CACHED_HANDLE.get().getAllCache().clear();
        SpringBeanUtils.getInstance().setApplicationContext(null);
    }

    @Test
    void shouldExposePluginIdentity() {
        assertEquals(PluginEnum.AI_REQUEST_ID.getName(), plugin.named());
        assertEquals(PluginEnum.AI_REQUEST_ID.getCode(), plugin.getOrder());
        assertEquals(168, plugin.getOrder());
    }

    @Test
    void shouldGenerateContextAndExposeResponseHeader() {
        MockServerWebExchange exchange = exchange();
        AtomicReference<AgentRequestContext> reactorContext = new AtomicReference<>();
        AtomicReference<AgentRequestContext> exchangeContext = new AtomicReference<>();
        ShenyuPluginChain chain = next -> Mono.deferContextual(view -> {
            reactorContext.set(view.get(AgentRequestContext.REACTOR_KEY));
            exchangeContext.set(next.getAttribute(AgentRequestContext.EXCHANGE_ATTRIBUTE));
            return next.getResponse().setComplete();
        });

        StepVerifier.create(plugin.doExecute(exchange, chain, selector(), rule("{}")))
                .verifyComplete();

        assertNotNull(reactorContext.get());
        assertEquals(reactorContext.get(), exchangeContext.get());
        assertEquals("llm", reactorContext.get().getTrafficType());
        assertEquals("selector-1", reactorContext.get().getSelectorId());
        assertEquals("rule-1", reactorContext.get().getRuleId());
        assertTrue(AgentRequestContext.isReusableRequestId(reactorContext.get().getRequestId()));
        assertEquals(reactorContext.get().getRequestId(),
                exchange.getResponse().getHeaders().getFirst(Constants.X_SHENYU_AI_REQUEST_ID));
    }

    @Test
    void shouldReuseExistingExchangeContextWithoutReplacingFacts() {
        MockServerWebExchange exchange = exchange();
        AgentRequestContext existing = new AgentRequestContext("existing-id", "llm", "existing-selector", "existing-rule")
                .withIdentity("tenant", "subject");
        exchange.getAttributes().put(AgentRequestContext.EXCHANGE_ATTRIBUTE, existing);
        AtomicReference<AgentRequestContext> captured = new AtomicReference<>();
        ShenyuPluginChain chain = next -> Mono.deferContextual(view -> {
            captured.set(view.get(AgentRequestContext.REACTOR_KEY));
            return next.getResponse().setComplete();
        });

        StepVerifier.create(plugin.doExecute(exchange, chain, selector(), rule("{}")))
                .verifyComplete();

        assertEquals(existing, captured.get());
        assertEquals("existing-id", exchange.getResponse().getHeaders().getFirst(Constants.X_SHENYU_AI_REQUEST_ID));
    }

    @Test
    void shouldReuseReactorContextWhenExchangeAttributeIsMissing() {
        MockServerWebExchange exchange = exchange();
        AgentRequestContext existing = new AgentRequestContext("reactor-id", "llm", "s", "r");
        AtomicReference<AgentRequestContext> captured = new AtomicReference<>();
        ShenyuPluginChain chain = next -> Mono.deferContextual(view -> {
            captured.set(view.get(AgentRequestContext.REACTOR_KEY));
            return next.getResponse().setComplete();
        });

        StepVerifier.create(plugin.doExecute(exchange, chain, selector(), rule("{}"))
                        .contextWrite(context -> context.put(AgentRequestContext.REACTOR_KEY, existing)))
                .verifyComplete();

        assertEquals(existing, captured.get());
        assertEquals(existing, exchange.getAttribute(AgentRequestContext.EXCHANGE_ATTRIBUTE));
        assertEquals("reactor-id", exchange.getResponse().getHeaders().getFirst(Constants.X_SHENYU_AI_REQUEST_ID));
    }

    @Test
    void shouldPreferExchangeContextOverReactorContext() {
        MockServerWebExchange exchange = exchange();
        AgentRequestContext exchangeContext = new AgentRequestContext("exchange-id", "llm", "s", "r");
        AgentRequestContext reactorContext = new AgentRequestContext("reactor-id", "llm", "s", "r");
        exchange.getAttributes().put(AgentRequestContext.EXCHANGE_ATTRIBUTE, exchangeContext);
        AtomicReference<AgentRequestContext> captured = new AtomicReference<>();
        ShenyuPluginChain chain = next -> Mono.deferContextual(view -> {
            captured.set(view.get(AgentRequestContext.REACTOR_KEY));
            return next.getResponse().setComplete();
        });

        StepVerifier.create(plugin.doExecute(exchange, chain, selector(), rule("{}"))
                        .contextWrite(context -> context.put(AgentRequestContext.REACTOR_KEY, reactorContext)))
                .verifyComplete();

        assertEquals(exchangeContext, captured.get());
    }

    @Test
    void shouldReplaceBlankExistingIdentifier() {
        MockServerWebExchange exchange = exchange();
        exchange.getAttributes().put(AgentRequestContext.EXCHANGE_ATTRIBUTE,
                new AgentRequestContext("   ", "llm", "s", "r"));
        AtomicReference<AgentRequestContext> captured = new AtomicReference<>();
        ShenyuPluginChain chain = next -> Mono.deferContextual(view -> {
            captured.set(view.get(AgentRequestContext.REACTOR_KEY));
            return next.getResponse().setComplete();
        });

        StepVerifier.create(plugin.doExecute(exchange, chain, selector(), rule("{}")))
                .verifyComplete();

        assertNotNull(captured.get());
        assertNotEquals("   ", captured.get().getRequestId());
        assertTrue(AgentRequestContext.isReusableRequestId(captured.get().getRequestId()));
    }

    @Test
    void shouldReplaceExistingIdentifierWithUnsafeCharacters() {
        MockServerWebExchange exchange = exchange();
        exchange.getAttributes().put(AgentRequestContext.EXCHANGE_ATTRIBUTE,
                new AgentRequestContext("bad id", "llm", "s", "r"));
        AtomicReference<AgentRequestContext> captured = new AtomicReference<>();
        ShenyuPluginChain chain = next -> Mono.deferContextual(view -> {
            captured.set(view.get(AgentRequestContext.REACTOR_KEY));
            return next.getResponse().setComplete();
        });

        StepVerifier.create(plugin.doExecute(exchange, chain, selector(), rule("{}")))
                .verifyComplete();

        assertNotNull(captured.get());
        assertNotEquals("bad id", captured.get().getRequestId());
        assertTrue(AgentRequestContext.isReusableRequestId(captured.get().getRequestId()));
    }

    @Test
    void shouldReplaceOverlongExistingIdentifier() {
        MockServerWebExchange exchange = exchange();
        String overlong = "a".repeat(129);
        exchange.getAttributes().put(AgentRequestContext.EXCHANGE_ATTRIBUTE,
                new AgentRequestContext(overlong, "llm", "s", "r"));
        AtomicReference<AgentRequestContext> captured = new AtomicReference<>();
        ShenyuPluginChain chain = next -> Mono.deferContextual(view -> {
            captured.set(view.get(AgentRequestContext.REACTOR_KEY));
            return next.getResponse().setComplete();
        });

        StepVerifier.create(plugin.doExecute(exchange, chain, selector(), rule("{}")))
                .verifyComplete();

        assertNotNull(captured.get());
        assertNotEquals(overlong, captured.get().getRequestId());
        assertTrue(AgentRequestContext.isReusableRequestId(captured.get().getRequestId()));
    }

    @Test
    void shouldNotAddResponseHeaderWhenDisabled() {
        MockServerWebExchange exchange = exchange();
        AtomicReference<AgentRequestContext> captured = new AtomicReference<>();
        ShenyuPluginChain chain = next -> Mono.deferContextual(view -> {
            captured.set(view.get(AgentRequestContext.REACTOR_KEY));
            return next.getResponse().setComplete();
        });

        StepVerifier.create(plugin.doExecute(exchange, chain, selector(),
                        rule("{\"responseHeaderEnabled\":false}")))
                .verifyComplete();

        assertNotNull(captured.get());
        assertNull(exchange.getResponse().getHeaders().getFirst(Constants.X_SHENYU_AI_REQUEST_ID));
    }

    @Test
    void shouldUseDefaultRuleForNonContinuedSelector() {
        MockServerWebExchange exchange = exchange();
        AtomicReference<AgentRequestContext> captured = new AtomicReference<>();
        ShenyuPluginChain chain = next -> Mono.deferContextual(view -> {
            captured.set(view.get(AgentRequestContext.REACTOR_KEY));
            return next.getResponse().setComplete();
        });
        SelectorData selector = SelectorData.builder().id("selector-1").continued(false).build();
        RuleData rule = RuleData.builder().id(Constants.DEFAULT_RULE).selectorId("selector-1").build();

        StepVerifier.create(plugin.doExecute(exchange, chain, selector, rule))
                .verifyComplete();

        assertNotNull(captured.get());
        assertEquals(captured.get().getRequestId(),
                exchange.getResponse().getHeaders().getFirst(Constants.X_SHENYU_AI_REQUEST_ID));
    }

    @Test
    void shouldNotTrustClientSuppliedHeader() {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/ai")
                .header(Constants.X_SHENYU_AI_REQUEST_ID, "client-request-id"));
        AtomicReference<AgentRequestContext> captured = new AtomicReference<>();
        ShenyuPluginChain chain = next -> Mono.deferContextual(view -> {
            captured.set(view.get(AgentRequestContext.REACTOR_KEY));
            return next.getResponse().setComplete();
        });

        StepVerifier.create(plugin.doExecute(exchange, chain, selector(), rule("{}")))
                .verifyComplete();

        assertNotNull(captured.get());
        assertNotEquals("client-request-id", captured.get().getRequestId());
        assertNotEquals("client-request-id",
                exchange.getResponse().getHeaders().getFirst(Constants.X_SHENYU_AI_REQUEST_ID));
    }

    @Test
    void shouldRejectInvalidRuleHandleBeforeCallingDownstream() {
        MockServerWebExchange exchange = exchange();
        AtomicBoolean called = new AtomicBoolean();
        ShenyuPluginChain chain = next -> {
            called.set(true);
            return Mono.empty();
        };

        StepVerifier.create(plugin.doExecute(exchange, chain, selector(), rule("not-json")))
                .verifyComplete();

        assertFalse(called.get());
        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, exchange.getResponse().getStatusCode());
        AgentRequestContext context = exchange.getAttribute(AgentRequestContext.EXCHANGE_ATTRIBUTE);
        assertNotNull(context);
        assertTrue(AgentRequestContext.isReusableRequestId(context.getRequestId()));
        assertEquals(context.getRequestId(),
                exchange.getResponse().getHeaders().getFirst(Constants.X_SHENYU_AI_REQUEST_ID));
    }

    @Test
    void shouldRejectNonBooleanResponseHeaderFlag() {
        MockServerWebExchange exchange = exchange();
        AtomicBoolean called = new AtomicBoolean();
        ShenyuPluginChain chain = next -> {
            called.set(true);
            return Mono.empty();
        };

        StepVerifier.create(plugin.doExecute(exchange, chain, selector(),
                        rule("{\"responseHeaderEnabled\":\"false\"}")))
                .verifyComplete();

        assertFalse(called.get());
        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, exchange.getResponse().getStatusCode());
        AgentRequestContext context = exchange.getAttribute(AgentRequestContext.EXCHANGE_ATTRIBUTE);
        assertNotNull(context);
        assertEquals(context.getRequestId(),
                exchange.getResponse().getHeaders().getFirst(Constants.X_SHENYU_AI_REQUEST_ID));
    }

    @Test
    void shouldCorrelateInvalidRuleWithExistingContext() {
        MockServerWebExchange exchange = exchange();
        AgentRequestContext existing = new AgentRequestContext("existing-id", "llm", "existing-selector", "existing-rule");
        exchange.getAttributes().put(AgentRequestContext.EXCHANGE_ATTRIBUTE, existing);

        StepVerifier.create(plugin.doExecute(exchange, next -> Mono.empty(), selector(), rule("not-json")))
                .verifyComplete();

        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, exchange.getResponse().getStatusCode());
        assertEquals(existing, exchange.getAttribute(AgentRequestContext.EXCHANGE_ATTRIBUTE));
        assertEquals("existing-id",
                exchange.getResponse().getHeaders().getFirst(Constants.X_SHENYU_AI_REQUEST_ID));
    }

    @Test
    void shouldPassThroughWhenPluginIsNotEnabled() {
        ApplicationContext applicationContext = mock(ApplicationContext.class);
        when(applicationContext.getBean(ShenyuConfig.class)).thenReturn(new ShenyuConfig());
        SpringBeanUtils.getInstance().setApplicationContext(applicationContext);
        BaseDataCache.getInstance().removePluginDataByPluginName(PluginEnum.AI_REQUEST_ID.getName());
        MockServerWebExchange exchange = exchange();
        AtomicBoolean called = new AtomicBoolean();
        ShenyuPluginChain chain = next -> {
            called.set(true);
            return next.getResponse().setComplete();
        };

        StepVerifier.create(plugin.execute(exchange, chain)).verifyComplete();

        assertTrue(called.get());
        assertNull(exchange.getAttribute(AgentRequestContext.EXCHANGE_ATTRIBUTE));
        assertNull(exchange.getResponse().getHeaders().getFirst(Constants.X_SHENYU_AI_REQUEST_ID));
    }

    @Test
    void shouldSkipInternalChildInvocation() {
        ServerWebExchange child = MockServerWebExchange.from(MockServerHttpRequest.post("/tool").build());
        child.getAttributes().put(AgentRequestContext.HTTP_TOOL_CALL_ATTRIBUTE, true);
        ServerWebExchange notChild = MockServerWebExchange.from(MockServerHttpRequest.post("/ai").build());
        notChild.getAttributes().put(AgentRequestContext.HTTP_TOOL_CALL_ATTRIBUTE, false);

        assertTrue(plugin.skip(child));
        assertFalse(plugin.skip(notChild));
        assertFalse(plugin.skip(exchange()));
    }

    @Test
    void shouldPreserveRequestIdAcrossSchedulerSwitch() {
        MockServerWebExchange exchange = exchange();
        Scheduler scheduler = Schedulers.newSingle("ai-request-id-context-test");
        AtomicReference<AgentRequestContext> captured = new AtomicReference<>();
        ShenyuPluginChain chain = next -> Mono.delay(Duration.ofMillis(5), scheduler)
                .then(Mono.deferContextual(view -> {
                    captured.set(view.get(AgentRequestContext.REACTOR_KEY));
                    return next.getResponse().setComplete();
                }));

        try {
            StepVerifier.create(plugin.doExecute(exchange, chain, selector(), rule("{}")))
                    .verifyComplete();
        } finally {
            scheduler.dispose();
        }

        assertNotNull(captured.get());
        assertEquals(captured.get().getRequestId(),
                exchange.getResponse().getHeaders().getFirst(Constants.X_SHENYU_AI_REQUEST_ID));
    }

    @Test
    void shouldGenerateDistinctIdsForConcurrentRequests() {
        ConcurrentLinkedQueue<String> requestIds = new ConcurrentLinkedQueue<>();
        Flux<Void> executions = Flux.range(0, 32).flatMap(index -> {
            MockServerWebExchange exchange = exchange();
            ShenyuPluginChain chain = next -> Mono.deferContextual(view -> {
                requestIds.add(view.<AgentRequestContext>get(AgentRequestContext.REACTOR_KEY).getRequestId());
                return next.getResponse().setComplete();
            });
            return plugin.doExecute(exchange, chain, selector(), rule("{}"));
        }, 16);

        StepVerifier.create(executions).verifyComplete();

        assertEquals(32, requestIds.size());
        assertEquals(32, Set.copyOf(requestIds).size());
    }

    @Test
    void shouldNotSubscribeRequestBody() {
        AtomicBoolean consumed = new AtomicBoolean();
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/ai")
                .body(Flux.defer(() -> {
                    consumed.set(true);
                    return Flux.empty();
                })));
        ShenyuPluginChain chain = next -> next.getResponse().setComplete();

        StepVerifier.create(plugin.doExecute(exchange, chain, selector(), rule("{}")))
                .verifyComplete();

        assertFalse(consumed.get());
    }

    private MockServerWebExchange exchange() {
        return MockServerWebExchange.from(MockServerHttpRequest.post("/ai").build());
    }

    private SelectorData selector() {
        return SelectorData.builder().id("selector-1").continued(true).build();
    }

    private RuleData rule(final String handle) {
        return RuleData.builder().id("rule-1").selectorId("selector-1").handle(handle).build();
    }
}
