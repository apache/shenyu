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

package org.apache.shenyu.plugin.agent.gateway.tool;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.AgentGatewayHttpToolConfig;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.ContextMappingRuleHandle;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.agent.gateway.AgentGatewayConstants;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.utils.RequestUrlUtils;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.context.path.ContextPathPlugin;
import org.apache.shenyu.plugin.context.path.handler.ContextPathPluginDataHandler;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.core.io.buffer.DataBufferLimitException;
import org.springframework.http.HttpStatus;
import org.springframework.http.HttpCookie;
import org.springframework.http.MediaType;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;
import reactor.core.scheduler.Schedulers;

import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.util.Set;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Function;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Fresh cursors, independent exchanges/responses, bounded capture and downstream cancellation.
 */
class AgentHttpToolExecutorTest {

    @BeforeEach
    void preparePluginCaches() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        ShenyuConfig config = new ShenyuConfig();
        config.getSelectorMatchCache().getCache().setEnabled(false);
        config.getRuleMatchCache().getCache().setEnabled(false);
        when(context.getBean(ShenyuConfig.class)).thenReturn(config);
        SpringBeanUtils.getInstance().setApplicationContext(context);
    }

    @AfterEach
    void cleanPluginCaches() {
        BaseDataCache.getInstance().cleanPluginData();
        BaseDataCache.getInstance().cleanSelectorData();
        BaseDataCache.getInstance().cleanRuleData();
        ContextPathPluginDataHandler.CACHED_HANDLE.get().getAllCache().clear();
    }

    @Test
    void forwardsEncodedQueryArgumentsExactlyOnceThroughTheOrdinaryHttpRoute() {
        // The route the executor hands to the ordinary chain is the same input RequestUrlUtils receives in
        // production, so building the upstream URI here proves what the gateway would actually send.
        RecordingChain chain = new RecordingChain(child -> {
            assertEquals("http://127.0.0.1:18080/orders/status?id=A%26B",
                    RequestUrlUtils.buildRequestUri(child, "http://127.0.0.1:18080").toString());
            return write(child, "{}");
        });
        StepVerifier.create(executor(origin(), chain, 4096).invoke(tool("GET"), invocation("A&B"))).expectNextCount(1).verifyComplete();
        assertEquals(1, chain.forks.get());
    }

    @Test
    void forwardsABodyArgumentWithoutInventingAQuery() {
        RecordingChain chain = new RecordingChain(child -> {
            assertEquals("http://127.0.0.1:18080/orders/status",
                    RequestUrlUtils.buildRequestUri(child, "http://127.0.0.1:18080").toString());
            return write(child, "{}");
        });
        StepVerifier.create(executor(origin(), chain, 4096).invoke(tool("POST"), invocation("A&B"))).expectNextCount(1).verifyComplete();
    }

    @Test
    void forwardsAQuerylessRouteWithoutAddingAQuestionMark() {
        RecordingChain chain = new RecordingChain(child -> {
            assertEquals("http://127.0.0.1:18080/orders/status",
                    RequestUrlUtils.buildRequestUri(child, "http://127.0.0.1:18080").toString());
            return write(child, "{}");
        });
        StepVerifier.create(executor(origin(), chain, 4096).invoke(toolWithoutArguments(), invocation(new JsonObject())))
                .expectNextCount(1).verifyComplete();
    }

    @Test
    void keepsTheContextPathRewriteWithoutDuplicatingTheQuery() {
        cacheContextPathRule("/api", "context-rule");
        final ContextPathPlugin contextPath = new ContextPathPlugin();
        RecordingChain chain = new RecordingChain(child -> contextPath.execute(child, routed -> {
            // The context path plugin only rewrites realUrl; the encoded argument must survive untouched.
            assertEquals("/orders/status", ((ShenyuContext) routed.getAttribute(Constants.CONTEXT)).getRealUrl());
            assertEquals("http://127.0.0.1:18080/orders/status?id=A%26B",
                    RequestUrlUtils.buildRequestUri(routed, "http://127.0.0.1:18080").toString());
            return write(routed, "{}");
        }));
        StepVerifier.create(executor(origin(), chain, 4096).invoke(tool("GET", "/api/orders/status"), invocation("A&B")))
                .expectNextCount(1).verifyComplete();
    }

    @Test
    void doesNotReuseClientBodyHeadersCookiesAttributesOrResponse() {
        final MockServerWebExchange origin = origin();
        origin.getAttributes().put("private", "parent-only");
        RecordingChain chain = new RecordingChain(child -> {
            assertFalse(child.getAttributes().containsKey("private"));
            assertTrue(Boolean.TRUE.equals(child.getAttribute(AgentGatewayConstants.HTTP_TOOL_CALL_ATTRIBUTE)));
            assertTrue(child.getRequest().getCookies().isEmpty());
            assertFalse(child.getRequest().getHeaders().containsKey("Authorization"));
            assertFalse(child.getRequest().getHeaders().containsKey("Cookie"));
            assertFalse(child.getRequest().getHeaders().containsKey("Mcp-Name"));
            assertEquals("/orders/status?id=A%26B", child.getRequest().getURI().getRawPath() + "?" + child.getRequest().getURI().getRawQuery());
            child.getAttributes().put("private", "child");
            return write(child, "{\"value\":\"ok\"}");
        });
        StepVerifier.create(executor(origin, chain, 4096).invoke(tool("GET"), invocation("A&B")))
                .assertNext(result -> assertEquals("ok", result.getAsJsonObject("body").get("value").getAsString())).verifyComplete();
        assertEquals("parent-only", origin.getAttribute("private"));
        assertFalse(origin.getResponse().isCommitted());
        assertEquals("Bearer must-not-forward", origin.getRequest().getHeaders().getFirst("Authorization"));
        assertEquals("private=session", origin.getRequest().getHeaders().getFirst("Cookie"));
        assertEquals(1, chain.forks.get());
        assertEquals(0, chain.parentExecutions.get());
    }

    @Test
    void mapsPostBodyWithoutBorrowingTheMcpBody() {
        RecordingChain chain = new RecordingChain(child -> org.springframework.core.io.buffer.DataBufferUtils.join(child.getRequest().getBody())
                .flatMap(buffer -> {
                    try {
                        byte[] bytes = new byte[buffer.readableByteCount()];
                        buffer.read(bytes);
                        assertEquals("{\"id\":\"A\"}", new String(bytes, StandardCharsets.UTF_8));
                    } finally {
                        org.springframework.core.io.buffer.DataBufferUtils.release(buffer);
                    }
                    return write(child, "{\"posted\":true}");
                }));
        StepVerifier.create(executor(origin(), chain, 4096).invoke(tool("POST"), invocation("A")))
                .assertNext(result -> assertTrue(result.getAsJsonObject("body").get("posted").getAsBoolean())).verifyComplete();
    }

    @ParameterizedTest
    @ValueSource(ints = {0, 1, 2})
    void conditionalChecksNeverBorrowOrMutateTheMcpResponse(final int variant) {
        final MockServerWebExchange origin = MockServerWebExchange.from(MockServerHttpRequest.post("/agent")
                .header("If-Match", "\"different-parent-etag\"").header("If-Unmodified-Since", "Wed, 21 Oct 2015 07:28:00 GMT"));
        RecordingChain chain = new RecordingChain(child -> {
            assertEquals("\"different-parent-etag\"", origin.getRequest().getHeaders().getFirst("If-Match"));
            boolean modified = switch (variant) {
                case 0 -> child.checkNotModified("\"parent-etag\"");
                case 1 -> child.checkNotModified(Instant.parse("2026-10-12T00:00:00Z"));
                default -> child.checkNotModified("\"parent-etag\"", Instant.parse("2026-10-12T00:00:00Z"));
            };
            assertFalse(modified);
            assertFalse(child.isNotModified());
            return write(child, "{}");
        });
        StepVerifier.create(executor(origin, chain, 4096).invoke(tool("GET"), invocation("A"))).expectNextCount(1).verifyComplete();
        assertFalse(origin.isNotModified());
        assertEquals(null, origin.getResponse().getStatusCode());
        assertTrue(origin.getResponse().getHeaders().isEmpty());
        assertFalse(origin.getResponse().isCommitted());
    }

    @Test
    void urlTransformersAndMultipartCleanupNeverBorrowTheParentExchange() {
        final MockServerWebExchange origin = origin();
        origin.addUrlTransformer(url -> "parent:" + url);
        RecordingChain chain = new RecordingChain(child -> {
            assertEquals("route", child.transformUrl("route"));
            child.addUrlTransformer(url -> "child:" + url);
            assertEquals("child:route", child.transformUrl("route"));
            assertEquals("parent:route", origin.transformUrl("route"));
            return child.cleanupMultipart().then(write(child, "{}"));
        });
        StepVerifier.create(executor(origin, chain, 4096).invoke(tool("GET"), invocation("A"))).expectNextCount(1).verifyComplete();
        assertEquals("parent:route", origin.transformUrl("route"));
    }

    @Test
    void sameClientIdAndConcurrentSubscriptionsKeepPrivateStateAndFreshCursors() {
        final RecordingChain chain = new RecordingChain(child -> Mono.deferContextual(context -> {
            String id = child.getRequest().getQueryParams().getFirst("id");
            child.getAttributes().put("private", id);
            assertEquals("trusted", context.get("test-context"));
            return write(child, "{\"id\":\"" + id + "\"}");
        }).subscribeOn(Schedulers.parallel()));
        final AgentHttpToolExecutor executor = executor(origin(), chain, 4096);
        StepVerifier.create(Flux.range(0, 32).flatMap(id -> executor.invoke(tool("GET"), invocation("id" + id))
                .map(result -> result.getAsJsonObject("body").get("id").getAsString())).collectList()
                .contextWrite(context -> context.put("test-context", "trusted")))
                .assertNext(values -> assertEquals(32, Set.copyOf(values).size())).verifyComplete();
        assertEquals(32, chain.forks.get());
    }

    @Test
    void cancellationPropagatesWithoutExecutingASecondAttempt() {
        final AtomicBoolean cancelled = new AtomicBoolean();
        RecordingChain chain = new RecordingChain(child -> Mono.<Void>never().doOnCancel(() -> cancelled.set(true)));
        StepVerifier.create(executor(origin(), chain, 4096).invoke(tool("GET"), invocation("A"))).thenCancel().verify();
        assertTrue(cancelled.get());
        assertEquals(1, chain.forks.get());
    }

    @Test
    void rejectsAnExpiredDeadlineBeforeForking() {
        RecordingChain chain = new RecordingChain(child -> write(child, "{}"));
        AgentToolInvocation input = new AgentToolInvocation("same-id", "subject", arguments("A"), "rule", 1, Instant.EPOCH);
        StepVerifier.create(executor(origin(), chain, 4096).invoke(tool("GET"), input)).expectError(TimeoutException.class).verify();
        assertEquals(0, chain.forks.get());
    }

    @Test
    void refusesOversizedResponsesAndRequests() {
        RecordingChain response = new RecordingChain(child -> write(child, "{\"large\":\"" + "x".repeat(2000) + "\"}"));
        StepVerifier.create(executor(origin(), response, 1024).invoke(tool("GET"), invocation("A"))).expectError(DataBufferLimitException.class).verify();
        RecordingChain request = new RecordingChain(child -> write(child, "{}"));
        AgentHttpToolExecutor bounded = new AgentHttpToolExecutor(origin(), request, 8, 1024);
        StepVerifier.create(bounded.invoke(tool("GET"), invocation("A"))).expectError(DataBufferLimitException.class).verify();
        assertEquals(0, request.forks.get());
    }

    @ParameterizedTest
    @ValueSource(strings = {"{\"x\":1,\"x\":2}", "{}{}", "not-json", "[[", "{\"deep\":"})
    void rejectsMalformedOrAmbiguousJson(final String body) {
        RecordingChain chain = new RecordingChain(child -> write(child, body));
        StepVerifier.create(executor(origin(), chain, 4096).invoke(tool("GET"), invocation("A"))).expectError().verify();
    }

    @ParameterizedTest
    @ValueSource(ints = {301, 400, 401, 404, 429, 500})
    void doesNotFollowRedirectsOrReturnErrorBodies(final int status) {
        RecordingChain chain = new RecordingChain(child -> {
            child.getResponse().setStatusCode(HttpStatus.valueOf(status));
            return write(child, "{\"secret\":\"upstream\"}");
        });
        StepVerifier.create(executor(origin(), chain, 4096).invoke(tool("GET"), invocation("A"))).expectError(AgentToolExecutionException.class).verify();
        assertEquals(1, chain.forks.get());
    }

    @Test
    void rejectsAbsentRoutesSessionsAndNonJsonResults() {
        RecordingChain absent = new RecordingChain(child -> Mono.empty());
        StepVerifier.create(executor(origin(), absent, 4096).invoke(tool("GET"), invocation("A"))).expectError(AgentToolExecutionException.class).verify();
        RecordingChain session = new RecordingChain(child -> child.getSession().then());
        StepVerifier.create(executor(origin(), session, 4096).invoke(tool("GET"), invocation("A"))).expectError(UnsupportedOperationException.class).verify();
        RecordingChain html = new RecordingChain(child -> {
            child.getResponse().getHeaders().setContentType(MediaType.TEXT_HTML);
            return child.getResponse().writeWith(Mono.just(child.getResponse().bufferFactory().wrap("<html/>".getBytes(StandardCharsets.UTF_8))));
        });
        StepVerifier.create(executor(origin(), html, 4096).invoke(tool("GET"), invocation("A"))).expectError(AgentToolExecutionException.class).verify();
    }

    @Test
    void customChainsMustOptIntoForking() {
        ShenyuPluginChain unsupported = child -> write(child, "{}");
        StepVerifier.create(executor(origin(), unsupported, 4096).invoke(tool("GET"), invocation("A"))).expectError(UnsupportedOperationException.class).verify();
    }

    private AgentHttpToolExecutor executor(final ServerWebExchange origin, final ShenyuPluginChain chain, final int maximum) {
        return new AgentHttpToolExecutor(origin, chain, 4096, maximum);
    }

    private MockServerWebExchange origin() {
        return MockServerWebExchange.from(MockServerHttpRequest.post("/agent")
                .header("Authorization", "Bearer must-not-forward").header("Cookie", "private=session").header("Mcp-Name", "lookup")
                .cookie(new HttpCookie("private", "session")).body("{\"rpc\":\"must-not-forward\"}"));
    }

    private AgentGatewayHttpToolConfig.Tool tool(final String method) {
        return tool(method, "/orders/status");
    }

    private AgentGatewayHttpToolConfig.Tool tool(final String method, final String url) {
        JsonObject plans = JsonParser.parseString("""
                {"lookup":{"description":"Look up a record","inputSchema":{"type":"object","properties":{"id":{"type":"string"}},"required":["id"]},
                "requestConfig":{"requestTemplate":{"method":"GET","url":"/orders/status"},"argsPosition":{"id":"query"}}}}
                """).getAsJsonObject();
        plans.getAsJsonObject("lookup").getAsJsonObject("requestConfig").getAsJsonObject("requestTemplate").addProperty("method", method);
        plans.getAsJsonObject("lookup").getAsJsonObject("requestConfig").getAsJsonObject("requestTemplate").addProperty("url", url);
        plans.getAsJsonObject("lookup").getAsJsonObject("requestConfig").getAsJsonObject("argsPosition").addProperty("id", "POST".equals(method) ? "body" : "query");
        return AgentGatewayHttpToolConfig.parse(plans, Set.of("lookup")).getTools().get("lookup");
    }

    private AgentGatewayHttpToolConfig.Tool toolWithoutArguments() {
        JsonObject plans = JsonParser.parseString("""
                {"lookup":{"description":"Look up a record","inputSchema":{"type":"object","properties":{}},
                "requestConfig":{"requestTemplate":{"method":"GET","url":"/orders/status"},"argsPosition":{}}}}
                """).getAsJsonObject();
        return AgentGatewayHttpToolConfig.parse(plans, Set.of("lookup")).getTools().get("lookup");
    }

    private void cacheContextPathRule(final String contextPath, final String ruleId) {
        PluginData plugin = new PluginData();
        plugin.setName(new ContextPathPlugin().named());
        plugin.setEnabled(true);
        BaseDataCache.getInstance().cachePluginData(plugin);
        SelectorData selector = new SelectorData();
        selector.setId("context-selector");
        selector.setPluginName(plugin.getName());
        selector.setType(SelectorTypeEnum.FULL_FLOW.getCode());
        selector.setContinued(true);
        selector.setEnabled(true);
        selector.setLogged(false);
        selector.setSort(0);
        BaseDataCache.getInstance().cacheSelectData(selector);
        RuleData rule = new RuleData();
        rule.setId(ruleId);
        rule.setSelectorId(selector.getId());
        rule.setEnabled(true);
        rule.setLogged(false);
        rule.setSort(0);
        ContextMappingRuleHandle handle = new ContextMappingRuleHandle();
        handle.setContextPath(contextPath);
        rule.setHandle(GsonUtils.getInstance().toJson(handle));
        new ContextPathPluginDataHandler().handlerRule(rule);
        BaseDataCache.getInstance().cacheRuleData(rule);
    }

    private AgentToolInvocation invocation(final String id) {
        return invocation(arguments(id));
    }

    private AgentToolInvocation invocation(final JsonObject value) {
        return new AgentToolInvocation("same-id", "subject", value, "rule", 1, Instant.now().plusSeconds(10));
    }

    private JsonObject arguments(final String id) {
        JsonObject value = new JsonObject();
        value.addProperty("id", id);
        return value;
    }

    private static Mono<Void> write(final ServerWebExchange child, final String body) {
        child.getResponse().getHeaders().setContentType(MediaType.APPLICATION_JSON);
        return child.getResponse().writeWith(Mono.just(child.getResponse().bufferFactory().wrap(body.getBytes(StandardCharsets.UTF_8))));
    }

    private static final class RecordingChain implements ShenyuPluginChain {

        private final Function<ServerWebExchange, Mono<Void>> action;

        private final AtomicInteger forks = new AtomicInteger();

        private final AtomicInteger parentExecutions = new AtomicInteger();

        RecordingChain(final Function<ServerWebExchange, Mono<Void>> action) {
            this.action = action;
        }

        @Override
        public Mono<Void> execute(final ServerWebExchange exchange) {
            parentExecutions.incrementAndGet();
            return Mono.error(new IllegalStateException("Original cursor must not run"));
        }

        @Override
        public ShenyuPluginChain fork() {
            forks.incrementAndGet();
            return action::apply;
        }
    }
}
