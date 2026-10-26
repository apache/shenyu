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

package org.apache.shenyu.plugin.mcp.server.transport;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.time.Duration;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import com.fasterxml.jackson.databind.ObjectMapper;
import io.modelcontextprotocol.server.McpRequestHandler;
import io.modelcontextprotocol.spec.McpSchema;
import io.modelcontextprotocol.spec.McpServerSession;
import io.modelcontextprotocol.spec.McpServerTransport;
import org.apache.shenyu.plugin.mcp.server.holder.ShenyuMcpExchangeHolder;
import org.apache.shenyu.plugin.mcp.server.session.McpSessionHelper;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.http.codec.HttpMessageWriter;
import org.springframework.http.server.reactive.ReactorHttpHandlerAdapter;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.http.server.reactive.MockServerHttpResponse;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.reactive.function.client.WebClient;
import org.springframework.web.reactive.function.server.HandlerStrategies;
import org.springframework.web.reactive.function.server.RequestPredicates;
import org.springframework.web.reactive.function.server.RouterFunctions;
import org.springframework.web.reactive.function.server.ServerRequest;
import org.springframework.web.reactive.function.server.ServerResponse;
import org.springframework.web.reactive.result.view.ViewResolver;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.Disposable;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;
import reactor.netty.http.server.HttpServer;
import reactor.netty.resources.LoopResources;
import reactor.test.StepVerifier;
import reactor.util.context.Context;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test case for {@link ShenyuStreamableHttpServerTransportProvider}.
 */
class ShenyuStreamableHttpServerTransportProviderTest {

    private static final String SESSION_ID_HEADER = "Mcp-Session-Id";

    private static final String INITIALIZE_REQUEST_BODY = "{\"jsonrpc\":\"2.0\",\"id\":\"init-1\",\"method\":\"initialize\","
            + "\"params\":{\"protocolVersion\":\"2025-03-26\",\"capabilities\":{},"
            + "\"clientInfo\":{\"name\":\"test-client\",\"version\":\"1.0.0\"}}}";

    private static final String TOOLS_LIST_REQUEST_BODY = "{\"jsonrpc\":\"2.0\",\"id\":\"req-1\",\"method\":\"tools/list\","
            + "\"params\":{}}";

    private static final String INITIALIZED_NOTIFICATION_BODY = "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\","
            + "\"params\":{}}";

    private static final String CANCELLED_NOTIFICATION_BODY = "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/cancelled\","
            + "\"params\":{\"requestId\":\"req-1\",\"reason\":\"test\"}}";

    private static final ServerResponse.Context RESPONSE_CONTEXT = new ServerResponse.Context() {
        @Override
        public List<HttpMessageWriter<?>> messageWriters() {
            return HandlerStrategies.withDefaults().messageWriters();
        }

        @Override
        public List<ViewResolver> viewResolvers() {
            return Collections.emptyList();
        }
    };

    @AfterEach
    void tearDown() {
        ShenyuMcpExchangeHolder.clear();
    }

    @Test
    void testPreflightUsesConfiguredHeadersAndMethods() {
        ShenyuStreamableHttpServerTransportProvider provider =
                new ShenyuStreamableHttpServerTransportProvider(new ObjectMapper(),
                        "/mcp/streamablehttp", "Content-Type, XRequest");
        ServerRequest request = createRequest(MockServerHttpRequest.options("/mcp/streamablehttp")
                .header("Origin", "http://localhost:6274")
                .header("Access-Control-Request-Headers", "xrequest, authorization")
                .build());

        StepVerifier.create(provider.handleUnifiedEndpoint(request))
                .assertNext(response -> {
                    assertEquals(HttpStatus.OK, response.statusCode());
                    assertEquals("POST, OPTIONS",
                            response.headers().getFirst("Access-Control-Allow-Methods"));
                    assertEquals("xrequest",
                            response.headers().getFirst("Access-Control-Allow-Headers"));
                    assertTrue(response.headers().getVary().contains("Origin"));
                    assertTrue(response.headers().getVary().contains("Access-Control-Request-Headers"));
                })
                .verifyComplete();
    }

    @Test
    void testPreflightUsesFallbackHeaders() {
        ShenyuStreamableHttpServerTransportProvider provider =
                new ShenyuStreamableHttpServerTransportProvider(new ObjectMapper(),
                        "/mcp/streamablehttp");
        ServerRequest request = createRequest(MockServerHttpRequest.options("/mcp/streamablehttp")
                .header("Origin", "http://localhost:6274")
                .header("Access-Control-Request-Headers", "xrequest")
                .build());

        ServerResponse response = provider.handleUnifiedEndpoint(request).block();
        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.statusCode());
        String allowHeaders = response.headers().getFirst("Access-Control-Allow-Headers");
        assertTrue(allowHeaders.toLowerCase(Locale.ROOT).contains("xrequest"));
    }

    /**
     * JSON-RPC notifications on an existing session must be acknowledged with
     * HTTP 202 and an empty body instead of a fabricated JSON-RPC response.
     */
    @Test
    void testNotificationWithExistingSessionReturnsAcceptedEmptyBody() {
        ShenyuStreamableHttpServerTransportProvider provider = providerWithRealSessions();
        MockServerHttpResponse initResponse = performRequest(provider, postRequest(INITIALIZE_REQUEST_BODY, null));
        String sessionId = initResponse.getHeaders().getFirst(SESSION_ID_HEADER);
        assertNotNull(sessionId);

        MockServerHttpResponse notificationResponse = performRequest(provider, postRequest(CANCELLED_NOTIFICATION_BODY, sessionId));
        assertEquals(HttpStatus.ACCEPTED, notificationResponse.getStatusCode());
        assertEquals(sessionId, notificationResponse.getHeaders().getFirst(SESSION_ID_HEADER));
        assertEquals("", notificationResponse.getBodyAsString().block());
    }

    /**
     * A notification sent after a business request must not replay the stale
     * response captured by the previous request on the same session.
     */
    @Test
    void testNotificationAfterRequestDoesNotReplayStaleResponse() {
        ShenyuStreamableHttpServerTransportProvider provider = providerWithRealSessions();
        MockServerHttpResponse initResponse = performRequest(provider, postRequest(INITIALIZE_REQUEST_BODY, null));
        String sessionId = initResponse.getHeaders().getFirst(SESSION_ID_HEADER);
        assertNotNull(sessionId);

        // Complete the handshake state so the business request handler can run.
        MockServerHttpResponse initializedResponse = performRequest(provider, postRequest(INITIALIZED_NOTIFICATION_BODY, sessionId));
        assertEquals(HttpStatus.ACCEPTED, initializedResponse.getStatusCode());
        assertEquals("", initializedResponse.getBodyAsString().block());

        // Business request populates the captured transport response.
        MockServerHttpResponse toolsResponse = performRequest(provider, postRequest(TOOLS_LIST_REQUEST_BODY, sessionId));
        assertEquals(HttpStatus.OK, toolsResponse.getStatusCode());
        String toolsBody = toolsResponse.getBodyAsString().block();
        assertTrue(toolsBody.contains("\"id\":\"req-1\""));
        assertTrue(toolsBody.contains("\"tools\""));

        // Notification on the same session must be 202 with an empty body
        // instead of replaying the stale tools/list response.
        MockServerHttpResponse notificationResponse = performRequest(provider, postRequest(CANCELLED_NOTIFICATION_BODY, sessionId));
        assertEquals(HttpStatus.ACCEPTED, notificationResponse.getStatusCode());
        assertEquals("", notificationResponse.getBodyAsString().block());
    }

    /**
     * A request carrying a session ID that no longer exists on the server must not
     * leave the restore-created session, transport, or exchange mapping behind after
     * the request completes.
     */
    @Test
    void testStaleSessionRestoreCleansUpCreatedSession() throws Exception {
        ShenyuStreamableHttpServerTransportProvider provider = providerWithRealSessions();

        MockServerHttpResponse response = performRequest(provider, postRequest(TOOLS_LIST_REQUEST_BODY, "stale-session-1"));
        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertTrue(response.getBodyAsString().block().contains("\"tools\""));
        final String actualSessionId = response.getHeaders().getFirst(SESSION_ID_HEADER);
        assertNotNull(actualSessionId);

        assertEquals(0, readMap(provider, "sessions").size());
        assertEquals(0, readMap(provider, "sessionTransports").size());
        assertNull(ShenyuMcpExchangeHolder.get(actualSessionId));

        // The restored session is not reusable, so a follow-up request with the
        // returned session ID is handled by a fresh restore instead of the same session.
        MockServerHttpResponse followUp = performRequest(provider, postRequest(TOOLS_LIST_REQUEST_BODY, actualSessionId));
        assertEquals(HttpStatus.OK, followUp.getStatusCode());
        assertTrue(followUp.getBodyAsString().block().contains("\"tools\""));
        assertNotEquals(actualSessionId, followUp.getHeaders().getFirst(SESSION_ID_HEADER));
        assertEquals(0, readMap(provider, "sessions").size());
        assertEquals(0, readMap(provider, "sessionTransports").size());
    }

    @Test
    void testCompatibilityLookupSharesRequestResponseCleanup() throws Exception {
        final ShenyuStreamableHttpServerTransportProvider provider = providerWithRealSessions();
        final String sessionId = initialize(provider);
        performRequest(provider, postRequest(TOOLS_LIST_REQUEST_BODY, sessionId));

        final Object transport = readMap(provider, "sessionTransports").get(sessionId);
        assertNotNull(transport);
        final Method lookup = transport.getClass().getDeclaredMethod("getLastSentMessage", Object.class);
        lookup.setAccessible(true);
        assertNull(lookup.invoke(transport, "req-1"));

        final Method reset = transport.getClass().getDeclaredMethod("resetCapturedMessage", Object.class);
        reset.setAccessible(true);
        reset.invoke(transport, new Object[] {null});
        assertNull(lookup.invoke(transport, "req-1"));
        provider.removeSession(sessionId);
    }

    /**
     * Regression test for the reported initialize-path session leak (#6833).
     *
     * <p>Before the fix the provider stored the session under the MCP server session ID while the
     * transport kept its own independently auto-generated {@code sessionId}, so {@code close()}
     * looked up a key that never existed in {@code sessions} / {@code sessionTransports} and left
     * both registries plus {@link ShenyuMcpExchangeHolder} populated forever.
     *
     * <p>Assertion: after a real initialize handshake the ID returned to the client
     * ({@code Mcp-Session-Id}) is exactly the key used in both registries, so a later
     * {@code close()} is able to remove the entry.
     */
    @Test
    void testInitializeRegistersSessionUnderReturnedSessionId() throws Exception {
        ShenyuStreamableHttpServerTransportProvider provider = providerWithRealSessions();

        MockServerHttpResponse response = performRequest(provider,
                postRequest(INITIALIZE_REQUEST_BODY, null));
        assertEquals(HttpStatus.OK, response.getStatusCode());

        final String returnedSessionId = response.getHeaders().getFirst(SESSION_ID_HEADER);
        assertNotNull(returnedSessionId);

        final Map<String, ?> sessions = readMap(provider, "sessions");
        final Map<String, ?> transports = readMap(provider, "sessionTransports");
        assertTrue(sessions.containsKey(returnedSessionId),
                "sessions must be keyed by the session ID returned to the client, otherwise close() cannot clean it up");
        assertTrue(transports.containsKey(returnedSessionId),
                "sessionTransports must be keyed by the session ID returned to the client, otherwise close() cannot clean it up");

        // Drive the close path: it is the only thing that exercises the transport's own sessionId,
        // so without the setSessionId() fix nothing would be removed here. Asserting only the map
        // keys above would pass on master too (they were always keyed by the returned id); this part
        // fails when line transport.setSessionId(newSessionId) is reverted.
        final McpServerTransport transport = (McpServerTransport) transports.get(returnedSessionId);
        assertNotNull(transport, "the transport must be reachable via the returned session id");
        transport.closeGracefully().block();

        assertTrue(sessions.isEmpty(), "close() must remove the session from the sessions map");
        assertTrue(transports.isEmpty(), "close() must remove the transport from the sessionTransports map");
        assertNull(ShenyuMcpExchangeHolder.get(returnedSessionId),
                "close() must remove the exchange binding for the returned session id");
    }

    @SuppressWarnings("unchecked")
    private Map<String, ?> readMap(final ShenyuStreamableHttpServerTransportProvider provider, final String fieldName)
            throws Exception {
        final Field field = provider.getClass().getDeclaredField(fieldName);
        field.setAccessible(true);
        return (Map<String, ?>) field.get(provider);
    }

    private ShenyuStreamableHttpServerTransportProvider providerWithRealSessions() {
        return providerWithHandler((exchange, params) -> Mono.just(Map.<String, Object>of("tools", List.of())));
    }

    @Test
    void testConcurrentRequestsKeepExchangeAndResponseCorrelation() throws Exception {
        final ShenyuStreamableHttpServerTransportProvider provider = providerWithHandler((exchange, params) -> {
            final ServerWebExchange requestExchange = (ServerWebExchange) exchange.transportContext()
                    .get(McpSessionHelper.SHENYU_EXCHANGE_CONTEXT_KEY);
            final String marker = requestExchange.getRequest().getHeaders().getFirst("X-Test-Request");
            return Mono.delay(Duration.ofMillis(80 - Integer.parseInt(marker) * 5L))
                    .thenReturn(Map.<String, Object>of("marker", marker));
        });
        final String sessionId = initialize(provider);
        StepVerifier.create(Flux.range(0, 8).flatMap(index -> {
            final String id = Integer.toString(index);
            final MockServerHttpRequest request = MockServerHttpRequest.post("/mcp/streamablehttp")
                    .header("Content-Type", "application/json").header(SESSION_ID_HEADER, sessionId)
                    .header("X-Test-Request", id).body(TOOLS_LIST_REQUEST_BODY.replace("req-1", id));
            final MockServerWebExchange exchange = MockServerWebExchange.from(request);
            return provider.handleUnifiedEndpoint(ServerRequest.create(exchange, HandlerStrategies.withDefaults().messageReaders()))
                    .flatMap(response -> response.writeTo(exchange, RESPONSE_CONTEXT))
                    .then(Mono.defer(() -> exchange.getResponse().getBodyAsString()))
                    .doOnNext(body -> {
                        assertTrue(body.contains("\"id\":\"" + id + "\""), body);
                        assertTrue(body.contains("\"marker\":\"" + id + "\""), body);
                    });
        })).expectNextCount(8).verifyComplete();
        final Object transport = readMap(provider, "sessionTransports").get(sessionId);
        final Field responses = transport.getClass().getDeclaredField("responses");
        responses.setAccessible(true);
        assertTrue(((Map<?, ?>) responses.get(transport)).isEmpty(), "Completed requests must release captured responses");
        provider.removeSession(sessionId);
        assertNull(ShenyuMcpExchangeHolder.get(sessionId));
    }

    @Test
    void testCancelReachesSessionHandler() {
        final AtomicBoolean cancelled = new AtomicBoolean();
        final ShenyuStreamableHttpServerTransportProvider provider = providerWithHandler((exchange, params) ->
                Mono.<Map<String, Object>>never().doOnCancel(() -> cancelled.set(true)));
        final String sessionId = initialize(provider);
        StepVerifier.create(provider.handleUnifiedEndpoint(createRequest(postRequest(TOOLS_LIST_REQUEST_BODY, sessionId))))
                .thenAwait(Duration.ofMillis(100)).thenCancel().verify(Duration.ofSeconds(3));
        assertTrue(cancelled.get());
        provider.removeSession(sessionId);
    }

    private String initialize(final ShenyuStreamableHttpServerTransportProvider provider) {
        final String sessionId = performRequest(provider, postRequest(INITIALIZE_REQUEST_BODY, null))
                .getHeaders().getFirst(SESSION_ID_HEADER);
        assertNotNull(sessionId);
        performRequest(provider, postRequest(INITIALIZED_NOTIFICATION_BODY, sessionId));
        return sessionId;
    }

    @Test
    void testFailedRequestDoesNotPoisonNextRequest() {
        final AtomicBoolean fail = new AtomicBoolean(true);
        final ShenyuStreamableHttpServerTransportProvider provider = providerWithHandler((exchange, params) -> {
            if (fail.getAndSet(false)) {
                return Mono.error(new IllegalStateException("fixture failure"));
            }
            return Mono.just(Map.<String, Object>of("tools", List.of()));
        });
        final String sessionId = initialize(provider);
        final String failed = performRequest(provider, postRequest(TOOLS_LIST_REQUEST_BODY, sessionId)).getBodyAsString().block();
        assertNotNull(failed);
        assertTrue(failed.contains("\"error\""), failed);
        final String succeeded = performRequest(provider, postRequest(TOOLS_LIST_REQUEST_BODY.replace("req-1", "req-2"), sessionId))
                .getBodyAsString().block();
        assertNotNull(succeeded);
        assertTrue(succeeded.contains("\"id\":\"req-2\""), succeeded);
        assertTrue(succeeded.contains("\"tools\""), succeeded);
        provider.removeSession(sessionId);
    }

    @Test
    void testCancellingOneRequestDoesNotCancelAnotherInSameSession() {
        final AtomicBoolean cancelled = new AtomicBoolean();
        final ShenyuStreamableHttpServerTransportProvider provider = providerWithHandler((exchange, params) -> {
            final ServerWebExchange requestExchange = (ServerWebExchange) exchange.transportContext()
                    .get(McpSessionHelper.SHENYU_EXCHANGE_CONTEXT_KEY);
            if ("cancel".equals(requestExchange.getRequest().getHeaders().getFirst("X-Test-Request"))) {
                return Mono.<Map<String, Object>>never().doOnCancel(() -> cancelled.set(true));
            }
            return Mono.just(Map.<String, Object>of("tools", List.of()));
        });
        final String sessionId = initialize(provider);
        final MockServerHttpRequest request = MockServerHttpRequest.post("/mcp/streamablehttp")
                .header("Content-Type", "application/json").header(SESSION_ID_HEADER, sessionId)
                .header("X-Test-Request", "cancel").body(TOOLS_LIST_REQUEST_BODY);
        final Disposable pending = provider.handleUnifiedEndpoint(createRequest(request)).subscribe();
        try {
            final String other = performRequest(provider, postRequest(TOOLS_LIST_REQUEST_BODY.replace("req-1", "req-2"), sessionId))
                    .getBodyAsString().block(Duration.ofSeconds(3));
            assertNotNull(other);
            assertTrue(other.contains("\"id\":\"req-2\""), other);
            assertTrue(other.contains("\"tools\""), other);
            pending.dispose();
            assertTrue(cancelled.get());
            final String next = performRequest(provider, postRequest(TOOLS_LIST_REQUEST_BODY.replace("req-1", "req-3"), sessionId))
                    .getBodyAsString().block(Duration.ofSeconds(3));
            assertNotNull(next);
            assertTrue(next.contains("\"id\":\"req-3\""), next);
        } finally {
            pending.dispose();
            provider.removeSession(sessionId);
        }
    }

    @Test
    void testCompletedRequestReleasesLastResponse() throws Exception {
        final ShenyuStreamableHttpServerTransportProvider provider = providerWithRealSessions();
        final String sessionId = initialize(provider);
        try {
            performRequest(provider, postRequest(TOOLS_LIST_REQUEST_BODY, sessionId));
            final Object transport = readMap(provider, "sessionTransports").get(sessionId);
            final Method lookup = transport.getClass().getDeclaredMethod("getLastSentMessage");
            lookup.setAccessible(true);
            assertNull(lookup.invoke(transport), "A completed request must not retain its result in the session slot");
        } finally {
            provider.removeSession(sessionId);
        }
    }

    @Test
    void testDuplicatePendingIdDoesNotInvokeSecondHandler() {
        final AtomicInteger calls = new AtomicInteger();
        final ShenyuStreamableHttpServerTransportProvider provider = providerWithHandler((exchange, params) ->
                calls.incrementAndGet() == 1 ? Mono.never() : Mono.just(Map.of("marker", "second")));
        final String sessionId = initialize(provider);
        final Disposable pending = provider.handleUnifiedEndpoint(createRequest(postRequest(TOOLS_LIST_REQUEST_BODY, sessionId))).subscribe();
        try {
            assertEquals(1, calls.get());
            final MockServerHttpResponse duplicate = performRequest(provider, postRequest(TOOLS_LIST_REQUEST_BODY, sessionId));
            assertEquals(HttpStatus.BAD_REQUEST, duplicate.getStatusCode());
            assertEquals(1, calls.get(), "Duplicate in-flight IDs must be rejected before a second handler executes");
        } finally {
            pending.dispose();
            provider.removeSession(sessionId);
        }
    }

    @Test
    void testSessionRemovalCancelsPendingHandler() {
        final AtomicBoolean cancelled = new AtomicBoolean();
        final AtomicReference<ServerResponse> result = new AtomicReference<>();
        final ShenyuStreamableHttpServerTransportProvider provider = providerWithHandler((exchange, params) ->
                Mono.<Map<String, Object>>never().doOnCancel(() -> cancelled.set(true)));
        final String sessionId = initialize(provider);
        final Disposable pending = provider.handleUnifiedEndpoint(createRequest(postRequest(TOOLS_LIST_REQUEST_BODY, sessionId))).subscribe(result::set);
        try {
            provider.removeSession(sessionId);
            assertTrue(cancelled.get(), "Closing a session must cancel its owned handler subscription");
            assertNotNull(result.get(), "A closed session must terminate the waiting HTTP request");
            assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, result.get().statusCode());
        } finally {
            pending.dispose();
            provider.removeSession(sessionId);
        }
    }

    @Test
    void testLateResponseAfterCancellationIsDiscarded() throws Exception {
        final AtomicReference<Context> requestContext = new AtomicReference<>();
        final AtomicReference<McpServerTransport> transport = new AtomicReference<>();
        final ShenyuStreamableHttpServerTransportProvider provider = providerWithHandler((exchange, params) ->
                Mono.deferContextual(context -> {
                    requestContext.set(Context.of(context));
                    return Mono.never();
                }), transport);
        final String sessionId = initialize(provider);
        final Disposable pending = provider.handleUnifiedEndpoint(createRequest(postRequest(TOOLS_LIST_REQUEST_BODY, sessionId))).subscribe();
        try {
            assertNotNull(requestContext.get());
            pending.dispose();
            final McpSchema.JSONRPCResponse late = new McpSchema.JSONRPCResponse("2.0", "req-1", Map.of("private", "late-result"), null);
            transport.get().sendMessage(late).contextWrite(requestContext.get()).block(Duration.ofSeconds(3));
            final Field responses = transport.get().getClass().getDeclaredField("responses");
            responses.setAccessible(true);
            assertTrue(((Map<?, ?>) responses.get(transport.get())).isEmpty(), "Late responses must not repopulate cancelled request state");
        } finally {
            pending.dispose();
            provider.removeSession(sessionId);
        }
    }

    @Test
    void testCancelledOwnerCannotOverwriteSameIdSuccessor() {
        final AtomicReference<Context> oldContext = new AtomicReference<>();
        final AtomicReference<McpServerTransport> transport = new AtomicReference<>();
        final AtomicInteger calls = new AtomicInteger();
        final Sinks.One<Map<String, Object>> freshResult = Sinks.one();
        final ShenyuStreamableHttpServerTransportProvider provider = providerWithHandler((exchange, params) -> Mono.deferContextual(context -> {
            if (calls.incrementAndGet() == 1) {
                oldContext.set(Context.of(context));
                return Mono.never();
            }
            return freshResult.asMono();
        }), transport);
        final String sessionId = initialize(provider);
        final Disposable first = provider.handleUnifiedEndpoint(createRequest(postRequest(TOOLS_LIST_REQUEST_BODY, sessionId))).subscribe();
        first.dispose();
        final AtomicReference<ServerResponse> result = new AtomicReference<>();
        final Disposable second = provider.handleUnifiedEndpoint(createRequest(postRequest(TOOLS_LIST_REQUEST_BODY, sessionId))).subscribe(result::set);
        try {
            assertEquals(2, calls.get());
            transport.get().sendMessage(new McpSchema.JSONRPCResponse("2.0", "req-1", Map.of("marker", "old-private"), null))
                    .contextWrite(oldContext.get()).block(Duration.ofSeconds(3));
            assertNull(result.get());
            freshResult.emitValue(Map.of("marker", "fresh"), Sinks.EmitFailureHandler.FAIL_FAST);
            assertNotNull(result.get());
            final MockServerWebExchange output = MockServerWebExchange.from(MockServerHttpRequest.get("/result"));
            result.get().writeTo(output, RESPONSE_CONTEXT).block(Duration.ofSeconds(3));
            final String body = output.getResponse().getBodyAsString().block(Duration.ofSeconds(3));
            assertTrue(body.contains("\"marker\":\"fresh\""), body);
            assertTrue(!body.contains("old-private"), body);
        } finally {
            first.dispose();
            second.dispose();
            provider.removeSession(sessionId);
        }
    }

    @Test
    void testSameIdInDifferentSessionsKeepsPrivateResults() {
        final ShenyuStreamableHttpServerTransportProvider provider = providerWithHandler((exchange, params) -> {
            final ServerWebExchange request = (ServerWebExchange) exchange.transportContext().get(McpSessionHelper.SHENYU_EXCHANGE_CONTEXT_KEY);
            return Mono.delay(Duration.ofMillis(10)).thenReturn(Map.of("marker", request.getRequest().getHeaders().getFirst("X-Test-Request")));
        });
        final List<String> sessions = List.of(initialize(provider), initialize(provider));
        try {
            StepVerifier.create(Flux.range(0, 2).flatMap(index -> {
                final String marker = "private-" + index;
                final MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/mcp/streamablehttp")
                        .header("Content-Type", "application/json").header(SESSION_ID_HEADER, sessions.get(index))
                        .header("X-Test-Request", marker).body(TOOLS_LIST_REQUEST_BODY));
                return provider.handleUnifiedEndpoint(ServerRequest.create(exchange, HandlerStrategies.withDefaults().messageReaders()))
                        .flatMap(response -> response.writeTo(exchange, RESPONSE_CONTEXT))
                        .then(Mono.defer(() -> exchange.getResponse().getBodyAsString()))
                        .doOnNext(body -> assertTrue(body.contains("\"marker\":\"" + marker + "\""), body));
            })).expectNextCount(2).verifyComplete();
        } finally {
            sessions.forEach(provider::removeSession);
        }
    }

    @Test
    void testClosingOneSessionLeavesAnotherRequestActive() {
        final AtomicBoolean firstCancelled = new AtomicBoolean();
        final AtomicBoolean secondCancelled = new AtomicBoolean();
        final Sinks.One<Map<String, Object>> secondResult = Sinks.one();
        final ShenyuStreamableHttpServerTransportProvider provider = providerWithHandler((exchange, params) -> {
            final ServerWebExchange request = (ServerWebExchange) exchange.transportContext().get(McpSessionHelper.SHENYU_EXCHANGE_CONTEXT_KEY);
            return "close".equals(request.getRequest().getHeaders().getFirst("X-Test-Request"))
                    ? Mono.<Map<String, Object>>never().doOnCancel(() -> firstCancelled.set(true))
                    : secondResult.asMono().doOnCancel(() -> secondCancelled.set(true));
        });
        final String firstSession = initialize(provider);
        final String secondSession = initialize(provider);
        final Disposable first = provider.handleUnifiedEndpoint(createRequest(MockServerHttpRequest.post("/mcp/streamablehttp")
                .header("Content-Type", "application/json").header(SESSION_ID_HEADER, firstSession)
                .header("X-Test-Request", "close").body(TOOLS_LIST_REQUEST_BODY))).subscribe();
        final AtomicReference<ServerResponse> result = new AtomicReference<>();
        final Disposable second = provider.handleUnifiedEndpoint(createRequest(postRequest(TOOLS_LIST_REQUEST_BODY, secondSession))).subscribe(result::set);
        try {
            provider.removeSession(firstSession);
            assertTrue(firstCancelled.get());
            assertTrue(!secondCancelled.get());
            assertNull(result.get());
            secondResult.emitValue(Map.of("tools", List.of()), Sinks.EmitFailureHandler.FAIL_FAST);
            assertNotNull(result.get());
            assertEquals(HttpStatus.OK, result.get().statusCode());
        } finally {
            first.dispose();
            second.dispose();
            provider.removeSession(firstSession);
            provider.removeSession(secondSession);
        }
    }

    @Test
    void testCallerDeadlineReleasesPendingRequest() throws Exception {
        final AtomicBoolean cancelled = new AtomicBoolean();
        final ShenyuStreamableHttpServerTransportProvider provider = providerWithHandler((exchange, params) ->
                Mono.<Map<String, Object>>never().doOnCancel(() -> cancelled.set(true)));
        final String sessionId = initialize(provider);
        try {
            StepVerifier.withVirtualTime(() -> provider.handleUnifiedEndpoint(createRequest(postRequest(TOOLS_LIST_REQUEST_BODY, sessionId)))
                    .timeout(Duration.ofSeconds(1)))
                    .thenAwait(Duration.ofSeconds(1)).expectError(TimeoutException.class).verify(Duration.ofSeconds(3));
            assertTrue(cancelled.get());
            final Object transport = readMap(provider, "sessionTransports").get(sessionId);
            final Field pending = transport.getClass().getDeclaredField("pendingResponses");
            pending.setAccessible(true);
            assertTrue(((Map<?, ?>) pending.get(transport)).isEmpty());
        } finally {
            provider.removeSession(sessionId);
        }
    }

    @Test
    void testGracefulShutdownCancelsPendingRequest() throws Exception {
        final AtomicBoolean cancelled = new AtomicBoolean();
        final ShenyuStreamableHttpServerTransportProvider provider = providerWithHandler((exchange, params) ->
                Mono.<Map<String, Object>>never().doOnCancel(() -> cancelled.set(true)));
        final String sessionId = initialize(provider);
        final Disposable pending = provider.handleUnifiedEndpoint(createRequest(postRequest(TOOLS_LIST_REQUEST_BODY, sessionId))).subscribe();
        try {
            StepVerifier.create(provider.closeGracefully()).verifyComplete();
            assertTrue(cancelled.get());
            assertTrue(readMap(provider, "sessions").isEmpty());
            assertTrue(readMap(provider, "sessionTransports").isEmpty());
            assertNull(ShenyuMcpExchangeHolder.get(sessionId));
        } finally {
            pending.dispose();
            provider.removeSession(sessionId);
        }
    }

    @Test
    void testRealHttpDuplicateAndDisconnectKeepPeerHealthy() throws Exception {
        final CountDownLatch started = new CountDownLatch(1);
        final CountDownLatch cancelled = new CountDownLatch(1);
        final AtomicInteger calls = new AtomicInteger();
        final ShenyuStreamableHttpServerTransportProvider provider = providerWithHandler((exchange, params) -> {
            calls.incrementAndGet();
            final ServerWebExchange request = (ServerWebExchange) exchange.transportContext().get(McpSessionHelper.SHENYU_EXCHANGE_CONTEXT_KEY);
            final String marker = request.getRequest().getHeaders().getFirst("X-Test-Request");
            return "pending".equals(marker) ? Mono.<Map<String, Object>>never().doOnSubscribe(subscription -> started.countDown())
                    .doOnCancel(cancelled::countDown) : Mono.just(Map.of("marker", marker));
        });
        final LoopResources loops = LoopResources.create("mcp-lifecycle-test", 2, true);
        final ReactorHttpHandlerAdapter adapter = new ReactorHttpHandlerAdapter(RouterFunctions.toHttpHandler(
                RouterFunctions.route(RequestPredicates.POST("/mcp/streamablehttp"), provider::handleUnifiedEndpoint)));
        final reactor.netty.DisposableServer server = HttpServer.create().host("127.0.0.1").port(0).runOn(loops).handle(adapter).bindNow();
        final WebClient client = WebClient.create("http://127.0.0.1:" + server.port());
        CompletableFuture<ResponseEntity<String>> pending = null;
        String sessionId = null;
        try {
            final ResponseEntity<String> initialized = networkPost(client, INITIALIZE_REQUEST_BODY, "", "init").block(Duration.ofSeconds(10));
            assertNotNull(initialized);
            sessionId = initialized.getHeaders().getFirst(SESSION_ID_HEADER);
            assertNotNull(sessionId);
            assertEquals(HttpStatus.ACCEPTED, networkPost(client, INITIALIZED_NOTIFICATION_BODY, sessionId, "init")
                    .block(Duration.ofSeconds(10)).getStatusCode());
            pending = networkPost(client, TOOLS_LIST_REQUEST_BODY, sessionId, "pending").toFuture();
            assertTrue(started.await(10, TimeUnit.SECONDS));
            final ResponseEntity<String> duplicate = networkPost(client, TOOLS_LIST_REQUEST_BODY, sessionId, "duplicate").block(Duration.ofSeconds(10));
            assertNotNull(duplicate);
            assertEquals(HttpStatus.BAD_REQUEST, duplicate.getStatusCode());
            assertEquals(1, calls.get());
            final ResponseEntity<String> peer = networkPost(client, TOOLS_LIST_REQUEST_BODY.replace("req-1", "req-2"), sessionId, "peer")
                    .block(Duration.ofSeconds(10));
            assertNotNull(peer);
            assertTrue(peer.getBody().contains("\"marker\":\"peer\""), peer.getBody());
            pending.cancel(true);
            assertTrue(cancelled.await(10, TimeUnit.SECONDS), "Real HTTP disconnect must cancel the owned SDK handler");
            final ResponseEntity<String> next = networkPost(client, TOOLS_LIST_REQUEST_BODY.replace("req-1", "req-3"), sessionId, "next")
                    .block(Duration.ofSeconds(10));
            assertNotNull(next);
            assertTrue(next.getBody().contains("\"marker\":\"next\""), next.getBody());
            assertEquals(3, calls.get());
        } finally {
            if (Objects.nonNull(pending)) {
                pending.cancel(true);
            }
            if (Objects.nonNull(sessionId)) {
                provider.removeSession(sessionId);
            }
            server.disposeNow();
            loops.disposeLater().block(Duration.ofSeconds(10));
        }
    }

    private Mono<ResponseEntity<String>> networkPost(final WebClient client, final String body, final String sessionId, final String marker) {
        return client.post().uri("/mcp/streamablehttp").contentType(MediaType.APPLICATION_JSON)
                .header(SESSION_ID_HEADER, sessionId).header("X-Test-Request", marker).bodyValue(body)
                .exchangeToMono(response -> response.toEntity(String.class));
    }

    private ShenyuStreamableHttpServerTransportProvider providerWithHandler(final McpRequestHandler<Map<String, Object>> handler) {
        return providerWithHandler(handler, new AtomicReference<>());
    }

    private ShenyuStreamableHttpServerTransportProvider providerWithHandler(final McpRequestHandler<Map<String, Object>> handler,
                                                                          final AtomicReference<McpServerTransport> capturedTransport) {
        ShenyuStreamableHttpServerTransportProvider provider =
                new ShenyuStreamableHttpServerTransportProvider(new ObjectMapper(), "/mcp/streamablehttp");
        provider.setSessionFactory(transport -> {
            capturedTransport.set(transport);
            return new McpServerSession(
                UUID.randomUUID().toString(),
                Duration.ofSeconds(10),
                transport,
                initializeRequest -> Mono.just(new McpSchema.InitializeResult(
                        "2025-03-26",
                        McpSchema.ServerCapabilities.builder().build(),
                        new McpSchema.Implementation("ShenyuMcpServer", "1.0.0"),
                        "test")),
                Map.<String, McpRequestHandler<?>>of("tools/list", handler),
                Map.of());
        });
        return provider;
    }

    private MockServerHttpRequest postRequest(final String body, final String sessionId) {
        MockServerHttpRequest.BodyBuilder builder = MockServerHttpRequest.post("/mcp/streamablehttp")
                .header("Content-Type", "application/json");
        if (Objects.nonNull(sessionId)) {
            builder.header(SESSION_ID_HEADER, sessionId);
        }
        return builder.body(body);
    }

    private MockServerHttpResponse performRequest(final ShenyuStreamableHttpServerTransportProvider provider,
                                                  final MockServerHttpRequest request) {
        MockServerWebExchange exchange = MockServerWebExchange.from(request);
        ServerRequest serverRequest = ServerRequest.create(exchange, HandlerStrategies.withDefaults().messageReaders());
        ServerResponse response = provider.handleUnifiedEndpoint(serverRequest).block();
        assertNotNull(response);
        response.writeTo(exchange, RESPONSE_CONTEXT).block();
        return exchange.getResponse();
    }

    private ServerRequest createRequest(final MockServerHttpRequest request) {
        return ServerRequest.create(
                MockServerWebExchange.from(request),
                HandlerStrategies.withDefaults().messageReaders()
        );
    }
}
