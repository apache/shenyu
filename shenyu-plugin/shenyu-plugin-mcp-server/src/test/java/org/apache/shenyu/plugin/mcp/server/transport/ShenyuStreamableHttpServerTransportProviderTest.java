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

import com.fasterxml.jackson.databind.ObjectMapper;
import io.modelcontextprotocol.spec.McpSchema;
import io.modelcontextprotocol.spec.McpServerSession;
import io.modelcontextprotocol.server.McpRequestHandler;
import org.apache.shenyu.plugin.mcp.server.session.McpSessionHelper;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.Disposable;
import java.util.concurrent.atomic.AtomicBoolean;
import org.apache.shenyu.plugin.mcp.server.holder.ShenyuMcpExchangeHolder;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpStatus;
import org.springframework.http.codec.HttpMessageWriter;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.http.server.reactive.MockServerHttpResponse;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.reactive.function.server.HandlerStrategies;
import org.springframework.web.reactive.function.server.ServerRequest;
import org.springframework.web.reactive.function.server.ServerResponse;
import org.springframework.web.reactive.result.view.ViewResolver;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.lang.reflect.Field;
import java.time.Duration;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;

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

    private ShenyuStreamableHttpServerTransportProvider providerWithHandler(final McpRequestHandler<Map<String, Object>> handler) {
        ShenyuStreamableHttpServerTransportProvider provider =
                new ShenyuStreamableHttpServerTransportProvider(new ObjectMapper(), "/mcp/streamablehttp");
        provider.setSessionFactory(transport -> new McpServerSession(
                UUID.randomUUID().toString(),
                Duration.ofSeconds(10),
                transport,
                initializeRequest -> Mono.just(new McpSchema.InitializeResult(
                        "2025-03-26",
                        McpSchema.ServerCapabilities.builder().build(),
                        new McpSchema.Implementation("ShenyuMcpServer", "1.0.0"),
                        "test")),
                Map.<String, McpRequestHandler<?>>of("tools/list", handler),
                Map.of()));
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
