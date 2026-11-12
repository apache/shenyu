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
import io.modelcontextprotocol.spec.McpServerTransportProvider;
import org.apache.shenyu.plugin.mcp.server.holder.ShenyuMcpExchangeHolder;
import org.apache.shenyu.plugin.mcp.server.manager.ShenyuMcpServerManager;
import org.awaitility.Awaitility;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.http.codec.ServerSentEvent;
import org.springframework.http.server.reactive.ReactorHttpHandlerAdapter;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.reactive.function.client.WebClient;
import org.springframework.web.reactive.function.server.HandlerStrategies;
import org.springframework.web.reactive.function.server.RequestPredicates;
import org.springframework.web.reactive.function.server.RouterFunctions;
import org.springframework.web.reactive.function.server.ServerRequest;
import org.springframework.web.server.ResponseStatusException;
import reactor.core.Disposable;
import reactor.core.publisher.Mono;
import reactor.netty.DisposableServer;
import reactor.netty.http.server.HttpServer;
import reactor.netty.resources.LoopResources;
import reactor.test.StepVerifier;

import java.time.Duration;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class LegacySessionBoundsTest {

    private static final String HEADER = "Mcp-Session-Id";

    private static final String INITIALIZE = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\","
            + "\"params\":{\"protocolVersion\":\"2025-03-26\",\"capabilities\":{},\"clientInfo\":{\"name\":\"fixture\",\"version\":\"1\"}}}";

    private static final String LIST = "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/list\",\"params\":{}}";

    @AfterEach
    void cleanup() {
        ShenyuMcpExchangeHolder.clear();
    }

    @Test
    void streamableCapacityIncludesTemporarySessionsAndFailedHandshakeReleasesIt() {
        try (ShenyuStreamableHttpServerTransportProvider provider = httpProvider(McpSessionPolicy.unlimited().idleTimeout())) {
            factory(provider);
            final String id = initialize(provider);
            assertEquals(HttpStatus.SERVICE_UNAVAILABLE, provider.handleUnifiedEndpoint(post(INITIALIZE, null)).block().statusCode());
            assertEquals(HttpStatus.SERVICE_UNAVAILABLE, provider.handleUnifiedEndpoint(post(LIST, null)).block().statusCode());
            assertEquals(HttpStatus.SERVICE_UNAVAILABLE, provider.handleUnifiedEndpoint(post(LIST, "unknown")).block().statusCode());
            provider.removeSession(id);
            assertNull(ShenyuMcpExchangeHolder.get(id));
            assertEquals(HttpStatus.BAD_REQUEST, provider.handleUnifiedEndpoint(post(INITIALIZE.replace("2025-03-26", "unsupported"), null)).block().statusCode());
            assertNotNull(initialize(provider));
        }
    }

    @Test
    void streamableIdleReleasesHolderAndFactoryFailureDoesNotConsumeSlot() {
        try (ShenyuStreamableHttpServerTransportProvider provider = httpProvider(Duration.ofSeconds(1))) {
            provider.setSessionFactory(transport -> {
                throw new IllegalStateException("fixture failure");
            });
            assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, provider.handleUnifiedEndpoint(post(INITIALIZE, null)).block().statusCode());
            factory(provider);
            final String id = initialize(provider);
            assertNotNull(ShenyuMcpExchangeHolder.get(id));
            Awaitility.await().atMost(Duration.ofSeconds(5)).until(() -> Objects.isNull(ShenyuMcpExchangeHolder.get(id)));
            assertNotNull(initialize(provider));
        }
    }

    @Test
    void bothSseEntryPathsUseOneCapacityAndIdleOwner() {
        try (ShenyuSseServerTransportProvider provider = sseProvider(Duration.ofSeconds(1))) {
            factory(provider);
            final AtomicReference<String> id = new AtomicReference<>();
            StepVerifier.create(provider.createSseFlux(null))
                    .assertNext(event -> {
                        id.set(sessionId(event));
                        assertTrue(provider.hasSession(id.get()));
                    })
                    .then(() -> StepVerifier.create(provider.createSseFlux(null)).expectError(ResponseStatusException.class).verify())
                    .expectComplete().verify(Duration.ofSeconds(5));
            assertFalse(provider.hasSession(id.get()));
            assertNotNull(provider.handleSseConnection(null).block());
            StepVerifier.create(provider.createSseFlux(null)).expectNextCount(1).thenCancel().verify();
            provider.close();
            StepVerifier.create(provider.createSseFlux(null)).expectError().verify();
        }
    }

    @Test
    void sseMessagesHoldActivityUntilCompletionAndCancellation() {
        try (ShenyuSseServerTransportProvider provider = sseProvider(Duration.ofSeconds(1))) {
            final McpServerSession session = mock(McpServerSession.class);
            when(session.getId()).thenReturn("active-sse");
            when(session.handle(any())).thenReturn(Mono.never());
            provider.setSessionFactory(transport -> session);
            final Disposable stream = provider.createSseFlux(null).subscribe();
            final Disposable operation = provider.handleMessageEndpoint(message("active-sse")).subscribe();
            try {
                Awaitility.await().during(Duration.ofSeconds(2)).atMost(Duration.ofSeconds(5)).until(() -> provider.hasSession("active-sse"));
                operation.dispose();
                Awaitility.await().atMost(Duration.ofSeconds(5)).until(() -> !provider.hasSession("active-sse"));
            } finally {
                operation.dispose();
                stream.dispose();
            }
        }
    }

    @Test
    void sseVoidHandlersReturnResponsesOnBothMessageEntrypoints() {
        try (ShenyuSseServerTransportProvider provider = sseProvider(Duration.ZERO)) {
            final McpServerSession session = mock(McpServerSession.class);
            when(session.getId()).thenReturn("void-sse");
            when(session.handle(any())).thenReturn(Mono.empty());
            provider.setSessionFactory(transport -> session);
            final Disposable stream = provider.createSseFlux(null).subscribe();
            try {
                assertEquals(200, provider.handleMessageEndpoint(message("void-sse")).block().getStatusCode());
                assertEquals(HttpStatus.OK, provider.handleMessage(message("void-sse")).block().statusCode());
            } finally {
                stream.dispose();
            }
            assertFalse(provider.hasSession("void-sse"));
        }
    }

    @Test
    void failedSseFactoryAndCancelledStreamReleaseCapacityAndBindings() {
        try (ShenyuSseServerTransportProvider provider = sseProvider(Duration.ZERO)) {
            provider.setSessionFactory(transport -> {
                throw new IllegalStateException("factory");
            });
            StepVerifier.create(provider.createSseFlux(null)).expectError(IllegalStateException.class).verify(Duration.ofSeconds(5));
            factory(provider);
            final AtomicReference<String> id = new AtomicReference<>();
            StepVerifier.create(provider.createSseFlux(null)).assertNext(event -> {
                id.set(sessionId(event));
                assertTrue(provider.bindExchange(id.get(), MockServerWebExchange.from(MockServerHttpRequest.get("/sse").build())));
            }).thenCancel().verify(Duration.ofSeconds(5));
            assertNull(ShenyuMcpExchangeHolder.get(id.get()));
            assertFalse(provider.bindExchange(id.get(), MockServerWebExchange.from(MockServerHttpRequest.get("/late").build())));
            StepVerifier.create(provider.createSseFlux(null)).expectNextCount(1).thenCancel().verify(Duration.ofSeconds(5));
        }
    }

    @Test
    void streamableActiveHandlerSurvivesIdleExpiryAndCancellationStartsIdleClock() {
        try (ShenyuStreamableHttpServerTransportProvider provider = httpProvider(Duration.ofSeconds(1))) {
            final McpServerSession session = mock(McpServerSession.class);
            when(session.getId()).thenReturn("active-http");
            when(session.handle(any())).thenReturn(Mono.never());
            provider.setSessionFactory(transport -> session);
            final String id = initialize(provider);
            final Disposable operation = provider.handleUnifiedEndpoint(post(LIST, id)).subscribe();
            try {
                Awaitility.await().during(Duration.ofSeconds(2)).atMost(Duration.ofSeconds(5))
                        .until(() -> Objects.nonNull(ShenyuMcpExchangeHolder.get(id)));
                operation.dispose();
                Awaitility.await().atMost(Duration.ofSeconds(5)).until(() -> Objects.isNull(ShenyuMcpExchangeHolder.get(id)));
                assertFalse(provider.bindExchange(id, MockServerWebExchange.from(MockServerHttpRequest.get("/late").build())));
            } finally {
                operation.dispose();
            }
        }
    }

    @Test
    void managerShutdownClosesBothProtocolsAndRejectsRecreation() {
        final ShenyuMcpServerManager manager = new ShenyuMcpServerManager(null, new McpSessionPolicy(1, Duration.ZERO, Duration.ofSeconds(1)));
        final ShenyuStreamableHttpServerTransportProvider http = manager.getOrCreateStreamableHttpTransport("/test/streamablehttp");
        final ShenyuSseServerTransportProvider sse = manager.getOrCreateMcpServerTransport("/test/sse", "/message");
        final String id = initialize(http);
        final Disposable stream = sse.createSseFlux(null).subscribe();
        try {
            manager.close();
            assertNull(ShenyuMcpExchangeHolder.get(id));
            assertEquals(HttpStatus.SERVICE_UNAVAILABLE, http.handleUnifiedEndpoint(post(INITIALIZE, null)).block().statusCode());
            assertThrows(IllegalStateException.class, () -> manager.getOrCreateStreamableHttpTransport("/other/streamablehttp"));
        } finally {
            manager.close();
            stream.dispose();
        }
    }

    @Test
    void realHttpAndSseReclaimIdleSessionsAndAdmitReplacements() throws Exception {
        try (ShenyuStreamableHttpServerTransportProvider http = httpProvider(Duration.ofSeconds(2));
                ShenyuSseServerTransportProvider sse = sseProvider(Duration.ofSeconds(2))) {
            factory(http);
            factory(sse);
            final LoopResources loops = LoopResources.create("session-bounds", 2, true);
            final ReactorHttpHandlerAdapter adapter = new ReactorHttpHandlerAdapter(RouterFunctions.toHttpHandler(
                    RouterFunctions.route(RequestPredicates.POST("/mcp"), http::handleUnifiedEndpoint)
                            .andRoute(RequestPredicates.GET("/sse"), sse::handleSseConnection)));
            final DisposableServer server = HttpServer.create().host("127.0.0.1").port(0).runOn(loops).handle(adapter).bindNow();
            final WebClient client = WebClient.create("http://127.0.0.1:" + server.port());
            final CountDownLatch connected = new CountDownLatch(1);
            final CountDownLatch expired = new CountDownLatch(1);
            final AtomicReference<String> endpoint = new AtomicReference<>();
            final Disposable stream = client.get().uri("/sse").retrieve().bodyToFlux(String.class).doOnNext(data -> {
                endpoint.set(data);
                connected.countDown();
            }).doOnComplete(expired::countDown).subscribe();
            try {
                assertTrue(connected.await(10, TimeUnit.SECONDS));
                assertTrue(endpoint.get().contains("sessionId="));
                final ResponseEntity<String> initialized = networkInit(client);
                assertEquals(HttpStatus.OK, initialized.getStatusCode());
                final String id = initialized.getHeaders().getFirst(HEADER);
                assertNotNull(id);
                assertEquals(HttpStatus.SERVICE_UNAVAILABLE, networkInit(client).getStatusCode());
                assertTrue(expired.await(10, TimeUnit.SECONDS));
                Awaitility.await().atMost(Duration.ofSeconds(10)).until(() -> Objects.isNull(ShenyuMcpExchangeHolder.get(id)));
                assertEquals(HttpStatus.OK, networkInit(client).getStatusCode());
                assertNotNull(client.get().uri("/sse").retrieve().bodyToFlux(String.class).next().block(Duration.ofSeconds(10)));
            } finally {
                stream.dispose();
                server.disposeNow();
                loops.disposeLater().block(Duration.ofSeconds(10));
            }
        }
    }

    private ResponseEntity<String> networkInit(final WebClient client) {
        return client.post().uri("/mcp").contentType(MediaType.APPLICATION_JSON).bodyValue(INITIALIZE)
                .exchangeToMono(response -> response.toEntity(String.class)).block(Duration.ofSeconds(10));
    }

    private ShenyuStreamableHttpServerTransportProvider httpProvider(final Duration idle) {
        return ShenyuStreamableHttpServerTransportProvider.builder().objectMapper(new ObjectMapper()).endpoint("/mcp")
                .sessionPolicy(new McpSessionPolicy(1, idle, Duration.ofSeconds(1))).build();
    }

    private ShenyuSseServerTransportProvider sseProvider(final Duration idle) {
        return ShenyuSseServerTransportProvider.builder().objectMapper(new ObjectMapper()).messageEndpoint("/message")
                .sessionPolicy(new McpSessionPolicy(1, idle, Duration.ofSeconds(1))).build();
    }

    private void factory(final McpServerTransportProvider provider) {
        provider.setSessionFactory(transport -> new McpServerSession(UUID.randomUUID().toString(), Duration.ofSeconds(5), transport,
                request -> Mono.just(new McpSchema.InitializeResult("2025-03-26", McpSchema.ServerCapabilities.builder().build(),
                        new McpSchema.Implementation("fixture", "1"), "fixture")), Map.of(), Map.of()));
    }

    private String initialize(final ShenyuStreamableHttpServerTransportProvider provider) {
        final String id = provider.handleUnifiedEndpoint(post(INITIALIZE, null)).block().headers().getFirst(HEADER);
        assertNotNull(id);
        return id;
    }

    private ServerRequest post(final String body, final String id) {
        final MockServerHttpRequest.BodyBuilder builder = MockServerHttpRequest.post("/mcp").contentType(MediaType.APPLICATION_JSON);
        if (Objects.nonNull(id)) {
            builder.header(HEADER, id);
        }
        return ServerRequest.create(MockServerWebExchange.from(builder.body(body)), HandlerStrategies.withDefaults().messageReaders());
    }

    private ServerRequest message(final String id) {
        return ServerRequest.create(MockServerWebExchange.from(MockServerHttpRequest.post("/message?sessionId=" + id)
                .contentType(MediaType.APPLICATION_JSON).body(LIST)), HandlerStrategies.withDefaults().messageReaders());
    }

    private String sessionId(final ServerSentEvent<?> event) {
        final String endpoint = event.data().toString();
        return endpoint.substring(endpoint.indexOf("sessionId=") + "sessionId=".length());
    }
}
