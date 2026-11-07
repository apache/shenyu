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

import io.modelcontextprotocol.json.jackson.JacksonMcpJsonMapper;
import io.modelcontextprotocol.spec.McpSchema;
import io.modelcontextprotocol.spec.McpServerSession;
import io.modelcontextprotocol.spec.McpServerTransport;
import org.apache.shenyu.plugin.mcp.server.holder.ShenyuMcpExchangeHolder;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.reactivestreams.Subscription;
import org.springframework.http.codec.ServerSentEvent;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.reactive.function.server.HandlerStrategies;
import org.springframework.web.reactive.function.server.RequestPredicates;
import org.springframework.web.reactive.function.server.RouterFunction;
import org.springframework.web.reactive.function.server.RouterFunctions;
import org.springframework.web.reactive.function.server.ServerRequest;
import org.springframework.http.server.reactive.ReactorHttpHandlerAdapter;
import reactor.core.publisher.BaseSubscriber;
import reactor.core.publisher.Mono;
import reactor.netty.DisposableServer;
import reactor.netty.http.server.HttpServer;
import reactor.netty.resources.LoopResources;
import org.springframework.web.reactive.function.client.WebClient;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.BooleanSupplier;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

/**
 * Exercises the bounded admission and slow-consumer policy of
 * {@link ShenyuSseServerTransportProvider} through the real transport: controlled
 * zero-demand subscriptions prove the finite overflow outcome, the UTF-8 byte budget
 * and the idempotent terminal path, while loopback SSE connections cover the
 * {@code handleSseConnection} entry, ordering and abrupt disconnects.
 */
class ShenyuSseServerTransportProviderTest {

    private static final Duration SEND_TIMEOUT = Duration.ofSeconds(5);

    private static final Duration AWAIT_TIMEOUT = Duration.ofSeconds(5);

    private final CopyOnWriteArrayList<BaseSubscriber<ServerSentEvent<?>>> subscriptions =
            new CopyOnWriteArrayList<>();

    @AfterEach
    void tearDown() {
        subscriptions.forEach(BaseSubscriber::dispose);
        ShenyuMcpExchangeHolder.clear();
    }

    @Test
    void zeroDemandOverflowCompletesTheStreamAndCleansState() throws Exception {
        List<McpServerTransport> transports = new CopyOnWriteArrayList<>();
        List<McpServerSession> sessions = new CopyOnWriteArrayList<>();
        SseSessionPolicy policy = SseSessionPolicy.builder()
                .maxPendingEvents(2)
                .maxPendingBytes(4096)
                .stallTimeout(Duration.ofSeconds(30))
                .build();
        ShenyuSseServerTransportProvider provider = newProvider(policy, transports, sessions);
        CountDownLatch completed = new CountDownLatch(1);
        BaseSubscriber<ServerSentEvent<?>> subscriber = noDemandSubscriber(completed);
        provider.createSseFlux(request()).subscribe(subscriber);

        McpServerTransport transport = awaitTransport(transports);
        final String sessionId = awaitSession(sessions);

        transport.sendMessage(notification("m1")).block(SEND_TIMEOUT);
        transport.sendMessage(notification("m2")).block(SEND_TIMEOUT);
        assertEquals(2, provider.getTotalPendingEvents());

        SseSessionRejectException reject = assertThrows(SseSessionRejectException.class,
                () -> transport.sendMessage(notification("m3")).block(SEND_TIMEOUT));
        assertEquals(SseSessionState.Reservation.OVERFLOW, reject.getReservation());

        assertEquals(1, provider.getTerminalCount(SseSessionState.TerminalReason.OUTPUT_OVERFLOW));
        assertEquals(0, provider.getTerminalCount(SseSessionState.TerminalReason.CLIENT_DISCONNECTED));
        assertEquals(2, provider.getAcceptedSendCount());
        assertEquals(1, provider.getRejectedSendCount());
        assertEquals(0, provider.getTotalPendingEvents());
        awaitTrue(() -> !provider.hasSession(sessionId), "session must be removed after overflow");
        assertTrue(Objects.isNull(ShenyuMcpExchangeHolder.get(sessionId)), "exchange holder entry must be removed");

        // the stream terminates deterministically for a consumer that resumes reading:
        // buffered events drain in order, then completion arrives
        subscriber.request(10);
        assertTrue(completed.await(AWAIT_TIMEOUT.toMillis(), TimeUnit.MILLISECONDS),
                "terminated stream must complete after the consumer drains its buffer");
    }

    @Test
    void byteBudgetCountsSerializedUtf8BytesNotCharacters() throws Exception {
        ObjectMapperHolder holder = new ObjectMapperHolder();
        McpSchema.JSONRPCMessage ascii = new McpSchema.JSONRPCNotification("2.0", "m", Map.of("p", "aaa"));
        final McpSchema.JSONRPCMessage multiByte = new McpSchema.JSONRPCNotification("2.0", "m", Map.of("p", "沉沉沉"));
        int asciiBytes = holder.lengthOf(ascii);
        assertTrue(asciiBytes > 0);

        // budget fits exactly one ASCII payload: the multi-byte message serializes to
        // more UTF-8 bytes although its Java character count is identical
        SseSessionPolicy policy = SseSessionPolicy.builder()
                .maxPendingEvents(10)
                .maxPendingBytes(asciiBytes)
                .stallTimeout(Duration.ofSeconds(30))
                .build();

        List<McpServerTransport> transports = new CopyOnWriteArrayList<>();
        ShenyuSseServerTransportProvider provider = newProvider(policy, transports, new CopyOnWriteArrayList<>());
        BaseSubscriber<ServerSentEvent<?>> subscriber = noDemandSubscriber(new CountDownLatch(1));
        provider.createSseFlux(request()).subscribe(subscriber);
        McpServerTransport transport = awaitTransport(transports);

        transport.sendMessage(ascii).block(SEND_TIMEOUT);
        assertEquals(asciiBytes, provider.getTotalPendingBytes());

        SseSessionRejectException reject = assertThrows(SseSessionRejectException.class,
                () -> transport.sendMessage(multiByte).block(SEND_TIMEOUT));
        assertEquals(SseSessionState.Reservation.OVERSIZED, reject.getReservation());
        assertEquals(1, provider.getRejectedSendCount());
        assertTrue(provider.getTerminalCount(SseSessionState.TerminalReason.OUTPUT_OVERFLOW) == 0,
                "an oversized single message must not terminate the session");
    }

    @Test
    void oversizedMessageKeepsTheSessionUsable() throws Exception {
        ObjectMapperHolder holder = new ObjectMapperHolder();
        McpSchema.JSONRPCMessage small = new McpSchema.JSONRPCNotification("2.0", "s", Map.of());
        McpSchema.JSONRPCMessage big = new McpSchema.JSONRPCNotification("2.0", "b", Map.of("p", "xxxxxxxxxxxxxxxxxxxx"));
        int smallBytes = holder.lengthOf(small);
        assertTrue(holder.lengthOf(big) > smallBytes, "test payload assumption");

        SseSessionPolicy policy = SseSessionPolicy.builder()
                .maxPendingEvents(10)
                .maxPendingBytes(smallBytes)
                .stallTimeout(Duration.ofSeconds(30))
                .build();
        List<McpServerTransport> transports = new CopyOnWriteArrayList<>();
        ShenyuSseServerTransportProvider provider = newProvider(policy, transports, new CopyOnWriteArrayList<>());
        CountDownLatch completed = new CountDownLatch(1);
        BaseSubscriber<ServerSentEvent<?>> subscriber = noDemandSubscriber(completed);
        provider.createSseFlux(request()).subscribe(subscriber);
        McpServerTransport transport = awaitTransport(transports);

        SseSessionRejectException reject = assertThrows(SseSessionRejectException.class,
                () -> transport.sendMessage(big).block(SEND_TIMEOUT));
        assertEquals(SseSessionState.Reservation.OVERSIZED, reject.getReservation());

        // the session stays usable for a fitting message and the stream stays open
        transport.sendMessage(small).block(SEND_TIMEOUT);
        assertEquals(smallBytes, provider.getTotalPendingBytes());
        assertFalse(completed.await(300, TimeUnit.MILLISECONDS), "session must survive an oversized rejection");
    }

    @Test
    void endpointEventIsDeliveredFirstAndOrderIsPreserved() throws Exception {
        List<McpServerTransport> transports = new CopyOnWriteArrayList<>();
        ShenyuSseServerTransportProvider provider =
                newProvider(SseSessionPolicy.defaults(), transports, new CopyOnWriteArrayList<>());
        LinkedBlockingQueue<ServerSentEvent<?>> received = new LinkedBlockingQueue<>();
        provider.createSseFlux(request()).subscribe(received::offer);

        McpServerTransport transport = awaitTransport(transports);
        transport.sendMessage(notification("m1")).block(SEND_TIMEOUT);
        transport.sendMessage(notification("m2")).block(SEND_TIMEOUT);
        transport.sendMessage(notification("m3")).block(SEND_TIMEOUT);

        ServerSentEvent<?> first = received.poll(AWAIT_TIMEOUT.toSeconds(), TimeUnit.SECONDS);
        assertNotNull(first);
        assertEquals("endpoint", first.event());
        for (String method : List.of("m1", "m2", "m3")) {
            ServerSentEvent<?> event = received.poll(AWAIT_TIMEOUT.toSeconds(), TimeUnit.SECONDS);
            assertNotNull(event, "event for " + method + " must arrive in order");
            assertEquals("message", event.event());
            assertTrue(String.valueOf(event.data()).contains(method), "expected " + method);
        }
    }

    @Test
    void idleSessionWithoutPendingOutputSurvivesTheStallWindow() throws Exception {
        List<McpServerTransport> transports = new CopyOnWriteArrayList<>();
        SseSessionPolicy policy = SseSessionPolicy.builder()
                .maxPendingEvents(8)
                .maxPendingBytes(4096)
                .stallTimeout(Duration.ofMillis(150))
                .build();
        ShenyuSseServerTransportProvider provider = newProvider(policy, transports, new CopyOnWriteArrayList<>());
        CountDownLatch completed = new CountDownLatch(1);
        BaseSubscriber<ServerSentEvent<?>> subscriber = noDemandSubscriber(completed);
        provider.createSseFlux(request()).subscribe(subscriber);
        awaitTransport(transports);

        Thread.sleep(500);
        assertFalse(completed.await(1, TimeUnit.MILLISECONDS), "a healthy idle session must not time out");
        assertEquals(0, provider.getTerminalCount(SseSessionState.TerminalReason.OUTPUT_STALLED));
    }

    @Test
    void stalledPendingOutputTerminatesWithinTheStallBound() throws Exception {
        List<McpServerTransport> transports = new CopyOnWriteArrayList<>();
        List<McpServerSession> sessions = new CopyOnWriteArrayList<>();
        SseSessionPolicy policy = SseSessionPolicy.builder()
                .maxPendingEvents(8)
                .maxPendingBytes(4096)
                .stallTimeout(Duration.ofMillis(150))
                .build();
        ShenyuSseServerTransportProvider provider = newProvider(policy, transports, sessions);
        CountDownLatch completed = new CountDownLatch(1);
        BaseSubscriber<ServerSentEvent<?>> subscriber = noDemandSubscriber(completed);
        provider.createSseFlux(request()).subscribe(subscriber);
        McpServerTransport transport = awaitTransport(transports);
        final String sessionId = awaitSession(sessions);

        transport.sendMessage(notification("slow")).block(SEND_TIMEOUT);
        awaitTrue(() -> provider.getTerminalCount(SseSessionState.TerminalReason.OUTPUT_STALLED) == 1,
                "pending output without progress must terminate the session");
        assertEquals(1, provider.getTerminalCount(SseSessionState.TerminalReason.OUTPUT_STALLED));
        assertEquals(0, provider.getTotalPendingEvents());
        awaitTrue(() -> !provider.hasSession(sessionId), "stalled session must be removed");

        subscriber.request(10);
        assertTrue(completed.await(AWAIT_TIMEOUT.toMillis(), TimeUnit.MILLISECONDS),
                "terminated stream must complete after the consumer drains its buffer");
    }

    @Test
    void stalledSessionDoesNotAffectAConcurrentHealthySession() throws Exception {
        SseSessionPolicy policy = SseSessionPolicy.builder()
                .maxPendingEvents(8)
                .maxPendingBytes(4096)
                .stallTimeout(Duration.ofMillis(150))
                .build();
        List<McpServerTransport> transports = new CopyOnWriteArrayList<>();
        ShenyuSseServerTransportProvider provider = newProvider(policy, transports, new CopyOnWriteArrayList<>());

        CountDownLatch stalled = new CountDownLatch(1);
        BaseSubscriber<ServerSentEvent<?>> stalledSubscriber = noDemandSubscriber(stalled);
        provider.createSseFlux(request()).subscribe(stalledSubscriber);

        LinkedBlockingQueue<ServerSentEvent<?>> healthyReceived = new LinkedBlockingQueue<>();
        provider.createSseFlux(request()).subscribe(healthyReceived::offer);

        McpServerTransport stalledTransport = awaitTransport(transports, 0);
        final McpServerTransport healthyTransport = awaitTransport(transports, 1);

        stalledTransport.sendMessage(notification("held")).block(SEND_TIMEOUT);
        awaitTrue(() -> provider.getTerminalCount(SseSessionState.TerminalReason.OUTPUT_STALLED) == 1,
                "stalled session must terminate");
        assertEquals(1, provider.getTerminalCount(SseSessionState.TerminalReason.OUTPUT_STALLED));

        healthyTransport.sendMessage(notification("h1")).block(SEND_TIMEOUT);
        healthyTransport.sendMessage(notification("h2")).block(SEND_TIMEOUT);

        assertNotNull(healthyReceived.poll(AWAIT_TIMEOUT.toSeconds(), TimeUnit.SECONDS));
        ServerSentEvent<?> first = healthyReceived.poll(AWAIT_TIMEOUT.toSeconds(), TimeUnit.SECONDS);
        assertNotNull(first);
        assertTrue(String.valueOf(first.data()).contains("h1"));
        ServerSentEvent<?> second = healthyReceived.poll(AWAIT_TIMEOUT.toSeconds(), TimeUnit.SECONDS);
        assertNotNull(second);
        assertTrue(String.valueOf(second.data()).contains("h2"));
    }

    @Test
    void clientDisconnectReleasesStateExactlyOnce() throws Exception {
        List<McpServerTransport> transports = new CopyOnWriteArrayList<>();
        List<McpServerSession> sessions = new CopyOnWriteArrayList<>();
        ShenyuSseServerTransportProvider provider =
                newProvider(SseSessionPolicy.defaults(), transports, sessions);
        BaseSubscriber<ServerSentEvent<?>> subscriber = noDemandSubscriber(new CountDownLatch(1));
        provider.createSseFlux(request()).subscribe(subscriber);
        McpServerTransport transport = awaitTransport(transports);
        final String sessionId = awaitSession(sessions);

        transport.sendMessage(notification("pending")).block(SEND_TIMEOUT);
        assertEquals(1, provider.getTotalPendingEvents());

        subscriber.dispose();

        awaitTrue(() -> !provider.hasSession(sessionId), "disconnected session must be removed");
        assertEquals(1, provider.getTerminalCount(SseSessionState.TerminalReason.CLIENT_DISCONNECTED));
        assertEquals(0, provider.getTerminalCount(SseSessionState.TerminalReason.OUTPUT_OVERFLOW));
        assertEquals(0, provider.getTerminalCount(SseSessionState.TerminalReason.OUTPUT_STALLED));
        assertEquals(0, provider.getTerminalCount(SseSessionState.TerminalReason.PROVIDER_CLOSED));
        assertEquals(0, provider.getTotalPendingEvents());
    }

    @Test
    void loopbackFastReaderReceivesEndpointThenOrderedMessages() throws Exception {
        List<McpServerTransport> transports = new CopyOnWriteArrayList<>();
        ShenyuSseServerTransportProvider provider =
                newProvider(SseSessionPolicy.defaults(), transports, new CopyOnWriteArrayList<>());
        LoopResources loops = LoopResources.create("mcp-sse-fast");
        DisposableServer server = loopbackServer(provider, loops);
        try {
            WebClient client = WebClient.create();
            Mono<List<String>> body = client.get()
                    .uri("http://127.0.0.1:" + server.port() + "/sse")
                    .exchangeToMono(response -> response.bodyToFlux(String.class).take(4).collectList());
            CountDownLatch done = new CountDownLatch(1);
            AtomicReference<List<String>> lines = new AtomicReference<>();
            body.subscribe(list -> {
                lines.set(list);
                done.countDown();
            });

            McpServerTransport transport = awaitTransport(transports);
            transport.sendMessage(notification("l1")).block(SEND_TIMEOUT);
            transport.sendMessage(notification("l2")).block(SEND_TIMEOUT);
            transport.sendMessage(notification("l3")).block(SEND_TIMEOUT);

            assertTrue(done.await(AWAIT_TIMEOUT.toMillis(), TimeUnit.MILLISECONDS), "SSE body must be consumed");
            List<String> received = lines.get();
            assertEquals(4, received.size(), "endpoint event plus three messages");
            assertTrue(received.get(0).contains("/message"), "first event must carry the message endpoint");
            for (int i = 0; i < 3; i++) {
                assertTrue(received.get(i + 1).contains("l" + (i + 1)), "messages must arrive in order");
            }
        } finally {
            server.disposeNow();
            loops.disposeLater().block(Duration.ofSeconds(5));
        }
    }

    @Test
    void loopbackThrottledReaderRecoversWithoutLoss() throws Exception {
        List<McpServerTransport> transports = new CopyOnWriteArrayList<>();
        SseSessionPolicy policy = SseSessionPolicy.builder()
                .maxPendingEvents(64)
                .maxPendingBytes(65536)
                .stallTimeout(Duration.ofSeconds(10))
                .build();
        ShenyuSseServerTransportProvider provider = newProvider(policy, transports, new CopyOnWriteArrayList<>());
        LoopResources loops = LoopResources.create("mcp-sse-slow");
        DisposableServer server = loopbackServer(provider, loops);
        try {
            WebClient client = WebClient.create();
            Mono<List<String>> body = client.get()
                    .uri("http://127.0.0.1:" + server.port() + "/sse")
                    .exchangeToMono(response -> response.bodyToFlux(String.class).limitRate(1).take(4).collectList());
            CountDownLatch done = new CountDownLatch(1);
            AtomicReference<List<String>> lines = new AtomicReference<>();
            body.subscribe(list -> {
                lines.set(list);
                done.countDown();
            });

            McpServerTransport transport = awaitTransport(transports);
            for (int i = 1; i <= 3; i++) {
                transport.sendMessage(notification("t" + i)).block(SEND_TIMEOUT);
            }

            assertTrue(done.await(AWAIT_TIMEOUT.toMillis(), TimeUnit.MILLISECONDS),
                    "throttled reader must still receive everything");
            List<String> received = lines.get();
            assertEquals(4, received.size(), "no loss under temporary pressure");
            for (int i = 0; i < 3; i++) {
                assertTrue(received.get(i + 1).contains("t" + (i + 1)), "order must be preserved");
            }
        } finally {
            server.disposeNow();
            loops.disposeLater().block(Duration.ofSeconds(5));
        }
    }

    @Test
    void loopbackDisconnectRemovesTheServerSession() throws Exception {
        List<McpServerTransport> transports = new CopyOnWriteArrayList<>();
        List<McpServerSession> sessions = new CopyOnWriteArrayList<>();
        ShenyuSseServerTransportProvider provider =
                newProvider(SseSessionPolicy.defaults(), transports, sessions);
        LoopResources loops = LoopResources.create("mcp-sse-cancel");
        DisposableServer server = loopbackServer(provider, loops);
        try {
            WebClient client = WebClient.create();
            // take(1) cancels the SSE connection right after the endpoint event
            client.get()
                    .uri("http://127.0.0.1:" + server.port() + "/sse")
                    .exchangeToMono(response -> response.bodyToFlux(String.class).take(1).then())
                    .block(SEND_TIMEOUT);

            String sessionId = awaitSession(sessions);
            awaitTrue(() -> !provider.hasSession(sessionId), "cancelled SSE connection must close its session");
            assertEquals(1, provider.getTerminalCount(SseSessionState.TerminalReason.CLIENT_DISCONNECTED));
        } finally {
            server.disposeNow();
            loops.disposeLater().block(Duration.ofSeconds(5));
        }
    }

    private ShenyuSseServerTransportProvider newProvider(final SseSessionPolicy policy,
                                                         final List<McpServerTransport> transports,
                                                         final List<McpServerSession> sessions) {
        ObjectMapperHolder holder = new ObjectMapperHolder();
        ShenyuSseServerTransportProvider provider = new ShenyuSseServerTransportProvider(
                holder.objectMapper, "", "/message", "/sse", policy);
        provider.setSessionFactory(transport -> {
            transports.add(transport);
            McpServerSession session = new McpServerSession(
                    UUID.randomUUID().toString(),
                    Duration.ofSeconds(60),
                    transport,
                    initializeRequest -> Mono.just(new McpSchema.InitializeResult(
                            "2025-03-26",
                            McpSchema.ServerCapabilities.builder().build(),
                            new McpSchema.Implementation("ShenyuMcpServer", "1.0.0"),
                            "test")),
                    Map.of(),
                    Map.of());
            sessions.add(session);
            return session;
        });
        return provider;
    }

    private DisposableServer loopbackServer(final ShenyuSseServerTransportProvider provider, final LoopResources loops) {
        RouterFunction<?> route = RouterFunctions.route(RequestPredicates.GET("/sse"), provider::handleSseConnection);
        return HttpServer.create()
                .host("127.0.0.1")
                .port(0)
                .runOn(loops)
                .handle(new ReactorHttpHandlerAdapter(RouterFunctions.toHttpHandler(route)))
                .bindNow();
    }

    private BaseSubscriber<ServerSentEvent<?>> noDemandSubscriber(final CountDownLatch completed) {
        BaseSubscriber<ServerSentEvent<?>> subscriber = new BaseSubscriber<ServerSentEvent<?>>() {
            @Override
            protected void hookOnSubscribe(final Subscription subscription) {
                // hold back all demand to simulate a client that stopped reading
            }

            @Override
            protected void hookOnComplete() {
                completed.countDown();
            }

            @Override
            protected void hookOnError(final Throwable throwable) {
                completed.countDown();
            }
        };
        subscriptions.add(subscriber);
        return subscriber;
    }

    private McpServerTransport awaitTransport(final List<McpServerTransport> transports) throws Exception {
        return awaitTransport(transports, 0);
    }

    private McpServerTransport awaitTransport(final List<McpServerTransport> transports, final int index) throws Exception {
        awaitTrue(() -> transports.size() > index, "session transport must be created");
        return transports.get(index);
    }

    private String awaitSession(final List<McpServerSession> sessions) throws Exception {
        awaitTrue(() -> !sessions.isEmpty(), "session must be created");
        return sessions.get(0).getId();
    }

    private static void awaitTrue(final BooleanSupplier condition, final String description) throws Exception {
        long deadline = System.currentTimeMillis() + AWAIT_TIMEOUT.toMillis();
        while (System.currentTimeMillis() < deadline) {
            if (condition.getAsBoolean()) {
                return;
            }
            Thread.sleep(20);
        }
        fail(description);
    }

    private McpSchema.JSONRPCMessage notification(final String method) {
        return new McpSchema.JSONRPCNotification("2.0", method, Map.of("trace", method));
    }

    private ServerRequest request() {
        return ServerRequest.create(
                MockServerWebExchange.from(MockServerHttpRequest.get("/sse").build()),
                HandlerStrategies.withDefaults().messageReaders());
    }

    /**
     * Shared object mapper plus the same JSON mapper the provider uses internally, so
     * tests can compute exact serialized UTF-8 lengths for boundary policies.
     */
    private static final class ObjectMapperHolder {

        private final com.fasterxml.jackson.databind.ObjectMapper objectMapper =
                new com.fasterxml.jackson.databind.ObjectMapper();

        private final JacksonMcpJsonMapper jsonMapper = new JacksonMcpJsonMapper(objectMapper);

        int lengthOf(final McpSchema.JSONRPCMessage message) {
            try {
                return jsonMapper.writeValueAsString(message).getBytes(StandardCharsets.UTF_8).length;
            } catch (IOException e) {
                throw new IllegalStateException(e);
            }
        }
    }
}
