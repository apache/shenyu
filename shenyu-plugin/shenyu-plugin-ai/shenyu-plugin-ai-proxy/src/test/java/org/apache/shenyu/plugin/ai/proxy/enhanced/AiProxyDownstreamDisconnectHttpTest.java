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

package org.apache.shenyu.plugin.ai.proxy.enhanced;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.AiProxyHandle;
import org.apache.shenyu.common.metrics.AgentRuntimeMetrics;
import org.apache.shenyu.common.metrics.AiAttemptEvent;
import org.apache.shenyu.common.metrics.AiCallObserver;
import org.apache.shenyu.common.metrics.AiCallOutcome;
import org.apache.shenyu.common.metrics.AiTerminalEvent;
import org.apache.shenyu.plugin.ai.proxy.enhanced.handler.AiProxyPluginHandler;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocolFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.OpenAiChat;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProviderFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.OpenAi;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyConfigService;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyEngine;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.WebClientAiProxyTransport;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.junit.jupiter.api.Test;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.server.reactive.ReactorHttpHandlerAdapter;
import org.springframework.web.reactive.function.client.WebClient;
import org.springframework.web.server.adapter.WebHttpHandlerBuilder;
import reactor.core.Disposable;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.netty.DisposableServer;
import reactor.netty.http.server.HttpServer;

import java.io.IOException;
import java.io.InputStream;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Real downstream disconnect classification across the AI proxy terminal boundary.
 *
 * <p>Every case runs a real WebClient client against a real WebFlux gateway whose response writer
 * proxies a real Reactor Netty SSE upstream, so a peer disconnect is a socket fact instead of a
 * mocked exchange signal.</p>
 */
class AiProxyDownstreamDisconnectHttpTest {

    private static final Duration TIMEOUT = Duration.ofSeconds(10);

    private static final long POLL_MILLIS = 20L;

    private static final int STABLE_POLLS = 4;

    private static final String FIRST_CHUNK = "data: {\"choices\":[{\"delta\":{\"content\":\"hello\"}}]}\n\n";

    private static final String TERMINAL_CHUNK = "data: [DONE]\n\n";

    private static final String REQUEST_BODY = "{\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}],\"stream\":true}";

    @Test
    void clientDisconnectBeforeFirstChunkIsReportedAsCancelled() {
        try (Probe probe = new Probe(Flux.never())) {
            probe.disconnectBeforeFirstChunk();
            assertInterrupted(probe);
        }
    }

    @Test
    void clientDisconnectAfterFirstChunkIsReportedAsCancelled() {
        try (Probe probe = new Probe(Flux.just(FIRST_CHUNK).concatWith(Flux.never()))) {
            probe.disconnectAfterFirstChunk();
            assertInterrupted(probe);
        }
    }

    @Test
    void terminalMarkerStreamStaysSuccessful() {
        try (Probe probe = new Probe(Flux.just(FIRST_CHUNK, TERMINAL_CHUNK))) {
            probe.consumeFully();
            assertSingleObservation(probe);
            assertEquals(AiCallOutcome.SUCCESS, probe.terminal().outcome());
            assertEquals("UPSTREAM_MARKER", probe.terminal().completion().evidence().name());
            assertTrue(probe.metricOutcomes().stream().map(Enum::name).anyMatch("SUCCESS"::equals),
                    "runtime metrics must observe the successful outcome");
            assertFalse(probe.metricOutcomes().stream().map(Enum::name).anyMatch("CANCELLED"::equals),
                    "a protocol terminal marker must not be reported as cancelled");
        }
    }

    @Test
    void cleanEofWithoutTerminalMarkerStaysSuccessful() {
        try (Probe probe = new Probe(Flux.just(FIRST_CHUNK))) {
            probe.consumeFully();
            assertSingleObservation(probe);
            assertEquals(AiCallOutcome.SUCCESS, probe.terminal().outcome());
            assertFalse("SOURCE_CANCELLED".equals(probe.terminal().completion().evidence().name()),
                    "a clean EOF must not be conflated with a cancellation");
            assertFalse(probe.metricOutcomes().stream().map(Enum::name).anyMatch("CANCELLED"::equals),
                    "a clean EOF must not be reported as cancelled");
        }
    }

    @Test
    void clientAbortInTheMiddleOfTheStreamIsNeverReportedAsSuccess() {
        try (Probe probe = new Probe(Flux.just(FIRST_CHUNK)
                .concatWith(Flux.interval(Duration.ofMillis(20L)).map(index -> FIRST_CHUNK)))) {
            probe.abortMidStream();
            assertNotSuccess(probe);
        }
    }

    @Test
    void writeFailureAfterCommitKeepsTheStrongEvidenceAndIsAnError() {
        try (Probe probe = new Probe(Flux.just(FIRST_CHUNK)
                .concatWith(Flux.error(new IllegalStateException("private upstream text"))))) {
            probe.consumeToleratingFailure();
            assertEquals(AiCallOutcome.ERROR, probe.terminal().outcome(), "a failed write is not a success");
            assertEquals(AiCallOutcome.ERROR, probe.attempts().get(0).outcome(), "attempt outcome");
            assertEquals("TRANSPORT_FAILURE", probe.attempts().get(0).completion().evidence().name(),
                    "truncation and transport failure evidence must survive the cancellation path");
            assertEquals(1, probe.upstreamCalls(), "a failed write must not start another attempt");
            assertFalse(probe.metricOutcomes().stream().map(Enum::name).anyMatch("SUCCESS"::equals),
                    "a failed write must not increment the success outcome");
        }
    }

    private static void assertNotSuccess(final Probe probe) {
        assertSingleObservation(probe);
        assertNotEquals(AiCallOutcome.SUCCESS, probe.terminal().outcome(), "a broken write is never a success");
        assertNotEquals(AiCallOutcome.SUCCESS, probe.attempts().get(0).outcome(), "attempt outcome");
        assertEquals(1, probe.upstreamCalls(), "a broken write must not start another attempt or fallback");
        assertFalse(probe.metricOutcomes().stream().map(Enum::name).anyMatch("SUCCESS"::equals),
                "a broken write must not increment the success outcome");
    }

    private static void assertInterrupted(final Probe probe) {
        assertSingleObservation(probe);
        AiTerminalEvent terminal = probe.terminal();
        assertNotNull(terminal, "no terminal event recorded");
        assertEquals(AiCallOutcome.CANCELLED, terminal.outcome(), "logical outcome");
        assertEquals("SOURCE_CANCELLED", terminal.completion().evidence().name(),
                "the interrupted stream must keep the cancelled evidence");
        assertEquals("SOURCE_CANCELLED", probe.attempts().get(0).completion().evidence().name(),
                "attempt evidence");
        assertEquals(Integer.valueOf(200), terminal.httpStatus(), "the assigned HTTP status stays a separate fact");
        assertEquals("UNKNOWN", terminal.usage().status().name(), "usage must stay unknown");
        assertEquals(1, probe.attempts().size(), "one event per real attempt");
        assertEquals(AiCallOutcome.CANCELLED, probe.attempts().get(0).outcome(), "attempt outcome");
        assertEquals(1, probe.upstreamCalls(), "cancellation must not start another attempt or fallback");
        assertTrue(probe.awaitUpstreamCancelled(), "the original upstream must be cancelled");
        assertTrue(probe.metricOutcomes().stream().map(Enum::name).anyMatch("CANCELLED"::equals),
                "runtime metrics must observe CANCELLED");
        assertFalse(probe.metricOutcomes().stream().map(Enum::name).anyMatch("SUCCESS"::equals),
                "an interrupted request must not increment the success outcome");
    }

    private static void assertSingleObservation(final Probe probe) {
        probe.awaitTerminal();
        probe.awaitStableEvents();
        assertEquals(1, probe.terminals().size(), "exactly one terminal event");
        assertEquals(1, probe.attempts().size(), "one event per real attempt");
    }

    /**
     * Loopback fixture with a real upstream, a real gateway and a real downstream client.
     */
    private static final class Probe implements AutoCloseable {

        private static final AtomicInteger SEQUENCE = new AtomicInteger();

        private final String selectorId;

        private final DisposableServer upstream;

        private final DisposableServer gateway;

        private final List<AiTerminalEvent> terminals = new CopyOnWriteArrayList<>();

        private final List<AiAttemptEvent> attempts = new CopyOnWriteArrayList<>();

        private final List<AgentRuntimeMetrics.Outcome> metricOutcomes = new CopyOnWriteArrayList<>();

        private final AtomicInteger upstreamCalls = new AtomicInteger();

        private final CountDownLatch upstreamStarted = new CountDownLatch(1);

        private final CountDownLatch upstreamCancelled = new CountDownLatch(1);

        private final CountDownLatch terminalRecorded = new CountDownLatch(1);

        private volatile Disposable client;

        Probe(final Flux<String> upstreamBody) {
            this.selectorId = "downstream-disconnect-" + SEQUENCE.incrementAndGet();
            this.upstream = HttpServer.create().host("127.0.0.1").port(0)
                    .handle((request, response) -> {
                        upstreamCalls.incrementAndGet();
                        return response.header("Content-Type", "text/event-stream")
                                .sendString(upstreamBody
                                        .doOnSubscribe(ignored -> upstreamStarted.countDown())
                                        .doOnCancel(upstreamCancelled::countDown));
                    })
                    .bindNow();
            final AiCallObserver observer = new AiCallObserver() {
                @Override
                public void record(final AiTerminalEvent event) {
                    terminals.add(event);
                    terminalRecorded.countDown();
                }

                @Override
                public void recordAttempt(final AiAttemptEvent event) {
                    attempts.add(event);
                }
            };
            final AgentRuntimeMetrics metrics = new AgentRuntimeMetrics() {
                @Override
                public void finished(final Traffic traffic, final Outcome outcome, final Reason reason, final long elapsedMillis) {
                    metricOutcomes.add(outcome);
                }
            };
            AiProxyHandle handle = new AiProxyHandle();
            handle.setProvider("OpenAI");
            handle.setBaseUrl("http://127.0.0.1:" + upstream.port());
            handle.setApiKey("fixture-key");
            handle.setStream(true);
            handle.setModel("fixture-model");
            AiProxyPluginHandler handler = new AiProxyPluginHandler();
            handler.getSelectorCachedHandle().cachedHandle(CacheKeyUtils.INST.getKey(selectorId, Constants.DEFAULT_RULE), handle);
            AiProxyEngine engine = new AiProxyEngine(new AiProxyProtocolFactory(List.of(new OpenAiChat())),
                    new AiProxyProviderFactory(List.of(new OpenAi())),
                    new WebClientAiProxyTransport(WebClient.builder().build()), Duration.ZERO, observer);
            AiProxyPlugin plugin = new AiProxyPlugin(new AiProxyConfigService(handler), engine, handler, observer);
            SelectorData selector = new SelectorData();
            selector.setId(selectorId);
            ReactorHttpHandlerAdapter adapter = new ReactorHttpHandlerAdapter(WebHttpHandlerBuilder.webHandler(exchange -> {
                exchange.getAttributes().put(AgentRuntimeMetrics.KEY, metrics);
                return plugin.doExecute(exchange, ignored -> Mono.empty(), selector, new RuleData());
            }).build());
            final DisposableServer bound;
            try {
                bound = HttpServer.create().host("127.0.0.1").port(0).handle(adapter).bindNow();
            } catch (RuntimeException error) {
                this.upstream.disposeNow();
                throw error;
            }
            this.gateway = bound;
        }

        void disconnectBeforeFirstChunk() {
            this.client = incoming().doOnNext(DataBufferUtils::release).subscribe();
            assertTrue(await(upstreamStarted), "the upstream stream was never subscribed");
            this.client.dispose();
        }

        void disconnectAfterFirstChunk() {
            incoming().doOnNext(DataBufferUtils::release).take(1).then().block(TIMEOUT);
        }

        void consumeFully() {
            incoming().doOnNext(DataBufferUtils::release).then().block(TIMEOUT);
        }

        void consumeToleratingFailure() {
            incoming().doOnNext(DataBufferUtils::release).onErrorResume(ignored -> Flux.empty()).then().block(TIMEOUT);
        }

        void abortMidStream() {
            try (Socket socket = new Socket()) {
                socket.connect(new InetSocketAddress("127.0.0.1", this.gateway.port()), (int) TIMEOUT.toMillis());
                byte[] request = ("POST /chat HTTP/1.1\r\nHost: 127.0.0.1\r\nContent-Type: application/json\r\n"
                        + "Content-Length: " + REQUEST_BODY.length() + "\r\n\r\n" + REQUEST_BODY)
                        .getBytes(StandardCharsets.US_ASCII);
                socket.getOutputStream().write(request);
                socket.getOutputStream().flush();
                byte[] head = new byte[64];
                InputStream input = socket.getInputStream();
                assertTrue(input.read(head) > 0, "the response was never started");
                assertTrue(await(this.upstreamStarted), "the upstream stream was never subscribed");
                socket.setSoLinger(true, 0);
            } catch (IOException error) {
                throw new IllegalStateException("could not abort the downstream connection", error);
            }
        }

        private Flux<DataBuffer> incoming() {
            return WebClient.create().post().uri("http://127.0.0.1:" + gateway.port() + "/chat")
                    .header("Content-Type", "application/json")
                    .bodyValue(REQUEST_BODY)
                    .retrieve().bodyToFlux(DataBuffer.class);
        }

        void awaitTerminal() {
            assertTrue(await(terminalRecorded), "no terminal observation was recorded");
        }

        boolean awaitUpstreamCancelled() {
            return await(upstreamCancelled);
        }

        void awaitStableEvents() {
            final long deadline = System.nanoTime() + TIMEOUT.toNanos();
            int previous = -1;
            int stable = 0;
            while (System.nanoTime() < deadline) {
                final int current = this.terminals.size() + this.attempts.size();
                stable = current == previous ? stable + 1 : 0;
                previous = current;
                if (stable >= STABLE_POLLS) {
                    return;
                }
                try {
                    Thread.sleep(POLL_MILLIS);
                } catch (InterruptedException error) {
                    Thread.currentThread().interrupt();
                    throw new IllegalStateException("interrupted while waiting for quiescence", error);
                }
            }
            throw new IllegalStateException("the observation did not settle within " + TIMEOUT);
        }

        private static boolean await(final CountDownLatch latch) {
            try {
                return latch.await(TIMEOUT.getSeconds(), TimeUnit.SECONDS);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                return false;
            }
        }

        int upstreamCalls() {
            return upstreamCalls.get();
        }

        AiTerminalEvent terminal() {
            return terminals.isEmpty() ? null : terminals.get(0);
        }

        List<AiTerminalEvent> terminals() {
            return terminals;
        }

        List<AiAttemptEvent> attempts() {
            return attempts;
        }

        List<AgentRuntimeMetrics.Outcome> metricOutcomes() {
            return metricOutcomes;
        }

        @Override
        public void close() {
            if (Objects.nonNull(this.client)) {
                this.client.dispose();
            }
            gateway.disposeNow();
            upstream.disposeNow();
        }
    }
}
