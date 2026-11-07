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

package org.apache.shenyu.plugin.httpclient;

import com.sun.net.httpserver.HttpServer;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.base.utils.UpstreamAttemptObservation;
import org.apache.shenyu.plugin.base.utils.UpstreamAttemptObserver;
import org.apache.shenyu.plugin.base.utils.UpstreamAttemptObserverHolder;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.netty.http.client.HttpClient;
import reactor.test.StepVerifier;

import java.io.IOException;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Verifies the optional upstream-attempt observation adapter against real local HTTP
 * servers, for both the Netty and the WebClient transports (#8209): one observation
 * per actual transport attempt, headers-ready distinct from the terminal outcome, and
 * zero events when the observer is absent.
 */
class UpstreamAttemptObservationTest {

    private static final String SELECTOR_ID = "selector-8209";

    private final RecordingObserver observer = new RecordingObserver();

    private HttpServer server;

    private int port;

    private ServerWebExchange exchange;

    private ShenyuPluginChain chain;

    @BeforeEach
    void setUp() throws IOException {
        server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        server.start();
        port = server.getAddress().getPort();
        UpstreamAttemptObserverHolder.register(observer);
        exchange = MockServerWebExchange.from(MockServerHttpRequest.get("http://localhost/test").build());
        exchange.getAttributes().put(Constants.CONTEXT, new ShenyuContext());
        exchange.getAttributes().put(Constants.DIVIDE_SELECTOR_ID, SELECTOR_ID);
        exchange.getAttributes().put(Constants.HTTP_TIME_OUT, 5000L);
        exchange.getAttributes().put(Constants.LOAD_BALANCE, "random");
        chain = org.mockito.Mockito.mock(ShenyuPluginChain.class);
        org.mockito.Mockito.when(chain.execute(exchange)).thenReturn(Mono.empty());
        final org.springframework.context.ConfigurableApplicationContext applicationContext =
                org.mockito.Mockito.mock(org.springframework.context.ConfigurableApplicationContext.class);
        org.mockito.Mockito.when(applicationContext.getBean(org.apache.shenyu.plugin.api.result.ShenyuResult.class))
                .thenReturn(new org.apache.shenyu.plugin.api.result.DefaultShenyuResult());
        org.apache.shenyu.plugin.api.utils.SpringBeanUtils.getInstance().setApplicationContext(applicationContext);
    }

    @AfterEach
    void tearDown() {
        // back to the no-op observer: the transport path allocates nothing without one
        UpstreamAttemptObserverHolder.unregister();
        server.stop(0);
    }

    @Test
    void nettySuccessReportsStartedHeadersAndCompletedExactlyOnce() {
        serve(200, "ok");
        pointHttpUriAtServer();
        StepVerifier.create(nettyPlugin().execute(exchange, chain)).verifyComplete();

        assertEventsExactly("started:127.0.0.1:" + port, "headers:200", "finished:COMPLETED:200");
        final UpstreamAttemptObservation observation = observer.observations.get(0);
        assertEquals("GET", observation.getMethod());
        assertEquals(Integer.valueOf(200), observation.getHeadersStatus());
        assertTrue(observation.isFinished());
    }

    @Test
    void webClientSuccessReportsTheSameFacts() {
        serve(200, "ok");
        pointHttpUriAtServer();
        StepVerifier.create(webClientPlugin().execute(exchange, chain)).verifyComplete();

        assertEventsExactly("started:127.0.0.1:" + port, "headers:200", "finished:COMPLETED:200");
    }

    @Test
    void connectionFailureFinishesWithErrorWithoutHeaders() {
        // nothing is served on this port
        exchange.getAttributes().put(Constants.HTTP_URI, java.net.URI.create("http://127.0.0.1:1/test"));
        StepVerifier.create(nettyPlugin().execute(exchange, chain)).verifyError();

        assertEventsExactly("started:127.0.0.1:1", "finished:ERROR:null");
    }

    @Test
    void delayedBodyDoesNotFinishTheObservationEarly() {
        serveChunkedDelayed(400);
        pointHttpUriAtServer();
        // the response writer consumes the body inside the downstream chain; the
        // observation may only finish when that consumption is done (#8209)
        org.mockito.Mockito.when(chain.execute(exchange)).thenReturn(Mono.defer(() -> {
            final Object connection = exchange.getAttribute(Constants.CLIENT_RESPONSE_CONN_ATTR);
            return Objects.isNull(connection) ? Mono.empty()
                    : ((reactor.netty.Connection) connection).inbound().receive()
                            .aggregate().asString().then();
        }));
        StepVerifier.create(nettyPlugin().execute(exchange, chain)).verifyComplete();

        assertEventsExactly("started:127.0.0.1:" + port, "headers:200", "finished:COMPLETED:200");
    }

    @Test
    void downstreamCancellationFinishesTheObservationAsCancelled() throws Exception {
        serveHangingBody();
        pointHttpUriAtServer();
        org.mockito.Mockito.when(chain.execute(exchange)).thenReturn(Mono.defer(() -> {
            final Object connection = exchange.getAttribute(Constants.CLIENT_RESPONSE_CONN_ATTR);
            return Objects.isNull(connection) ? Mono.empty()
                    : ((reactor.netty.Connection) connection).inbound().receive()
                            .aggregate().asString().then();
        }));
        StepVerifier.create(nettyPlugin().execute(exchange, chain))
                .thenCancel()
                .verify();

        final UpstreamAttemptObservation finished = awaitFinished();
        assertNotNull(finished, "the observation must finish on cancellation");
        assertEquals(UpstreamAttemptObservation.Outcome.CANCELLED, finished.getOutcome(),
                "a downstream cancellation must finish the serving attempt as cancelled");
    }

    @Test
    void localRejectionProducesNoObservation() {
        exchange.getAttributes().remove(Constants.HTTP_URI);
        StepVerifier.create(nettyPlugin().execute(exchange, chain)).verifyComplete();

        assertTrue(observer.events.isEmpty(),
                "a selection or validation rejection before transport subscription is not an upstream call");
    }

    @Test
    void rejectedStatusWithNoRetriesIsAFailedAttemptNotASuccess() {
        serve(500, "nope");
        pointHttpUriAtServer();
        exchange.getAttributes().put(Constants.HTTP_RETRY, 0);
        StepVerifier.create(nettyPlugin().execute(exchange, chain))
                .verifyComplete();

        assertEventsExactly("started:127.0.0.1:" + port, "headers:500", "finished:ERROR:500");
    }

    @Test
    void throwingObserverDoesNotFailTheRequest() {
        serve(200, "ok");
        pointHttpUriAtServer();
        UpstreamAttemptObserverHolder.register(new UpstreamAttemptObserver() {
            @Override
            public void onAttemptStarted(final UpstreamAttemptObservation o) {
                throw new IllegalStateException("observer boom");
            }

            @Override
            public void onHeadersReady(final UpstreamAttemptObservation o) {
                throw new IllegalStateException("observer boom");
            }

            @Override
            public void onAttemptFinished(final UpstreamAttemptObservation o) {
                throw new IllegalStateException("observer boom");
            }
        });
        StepVerifier.create(nettyPlugin().execute(exchange, chain)).verifyComplete();
    }

    @Test
    void withoutAnObserverNothingIsReportedAndTheShortCircuitHolds() {
        UpstreamAttemptObserverHolder.unregister();
        serve(200, "ok");
        pointHttpUriAtServer();
        StepVerifier.create(nettyPlugin().execute(exchange, chain)).verifyComplete();

        assertFalse(UpstreamAttemptObserverHolder.isInstrumented());
        assertTrue(observer.events.isEmpty(), "the unregistered observer must see nothing");
    }

    @Test
    void concurrentRequestsEachFinishExactlyOnce() {
        serve(200, "ok");
        org.mockito.Mockito.when(chain.execute(org.mockito.ArgumentMatchers.any())).thenReturn(Mono.empty());
        IntStream.range(0, 8).parallel().forEach(i -> {
            final ServerWebExchange perRequest = MockServerWebExchange.from(
                    MockServerHttpRequest.get("http://localhost/test").build());
            perRequest.getAttributes().put(Constants.CONTEXT, new ShenyuContext());
            perRequest.getAttributes().put(Constants.HTTP_URI, java.net.URI.create("http://127.0.0.1:" + port + "/test"));
            perRequest.getAttributes().put(Constants.DIVIDE_SELECTOR_ID, SELECTOR_ID);
            perRequest.getAttributes().put(Constants.HTTP_TIME_OUT, 5000L);
            StepVerifier.create(nettyPlugin().execute(perRequest, chain)).verifyComplete();
        });
        assertEquals(8, observer.observations.size());
        assertEquals(8, observer.observations.stream().filter(UpstreamAttemptObservation::isFinished).count(),
                "state belongs to the request, not to shared mutable fields");
        assertEquals(8, observer.observations.stream().map(UpstreamAttemptObservation::getAttemptId).distinct().count());
    }

    private void pointHttpUriAtServer() {
        exchange.getAttributes().put(Constants.HTTP_URI, java.net.URI.create("http://127.0.0.1:" + port + "/test"));
    }

    private UpstreamAttemptObservation awaitFinished() throws Exception {
        final long deadline = System.currentTimeMillis() + 5000;
        while (System.currentTimeMillis() < deadline) {
            final UpstreamAttemptObservation done = observer.observations.stream()
                    .filter(UpstreamAttemptObservation::isFinished).findFirst().orElse(null);
            if (Objects.nonNull(done)) {
                return done;
            }
            Thread.sleep(50);
        }
        return null;
    }

    private void serve(final int status, final String body) {
        server.createContext("/test", exchange -> {
            final byte[] bytes = body.getBytes();
            exchange.getResponseHeaders().add("Content-Type", "text/plain");
            exchange.sendResponseHeaders(status, body.isEmpty() ? -1 : bytes.length);
            try (OutputStream out = exchange.getResponseBody()) {
                out.write(bytes);
            }
        });
    }

    private void serveChunkedDelayed(final long delayMillis) {
        server.createContext("/test", exchange -> {
            exchange.getResponseHeaders().add("Content-Type", "text/plain");
            exchange.sendResponseHeaders(200, 0);
            exchange.getResponseBody().write("first-".getBytes());
            exchange.getResponseBody().flush();
            try {
                Thread.sleep(delayMillis);
            } catch (InterruptedException ignored) {
                Thread.currentThread().interrupt();
            }
            exchange.getResponseBody().write("done".getBytes());
            exchange.getResponseBody().close();
        });
    }

    private void serveHangingBody() {
        final CountDownLatch never = new CountDownLatch(1);
        server.createContext("/test", exchange -> {
            exchange.getResponseHeaders().add("Content-Type", "text/plain");
            exchange.sendResponseHeaders(200, 0);
            exchange.getResponseBody().write("partial-".getBytes());
            exchange.getResponseBody().flush();
            try {
                never.await(10, TimeUnit.SECONDS);
            } catch (InterruptedException ignored) {
                Thread.currentThread().interrupt();
            }
        });
    }

    private int startSecondServer(final int status, final String body) {
        try {
            final HttpServer second = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
            second.createContext("/test", exchange -> {
                final byte[] bytes = body.getBytes();
                exchange.getResponseHeaders().add("Content-Type", "text/plain");
                exchange.sendResponseHeaders(status, bytes.length);
                try (OutputStream out = exchange.getResponseBody()) {
                    out.write(bytes);
                }
            });
            second.start();
            return second.getAddress().getPort();
        } catch (IOException e) {
            throw new IllegalStateException(e);
        }
    }

    private NettyHttpClientPlugin nettyPlugin() {
        return new NettyHttpClientPlugin(HttpClient.create(), 1024 * 1024);
    }

    private WebClientPlugin webClientPlugin() {
        return new WebClientPlugin(org.springframework.web.reactive.function.client.WebClient.builder()
                .baseUrl("http://127.0.0.1:" + port).build(), 1024 * 1024);
    }

    /**
     * Asserts the exact event sequence, normalizing the list implementation.
     */
    private void assertEventsExactly(final String... expected) {
        // doFinally runs after the terminal signal reaches the downstream subscriber, so the
        // terminal event can land a moment after verify returns: wait for it before asserting
        final long deadline = System.currentTimeMillis() + 5000;
        while (System.currentTimeMillis() < deadline
                && observer.events.stream().noneMatch(e -> e.startsWith("finished:"))) {
            try {
                Thread.sleep(20);
            } catch (InterruptedException ignored) {
                Thread.currentThread().interrupt();
                break;
            }
        }
        assertEquals(java.util.Arrays.asList(expected), new java.util.ArrayList<>(observer.events));
    }

    /**
     * Collects the observed facts in delivery order.
     */
    private static final class RecordingObserver implements UpstreamAttemptObserver {

        private final List<String> events = new CopyOnWriteArrayList<>();

        private final List<UpstreamAttemptObservation> observations = new CopyOnWriteArrayList<>();

        List<String> events() {
            return events;
        }

        List<UpstreamAttemptObservation> observations() {
            return observations;
        }

        @Override
        public void onAttemptStarted(final UpstreamAttemptObservation observation) {
            events.add("started:" + observation.getUrl());
            observations.add(observation);
        }

        @Override
        public void onHeadersReady(final UpstreamAttemptObservation observation) {
            events.add("headers:" + observation.getHeadersStatus());
        }

        @Override
        public void onAttemptFinished(final UpstreamAttemptObservation observation) {
            events.add("finished:" + observation.getOutcome() + ":" + observation.getHeadersStatus());
        }
    }
}
