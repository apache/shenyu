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

package org.apache.shenyu.plugin.ai.proxy.enhanced.transport;

import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.client.reactive.ReactorClientHttpConnector;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.netty.Connection;
import reactor.netty.DisposableServer;
import reactor.netty.NettyPipeline;
import reactor.netty.http.client.HttpClient;
import reactor.netty.http.server.HttpServer;
import reactor.netty.http.server.HttpServerResponse;
import reactor.netty.resources.ConnectionProvider;
import reactor.test.StepVerifier;

import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class WebClientAiProxyTransportIntegrationTest {

    private DisposableServer server;

    private ConnectionProvider connectionProvider;

    @AfterEach
    void tearDown() {
        if (Objects.nonNull(server)) {
            server.disposeNow();
        }
        if (Objects.nonNull(connectionProvider)) {
            connectionProvider.disposeLater().block(Duration.ofSeconds(5));
        }
    }

    @Test
    void testForwardsRequestAndPreservesResponseMetadataOverNetwork() {
        final AtomicReference<String> authorization = new AtomicReference<>();
        final AtomicReference<String> requestBody = new AtomicReference<>();
        server = HttpServer.create()
                .host("127.0.0.1")
                .port(0)
                .handle((request, response) -> request.receive().aggregate().asString()
                        .flatMap(body -> {
                            authorization.set(request.requestHeaders().get(HttpHeaders.AUTHORIZATION));
                            requestBody.set(body);
                            response.status(201)
                                    .addHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                                    .addHeader("X-Upstream", "first")
                                    .addHeader("X-Upstream", "second");
                            return response.sendString(Mono.just("{\"result\":\"ok\"}")).then();
                        }))
                .bindNow();

        final AiUpstreamResponse upstreamResponse = transport(defaultProperties()).execute(request()).block();

        assertEquals(201, upstreamResponse.getStatusCode());
        assertEquals(List.of(MediaType.APPLICATION_JSON_VALUE),
                upstreamResponse.getHeaders().get(HttpHeaders.CONTENT_TYPE));
        assertEquals(List.of("first", "second"), upstreamResponse.getHeaders().get("X-Upstream"));
        assertEquals("Bearer integration-key", authorization.get());
        assertEquals("{}", requestBody.get());
        StepVerifier.create(asStrings(upstreamResponse))
                .expectNext("{\"result\":\"ok\"}")
                .verifyComplete();
    }

    @Test
    void testPreservesUpstreamErrorResponseOverNetwork() {
        server = HttpServer.create()
                .host("127.0.0.1")
                .port(0)
                .handle((request, response) -> response.status(429)
                        .addHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                        .addHeader("Retry-After", "3")
                        .sendString(Mono.just("{\"error\":\"rate limited\"}")))
                .bindNow();

        final AiUpstreamResponse upstreamResponse = transport(defaultProperties()).execute(request()).block();

        assertEquals(429, upstreamResponse.getStatusCode());
        assertEquals(List.of("3"), upstreamResponse.getHeaders().get("Retry-After"));
        StepVerifier.create(asStrings(upstreamResponse))
                .expectNext("{\"error\":\"rate limited\"}")
                .verifyComplete();
    }

    @Test
    void testStreamsFirstChunkWithoutWaitingForResponseCompletion() {
        server = HttpServer.create()
                .host("127.0.0.1")
                .port(0)
                .handle((request, response) -> response.chunkedTransfer(true)
                        .sendString(Flux.concat(
                                Mono.just("first"),
                                Mono.delay(Duration.ofMillis(300)).thenReturn("second"))))
                .bindNow();

        final AiUpstreamResponse upstreamResponse = transport(defaultProperties()).execute(request()).block();

        StepVerifier.create(asStrings(upstreamResponse))
                .expectNext("first")
                .expectNoEvent(Duration.ofMillis(150))
                .expectNext("second")
                .verifyComplete();
    }

    @Test
    void testClassifiesRealResponseHeaderTimeout() {
        server = HttpServer.create()
                .host("127.0.0.1")
                .port(0)
                .handle((request, response) -> Mono.delay(Duration.ofMillis(500))
                        .flatMap(ignored -> response.sendString(Mono.just("late")).then()))
                .bindNow();
        final AiProxyTransportProperties properties = defaultProperties();
        properties.setResponseHeaderTimeout(Duration.ofMillis(100));

        StepVerifier.create(transport(properties).execute(request()))
                .expectErrorSatisfies(error -> assertKind(error,
                        AiTransportException.Kind.RESPONSE_HEADER_TIMEOUT))
                .verify();
    }

    @Test
    void testClassifiesConnectionClosedBeforeResponseHeaders() {
        server = HttpServer.create()
                .host("127.0.0.1")
                .port(0)
                .handle((request, response) -> {
                    response.withConnection(Connection::dispose);
                    return Mono.never();
                })
                .bindNow();

        StepVerifier.create(transport(defaultProperties()).execute(request()))
                .expectErrorSatisfies(error -> assertKind(error,
                        AiTransportException.Kind.CONNECTION_RESET))
                .verify();
    }

    @Test
    void testClassifiesConnectionClosedDuringStreamingBody() {
        server = HttpServer.create()
                .host("127.0.0.1")
                .port(0)
                .handle((request, response) -> response.chunkedTransfer(true)
                        .sendString(Flux.concat(
                                Mono.just("first"),
                                Mono.delay(Duration.ofMillis(100)).flatMap(ignored -> {
                                    response.withConnection(Connection::dispose);
                                    return Mono.never();
                                }))))
                .bindNow();
        final AiUpstreamResponse upstreamResponse = transport(defaultProperties()).execute(request()).block();

        StepVerifier.create(asStrings(upstreamResponse))
                .expectNext("first")
                .expectErrorSatisfies(error -> assertKind(error,
                        AiTransportException.Kind.CONNECTION_RESET))
                .verify();
    }

    @Test
    void testClassifiesRealStreamIdleTimeoutAndCancelsUpstream() throws InterruptedException {
        final CountDownLatch upstreamCancelled = new CountDownLatch(1);
        server = HttpServer.create()
                .host("127.0.0.1")
                .port(0)
                .handle((request, response) -> response.chunkedTransfer(true)
                        .sendString(Flux.concat(
                                        Mono.just("first"),
                                        Mono.delay(Duration.ofSeconds(5)).thenReturn("late"))
                                .doOnCancel(upstreamCancelled::countDown)))
                .bindNow();
        final AiProxyTransportProperties properties = defaultProperties();
        properties.setResponseHeaderTimeout(Duration.ofMillis(50));
        properties.setStreamingIdleTimeout(Duration.ofMillis(100));

        StepVerifier.create(transport(properties).execute(request())
                        .flatMapMany(AiUpstreamResponse::getBody)
                        .map(bytes -> new String(bytes, StandardCharsets.UTF_8)))
                .expectNext("first")
                .expectErrorSatisfies(error -> assertKind(error,
                        AiTransportException.Kind.STREAM_IDLE_TIMEOUT))
                .verify();
        assertTrue(upstreamCancelled.await(5, TimeUnit.SECONDS));
    }

    @Test
    void testRejectsRealOversizedContentLength() {
        server = HttpServer.create()
                .host("127.0.0.1")
                .port(0)
                .handle((request, response) -> response.addHeader(HttpHeaders.CONTENT_LENGTH, "6")
                        .sendString(Mono.just("abcdef")))
                .bindNow();
        final AiProxyTransportProperties properties = defaultProperties();
        properties.setMaxResponseBytes(5);

        StepVerifier.create(transport(properties).execute(request()))
                .expectErrorSatisfies(error -> assertKind(error,
                        AiTransportException.Kind.RESPONSE_TOO_LARGE))
                .verify();
    }

    @Test
    void testRejectsRealChunkedResponseIncrementally() {
        server = HttpServer.create()
                .host("127.0.0.1")
                .port(0)
                .handle((request, response) -> response.chunkedTransfer(true)
                        .sendString(Flux.concat(
                                Mono.just("abc"),
                                Mono.delay(Duration.ofMillis(100)).thenReturn("def"))))
                .bindNow();
        final AiProxyTransportProperties properties = defaultProperties();
        properties.setMaxResponseBytes(5);

        StepVerifier.create(transport(properties).execute(request())
                        .flatMapMany(AiUpstreamResponse::getBody)
                        .map(bytes -> new String(bytes, StandardCharsets.UTF_8)))
                .expectNext("abc")
                .expectErrorSatisfies(error -> assertKind(error,
                        AiTransportException.Kind.RESPONSE_TOO_LARGE))
                .verify();
    }

    @Test
    void testDownstreamCancellationCancelsRealUpstream() throws InterruptedException {
        final AtomicBoolean emittedFirstChunk = new AtomicBoolean();
        final CountDownLatch upstreamCancelled = new CountDownLatch(1);
        server = HttpServer.create()
                .host("127.0.0.1")
                .port(0)
                .handle((request, response) -> response.chunkedTransfer(true)
                        .sendString(Flux.interval(Duration.ofMillis(25))
                                .map(index -> {
                                    emittedFirstChunk.set(true);
                                    return "chunk-" + index;
                                })
                                .doOnCancel(upstreamCancelled::countDown)))
                .bindNow();

        StepVerifier.create(transport(defaultProperties()).execute(request())
                        .flatMapMany(AiUpstreamResponse::getBody))
                .expectNextCount(1)
                .thenCancel()
                .verify();
        assertTrue(emittedFirstChunk.get());
        assertTrue(upstreamCancelled.await(5, TimeUnit.SECONDS));
    }

    @Test
    void testReusesConnectionAfterResponseIsFullyConsumed() {
        final AtomicInteger requestCount = new AtomicInteger();
        final AtomicReference<String> firstConnection = new AtomicReference<>();
        final AtomicReference<String> secondConnection = new AtomicReference<>();
        server = HttpServer.create()
                .host("127.0.0.1")
                .port(0)
                .handle((request, response) -> {
                    captureConnection(response, requestCount.incrementAndGet() == 1
                            ? firstConnection : secondConnection);
                    return response.sendString(Mono.just("complete"));
                })
                .bindNow();
        final WebClientAiProxyTransport transport = transport(defaultProperties());

        assertEquals("complete", responseBody(transport.execute(request("/first")).block()));
        assertEquals("complete", responseBody(transport.execute(request("/second")).block()));

        assertSameConnection(firstConnection.get(), secondConnection.get());
    }

    @Test
    void testDoesNotReuseConnectionAfterDownstreamCancellation() throws InterruptedException {
        final AtomicReference<String> cancelledConnection = new AtomicReference<>();
        final AtomicReference<String> nextConnection = new AtomicReference<>();
        final CountDownLatch upstreamCancelled = new CountDownLatch(1);
        server = HttpServer.create()
                .host("127.0.0.1")
                .port(0)
                .handle((request, response) -> {
                    if ("/stream".equals(request.uri())) {
                        captureConnection(response, cancelledConnection);
                        return response.chunkedTransfer(true)
                                .sendString(Flux.interval(Duration.ofMillis(25))
                                        .map(index -> "chunk-" + index)
                                        .doOnCancel(upstreamCancelled::countDown));
                    }
                    captureConnection(response, nextConnection);
                    return response.sendString(Mono.just("ok"));
                })
                .bindNow();
        final WebClientAiProxyTransport transport = transport(defaultProperties());

        StepVerifier.create(transport.execute(request("/stream"))
                        .flatMapMany(AiUpstreamResponse::getBody))
                .expectNextCount(1)
                .thenCancel()
                .verify();
        assertTrue(upstreamCancelled.await(5, TimeUnit.SECONDS));
        assertEquals("ok", responseBody(transport.execute(request("/complete")).block()));

        assertDifferentConnections(cancelledConnection.get(), nextConnection.get());
    }

    @Test
    void testDoesNotReuseConnectionAfterStreamIdleTimeout() throws InterruptedException {
        final AtomicReference<String> timedOutConnection = new AtomicReference<>();
        final AtomicReference<String> nextConnection = new AtomicReference<>();
        final CountDownLatch upstreamCancelled = new CountDownLatch(1);
        server = HttpServer.create()
                .host("127.0.0.1")
                .port(0)
                .handle((request, response) -> {
                    if ("/idle".equals(request.uri())) {
                        captureConnection(response, timedOutConnection);
                        return response.chunkedTransfer(true)
                                .sendString(Flux.concat(
                                                Mono.just("first"),
                                                Mono.delay(Duration.ofSeconds(5)).thenReturn("late"))
                                        .doOnCancel(upstreamCancelled::countDown));
                    }
                    captureConnection(response, nextConnection);
                    return response.sendString(Mono.just("ok"));
                })
                .bindNow();
        final AiProxyTransportProperties properties = defaultProperties();
        properties.setStreamingIdleTimeout(Duration.ofMillis(100));
        final WebClientAiProxyTransport transport = transport(properties);

        StepVerifier.create(transport.execute(request("/idle"))
                        .flatMapMany(AiUpstreamResponse::getBody)
                        .map(bytes -> new String(bytes, StandardCharsets.UTF_8)))
                .expectNext("first")
                .expectErrorSatisfies(error -> assertKind(error,
                        AiTransportException.Kind.STREAM_IDLE_TIMEOUT))
                .verify();
        assertTrue(upstreamCancelled.await(5, TimeUnit.SECONDS));
        assertEquals("ok", responseBody(transport.execute(request("/complete")).block()));

        assertDifferentConnections(timedOutConnection.get(), nextConnection.get());
    }

    @Test
    void testDoesNotReuseConnectionAfterResponseSizeOverflow() throws InterruptedException {
        final AtomicReference<String> overflowConnection = new AtomicReference<>();
        final AtomicReference<String> nextConnection = new AtomicReference<>();
        final CountDownLatch upstreamCancelled = new CountDownLatch(1);
        server = HttpServer.create()
                .host("127.0.0.1")
                .port(0)
                .handle((request, response) -> {
                    if ("/oversized".equals(request.uri())) {
                        captureConnection(response, overflowConnection);
                        return response.chunkedTransfer(true)
                                .sendString(Flux.concat(
                                                Mono.just("abc"),
                                                Mono.delay(Duration.ofMillis(100)).thenReturn("def"),
                                                Mono.delay(Duration.ofSeconds(5)).thenReturn("late"))
                                        .doOnCancel(upstreamCancelled::countDown));
                    }
                    captureConnection(response, nextConnection);
                    return response.sendString(Mono.just("ok"));
                })
                .bindNow();
        final AiProxyTransportProperties properties = defaultProperties();
        properties.setMaxResponseBytes(5);
        final WebClientAiProxyTransport transport = transport(properties);

        StepVerifier.create(transport.execute(request("/oversized"))
                        .flatMapMany(AiUpstreamResponse::getBody)
                        .map(bytes -> new String(bytes, StandardCharsets.UTF_8)))
                .expectNext("abc")
                .expectErrorSatisfies(error -> assertKind(error,
                        AiTransportException.Kind.RESPONSE_TOO_LARGE))
                .verify();
        assertTrue(upstreamCancelled.await(5, TimeUnit.SECONDS));
        assertEquals("ok", responseBody(transport.execute(request("/complete")).block()));

        assertDifferentConnections(overflowConnection.get(), nextConnection.get());
    }

    private WebClientAiProxyTransport transport(final AiProxyTransportProperties properties) {
        connectionProvider = ConnectionProvider.builder("ai-proxy-transport-integration")
                .maxConnections(1)
                .pendingAcquireTimeout(Duration.ofSeconds(1))
                .build();
        final HttpClient httpClient = HttpClient.create(connectionProvider)
                .responseTimeout(properties.getResponseHeaderTimeout())
                .doOnResponse((response, connection) ->
                        connection.removeHandler(NettyPipeline.ResponseTimeoutHandler));
        final WebClient webClient = WebClient.builder()
                .clientConnector(new ReactorClientHttpConnector(httpClient))
                .build();
        return new WebClientAiProxyTransport(webClient, properties);
    }

    private AiUpstreamRequest request() {
        return request("/v1/chat/completions");
    }

    private AiUpstreamRequest request(final String path) {
        final AiUpstreamRequest request = new AiUpstreamRequest();
        request.setMethod("POST");
        request.setUri(URI.create("http://127.0.0.1:" + server.port() + path));
        request.setHeaders(Map.of(
                HttpHeaders.AUTHORIZATION, List.of("Bearer integration-key"),
                HttpHeaders.CONTENT_TYPE, List.of(MediaType.APPLICATION_JSON_VALUE)));
        request.setBody("{}".getBytes(StandardCharsets.UTF_8));
        return request;
    }

    private AiProxyTransportProperties defaultProperties() {
        final AiProxyTransportProperties properties = new AiProxyTransportProperties();
        properties.setResponseHeaderTimeout(Duration.ofSeconds(2));
        properties.setStreamingIdleTimeout(Duration.ofSeconds(2));
        properties.setTotalStreamDuration(Duration.ofSeconds(10));
        return properties;
    }

    private Flux<String> asStrings(final AiUpstreamResponse response) {
        return response.getBody().map(bytes -> new String(bytes, StandardCharsets.UTF_8));
    }

    private String responseBody(final AiUpstreamResponse response) {
        return asStrings(response).collectList().map(parts -> String.join("", parts)).block();
    }

    private void captureConnection(final HttpServerResponse response, final AtomicReference<String> connectionId) {
        response.withConnection(connection -> connectionId.set(connection.channel().id().asLongText()));
    }

    private void assertSameConnection(final String first, final String second) {
        assertNotNull(first);
        assertNotNull(second);
        assertEquals(first, second);
    }

    private void assertDifferentConnections(final String first, final String second) {
        assertNotNull(first);
        assertNotNull(second);
        assertNotEquals(first, second);
    }

    private void assertKind(final Throwable error, final AiTransportException.Kind expected) {
        assertEquals(expected, assertInstanceOf(AiTransportException.class, error).getKind());
    }
}
