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

import io.netty.channel.ConnectTimeoutException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.junit.jupiter.api.Test;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DefaultDataBufferFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.web.reactive.function.client.ClientRequest;
import org.springframework.web.reactive.function.client.ClientResponse;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.netty.http.client.PrematureCloseException;
import reactor.test.StepVerifier;

import javax.net.ssl.SSLException;
import java.io.IOException;
import java.net.URI;
import java.net.UnknownHostException;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class WebClientAiProxyTransportTest {

    @Test
    void testExecuteForwardsRequestAndStreamsResponse() {
        final AtomicReference<ClientRequest> capturedRequest = new AtomicReference<>();
        final Flux<String> chunks = Flux.just("data: first\n\n", "data: second\n\n");
        final WebClient webClient = WebClient.builder()
                .exchangeFunction(request -> {
                    capturedRequest.set(request);
                    return Mono.just(ClientResponse.create(HttpStatus.OK)
                            .header(HttpHeaders.CONTENT_TYPE, MediaType.TEXT_EVENT_STREAM_VALUE)
                            .body(chunks.map(chunk -> DefaultDataBufferFactory.sharedInstance.wrap(
                                    chunk.getBytes(StandardCharsets.UTF_8))))
                            .build());
                })
                .build();
        final WebClientAiProxyTransport transport = new WebClientAiProxyTransport(webClient);

        final AiUpstreamResponse response = transport.execute(createRequest()).block();

        assertEquals(HttpStatus.OK.value(), response.getStatusCode());
        assertEquals(MediaType.TEXT_EVENT_STREAM_VALUE,
                response.getHeaders().get(HttpHeaders.CONTENT_TYPE).get(0));
        assertEquals("POST", capturedRequest.get().method().name());
        assertEquals("Bearer test-key", capturedRequest.get().headers().getFirst(HttpHeaders.AUTHORIZATION));
        StepVerifier.create(response.getBody().map(bytes -> new String(bytes, StandardCharsets.UTF_8)))
                .expectNext("data: first\n\n", "data: second\n\n")
                .verifyComplete();
    }

    @Test
    void testExecutePreservesErrorStatusAndBody() {
        final WebClient webClient = WebClient.builder()
                .exchangeFunction(request -> Mono.just(ClientResponse.create(HttpStatus.BAD_REQUEST)
                        .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                        .body("{\"error\":\"bad request\"}")
                        .build()))
                .build();
        final WebClientAiProxyTransport transport = new WebClientAiProxyTransport(webClient);

        final AiUpstreamResponse response = transport.execute(createRequest()).block();

        assertEquals(HttpStatus.BAD_REQUEST.value(), response.getStatusCode());
        StepVerifier.create(response.getBody().map(bytes -> new String(bytes, StandardCharsets.UTF_8)))
                .expectNext("{\"error\":\"bad request\"}")
                .verifyComplete();
    }

    @Test
    void testAllowsResponseExactlyAtMaximumSize() {
        final WebClient webClient = webClientWithBody(Flux.just(dataBuffer("ab"), dataBuffer("cde")));
        final AiProxyTransportProperties properties = new AiProxyTransportProperties();
        properties.setMaxResponseBytes(5);
        final WebClientAiProxyTransport transport = new WebClientAiProxyTransport(webClient, properties);

        StepVerifier.create(transport.execute(createRequest()).flatMapMany(AiUpstreamResponse::getBody)
                        .map(bytes -> new String(bytes, StandardCharsets.UTF_8)))
                .expectNext("ab", "cde")
                .verifyComplete();
    }

    @Test
    void testAllowsContentLengthExactlyAtMaximumSize() {
        final WebClient webClient = WebClient.builder()
                .exchangeFunction(request -> Mono.just(ClientResponse.create(HttpStatus.OK)
                        .header(HttpHeaders.CONTENT_LENGTH, "5")
                        .body(Flux.just(dataBuffer("abcde")))
                        .build()))
                .build();
        final AiProxyTransportProperties properties = new AiProxyTransportProperties();
        properties.setMaxResponseBytes(5);
        final WebClientAiProxyTransport transport = new WebClientAiProxyTransport(webClient, properties);

        StepVerifier.create(transport.execute(createRequest()).flatMapMany(AiUpstreamResponse::getBody)
                        .map(bytes -> new String(bytes, StandardCharsets.UTF_8)))
                .expectNext("abcde")
                .verifyComplete();
    }

    @Test
    void testRejectsOversizedContentLengthAndCancelsBody() {
        final AtomicBoolean subscribed = new AtomicBoolean();
        final AtomicBoolean cancelled = new AtomicBoolean();
        final Flux<DataBuffer> body = Flux.<DataBuffer>never()
                .doOnSubscribe(ignored -> subscribed.set(true))
                .doOnCancel(() -> cancelled.set(true));
        final WebClient webClient = WebClient.builder()
                .exchangeFunction(request -> Mono.just(ClientResponse.create(HttpStatus.OK)
                        .header(HttpHeaders.CONTENT_LENGTH, "6")
                        .body(body)
                        .build()))
                .build();
        final AiProxyTransportProperties properties = new AiProxyTransportProperties();
        properties.setMaxResponseBytes(5);
        final WebClientAiProxyTransport transport = new WebClientAiProxyTransport(webClient, properties);

        StepVerifier.create(transport.execute(createRequest()).flatMapMany(AiUpstreamResponse::getBody))
                .expectErrorSatisfies(error -> assertEquals(AiTransportException.Kind.RESPONSE_TOO_LARGE,
                        assertInstanceOf(AiTransportException.class, error).getKind()))
                .verify();
        assertTrue(subscribed.get());
        assertTrue(cancelled.get());
    }

    @Test
    void testRejectsChunkThatExceedsCumulativeResponseSize() {
        final AtomicBoolean cancelled = new AtomicBoolean();
        final Flux<DataBuffer> body = Flux.concat(
                        Flux.just(dataBuffer("abc"), dataBuffer("def")),
                        Flux.<DataBuffer>never())
                .doOnCancel(() -> cancelled.set(true));
        final WebClient webClient = webClientWithBody(body);
        final AiProxyTransportProperties properties = new AiProxyTransportProperties();
        properties.setMaxResponseBytes(5);
        final WebClientAiProxyTransport transport = new WebClientAiProxyTransport(webClient, properties);

        StepVerifier.create(transport.execute(createRequest()).flatMapMany(AiUpstreamResponse::getBody)
                        .map(bytes -> new String(bytes, StandardCharsets.UTF_8)))
                .expectNext("abc")
                .expectErrorSatisfies(error -> assertEquals(AiTransportException.Kind.RESPONSE_TOO_LARGE,
                        assertInstanceOf(AiTransportException.class, error).getKind()))
                .verify();
        assertTrue(cancelled.get());
    }

    @Test
    void testRejectsOversizedFirstChunkWithoutEmittingIt() {
        final WebClient webClient = webClientWithBody(Flux.just(dataBuffer("abcdef")));
        final AiProxyTransportProperties properties = new AiProxyTransportProperties();
        properties.setMaxResponseBytes(5);
        final WebClientAiProxyTransport transport = new WebClientAiProxyTransport(webClient, properties);

        StepVerifier.create(transport.execute(createRequest()).flatMapMany(AiUpstreamResponse::getBody))
                .expectErrorSatisfies(error -> assertEquals(AiTransportException.Kind.RESPONSE_TOO_LARGE,
                        assertInstanceOf(AiTransportException.class, error).getKind()))
                .verify();
    }

    @Test
    void testClassifiesExchangeFailures() {
        assertFailureKind(new ConnectTimeoutException(), AiTransportException.Kind.CONNECT_TIMEOUT);
        assertFailureKind(new UnknownHostException(), AiTransportException.Kind.DNS_FAILURE);
        assertFailureKind(new SSLException("handshake failed"), AiTransportException.Kind.TLS_FAILURE);
        assertFailureKind(PrematureCloseException.TEST_EXCEPTION, AiTransportException.Kind.CONNECTION_RESET);
        assertFailureKind(new IOException("read failed"), AiTransportException.Kind.IO_FAILURE);
        assertFailureKind(new IllegalStateException("unexpected"), AiTransportException.Kind.UNKNOWN);
    }

    @Test
    void testClassifiesWrappedBodyFailure() {
        final WebClient webClient = WebClient.builder()
                .exchangeFunction(request -> Mono.just(ClientResponse.create(HttpStatus.OK)
                        .body(Flux.error(new IllegalStateException(new SSLException("handshake failed"))))
                        .build()))
                .build();
        final AiUpstreamResponse response = new WebClientAiProxyTransport(webClient).execute(createRequest()).block();

        StepVerifier.create(response.getBody())
                .expectErrorSatisfies(error -> assertEquals(AiTransportException.Kind.TLS_FAILURE,
                        assertInstanceOf(AiTransportException.class, error).getKind()))
                .verify();
    }

    @Test
    void testResponseHeaderTimeout() {
        final WebClient webClient = WebClient.builder()
                .exchangeFunction(request -> Mono.delay(Duration.ofSeconds(10))
                        .thenReturn(ClientResponse.create(HttpStatus.OK).build()))
                .build();
        final WebClientAiProxyTransport transport = new WebClientAiProxyTransport(
                webClient, properties(Duration.ofSeconds(5), Duration.ofSeconds(60), Duration.ofMinutes(10)));

        StepVerifier.withVirtualTime(() -> transport.execute(createRequest()))
                .thenAwait(Duration.ofSeconds(5))
                .expectErrorSatisfies(error -> assertEquals(AiTransportException.Kind.RESPONSE_HEADER_TIMEOUT,
                        assertInstanceOf(AiTransportException.class, error).getKind()))
                .verify();
    }

    @Test
    void testResponseHeaderTimeoutDoesNotLimitBodyStream() {
        final WebClient webClient = WebClient.builder()
                .exchangeFunction(request -> Mono.just(ClientResponse.create(HttpStatus.OK)
                        .body(Flux.never())
                        .build()))
                .build();
        final WebClientAiProxyTransport transport = new WebClientAiProxyTransport(
                webClient, properties(Duration.ofSeconds(5), Duration.ofSeconds(60), Duration.ofMinutes(10)));

        StepVerifier.withVirtualTime(() -> transport.execute(createRequest()))
                .expectNextMatches(response -> response.getStatusCode() == HttpStatus.OK.value())
                .verifyComplete();
    }

    @Test
    void testStreamingIdleTimeout() {
        final WebClient webClient = WebClient.builder()
                .exchangeFunction(request -> Mono.just(ClientResponse.create(HttpStatus.OK)
                        .body(Flux.never())
                        .build()))
                .build();
        final WebClientAiProxyTransport transport = new WebClientAiProxyTransport(
                webClient, properties(Duration.ofSeconds(5), Duration.ofSeconds(10), Duration.ofMinutes(10)));

        StepVerifier.withVirtualTime(() -> transport.execute(createRequest())
                        .flatMapMany(AiUpstreamResponse::getBody))
                .thenAwait(Duration.ofSeconds(10))
                .expectErrorSatisfies(error -> assertEquals(AiTransportException.Kind.STREAM_IDLE_TIMEOUT,
                        assertInstanceOf(AiTransportException.class, error).getKind()))
                .verify();
    }

    @Test
    void testStreamingIdleTimeoutRestartsAfterEachChunk() {
        final Flux<DataBuffer> body = Flux.defer(() -> Flux.interval(Duration.ofSeconds(4))
                .take(2)
                .map(index -> DefaultDataBufferFactory.sharedInstance.wrap(
                        ("chunk-" + index).getBytes(StandardCharsets.UTF_8))));
        final WebClient webClient = WebClient.builder()
                .exchangeFunction(request -> Mono.just(ClientResponse.create(HttpStatus.OK)
                        .body(body)
                        .build()))
                .build();
        final WebClientAiProxyTransport transport = new WebClientAiProxyTransport(
                webClient, properties(Duration.ofSeconds(5), Duration.ofSeconds(5), Duration.ofMinutes(10)));

        StepVerifier.withVirtualTime(() -> transport.execute(createRequest())
                        .flatMapMany(AiUpstreamResponse::getBody)
                        .map(bytes -> new String(bytes, StandardCharsets.UTF_8)))
                .thenAwait(Duration.ofSeconds(8))
                .expectNext("chunk-0", "chunk-1")
                .verifyComplete();
    }

    @Test
    void testTotalStreamDurationCancelsActiveStream() {
        final AtomicBoolean cancelled = new AtomicBoolean();
        final Flux<DataBuffer> body = Flux.defer(() -> Flux.interval(Duration.ofSeconds(1))
                .<DataBuffer>map(index -> DefaultDataBufferFactory.sharedInstance.wrap(
                        ("chunk-" + index).getBytes(StandardCharsets.UTF_8))))
                .doOnCancel(() -> cancelled.set(true));
        final WebClient webClient = WebClient.builder()
                .exchangeFunction(request -> Mono.just(ClientResponse.create(HttpStatus.OK)
                        .body(body)
                        .build()))
                .build();
        final WebClientAiProxyTransport transport = new WebClientAiProxyTransport(
                webClient, properties(Duration.ofSeconds(5), Duration.ofSeconds(2), Duration.ofMillis(4500)));

        StepVerifier.withVirtualTime(() -> transport.execute(createRequest())
                        .flatMapMany(AiUpstreamResponse::getBody))
                .thenAwait(Duration.ofMillis(4500))
                .expectNextCount(4)
                .expectErrorSatisfies(error -> assertEquals(AiTransportException.Kind.STREAM_DURATION_TIMEOUT,
                        assertInstanceOf(AiTransportException.class, error).getKind()))
                .verify();
        assertEquals(true, cancelled.get());
    }

    @Test
    void testTotalStreamDurationAllowsNormalCompletion() {
        final Flux<DataBuffer> body = Flux.just(DefaultDataBufferFactory.sharedInstance.wrap(
                "complete".getBytes(StandardCharsets.UTF_8)));
        final WebClient webClient = WebClient.builder()
                .exchangeFunction(request -> Mono.just(ClientResponse.create(HttpStatus.OK)
                        .body(body)
                        .build()))
                .build();
        final WebClientAiProxyTransport transport = new WebClientAiProxyTransport(
                webClient, properties(Duration.ofSeconds(5), Duration.ofSeconds(5), Duration.ofSeconds(10)));

        StepVerifier.withVirtualTime(() -> transport.execute(createRequest())
                        .flatMapMany(AiUpstreamResponse::getBody)
                        .map(bytes -> new String(bytes, StandardCharsets.UTF_8)))
                .expectNext("complete")
                .verifyComplete();
    }

    @Test
    void testRejectsNonPositiveTimeouts() {
        final AiProxyTransportProperties properties = new AiProxyTransportProperties();

        assertThrows(IllegalArgumentException.class, () -> properties.setResponseHeaderTimeout(Duration.ZERO));
        assertThrows(IllegalArgumentException.class, () -> properties.setStreamingIdleTimeout(Duration.ofSeconds(-1)));
        assertThrows(IllegalArgumentException.class, () -> properties.setTotalStreamDuration(Duration.ZERO));
        assertThrows(IllegalArgumentException.class, () -> properties.setMaxResponseBytes(-1));
    }

    private void assertFailureKind(final Throwable failure, final AiTransportException.Kind expected) {
        final WebClient webClient = WebClient.builder()
                .exchangeFunction(request -> Mono.error(failure))
                .build();
        final WebClientAiProxyTransport transport = new WebClientAiProxyTransport(webClient);

        StepVerifier.create(transport.execute(createRequest()))
                .expectErrorSatisfies(error -> assertEquals(expected,
                        assertInstanceOf(AiTransportException.class, error).getKind()))
                .verify();
    }

    private AiUpstreamRequest createRequest() {
        final AiUpstreamRequest request = new AiUpstreamRequest();
        request.setMethod("POST");
        request.setUri(URI.create("https://api.openai.com/v1/chat/completions"));
        request.setHeaders(Map.of(
                HttpHeaders.AUTHORIZATION, List.of("Bearer test-key"),
                HttpHeaders.CONTENT_TYPE, List.of(MediaType.APPLICATION_JSON_VALUE)));
        request.setBody("{}".getBytes(StandardCharsets.UTF_8));
        return request;
    }

    private WebClient webClientWithBody(final Flux<DataBuffer> body) {
        return WebClient.builder()
                .exchangeFunction(request -> Mono.just(ClientResponse.create(HttpStatus.OK)
                        .body(body)
                        .build()))
                .build();
    }

    private DataBuffer dataBuffer(final String value) {
        return DefaultDataBufferFactory.sharedInstance.wrap(value.getBytes(StandardCharsets.UTF_8));
    }

    private AiProxyTransportProperties properties(final Duration responseHeaderTimeout,
                                                   final Duration streamingIdleTimeout,
                                                   final Duration totalStreamDuration) {
        final AiProxyTransportProperties properties = new AiProxyTransportProperties();
        properties.setResponseHeaderTimeout(responseHeaderTimeout);
        properties.setStreamingIdleTimeout(streamingIdleTimeout);
        properties.setTotalStreamDuration(totalStreamDuration);
        return properties;
    }
}
