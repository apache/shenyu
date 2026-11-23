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

import io.netty.buffer.PooledByteBufAllocator;
import io.netty.channel.ConnectTimeoutException;
import io.netty.handler.codec.DecoderException;
import io.netty.handler.timeout.ReadTimeoutException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiProxyException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.junit.jupiter.api.Test;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.core.io.buffer.DefaultDataBufferFactory;
import org.springframework.core.io.buffer.NettyDataBuffer;
import org.springframework.core.io.buffer.NettyDataBufferFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.mock.http.client.reactive.MockClientHttpRequest;
import org.springframework.web.reactive.function.client.ClientRequest;
import org.springframework.web.reactive.function.client.ClientResponse;
import org.springframework.web.reactive.function.client.ExchangeStrategies;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.netty.http.client.PrematureCloseException;
import reactor.netty.internal.shaded.reactor.pool.PoolAcquireTimeoutException;
import reactor.test.StepVerifier;
import reactor.test.publisher.TestPublisher;

import javax.net.ssl.SSLException;
import java.io.IOException;
import java.net.URI;
import java.net.UnknownHostException;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class WebClientAiProxyTransportTest {

    @Test
    void streamingUploadWritesBeforeCompletionAndCannotBeReplayed() {
        final List<String> written = new ArrayList<>();
        final TestPublisher<DataBuffer> upload = TestPublisher.create();
        final WebClientAiProxyTransport transport = streamingTransport(new AiProxyTransportProperties(), buffers -> buffers
                .doOnNext(buffer -> {
                    written.add(buffer.toString(StandardCharsets.UTF_8));
                    DataBufferUtils.release(buffer);
                }).then());
        // 对同一发送对象订阅两次，验证第二次被拒且正文只读取一次。
        final Mono<AiUpstreamResponse> exchange = transport.prepareStreaming(createRequest(), upload.flux());
        StepVerifier.create(exchange)
                .then(() -> upload.next(dataBuffer(" { ")))
                .then(() -> assertEquals(List.of(" { "), written))
                .then(() -> upload.emit(dataBuffer("} ")))
                .expectNextMatches(response -> response.getStatusCode() == 200)
                .verifyComplete();
        assertEquals(List.of(" { ", "} "), written);
        StepVerifier.create(exchange).expectError(IllegalStateException.class).verify();
        assertEquals(1, upload.subscribeCount());
    }

    @Test
    void cancellingStreamingUploadCancelsOriginalBody() {
        // 发出第一块后取消，验证原始上传流收到取消信号且只被订阅一次。
        final TestPublisher<DataBuffer> upload = TestPublisher.create();
        final WebClientAiProxyTransport transport = streamingTransport(new AiProxyTransportProperties(),
                buffers -> buffers.doOnNext(DataBufferUtils::release).then());
        StepVerifier.create(transport.prepareStreaming(createRequest(), upload.flux()))
                .then(() -> upload.next(dataBuffer("first"))).thenCancel().verify();
        upload.assertCancelled();
        assertEquals(1, upload.subscribeCount());
    }

    @Test
    void streamingUploadRejectsDeclaredAndCumulativeOverflowAndReleasesBuffers() {
        final AiProxyTransportProperties properties = new AiProxyTransportProperties();
        properties.setMaxRequestBytes(3);
        final List<String> written = new ArrayList<>();
        final WebClientAiProxyTransport transport = streamingTransport(properties, buffers -> buffers.doOnNext(buffer -> {
            written.add(buffer.toString(StandardCharsets.UTF_8));
            DataBufferUtils.release(buffer);
        }).then());
        final AiUpstreamRequest request = createRequest();
        final TestPublisher<DataBuffer> upload = TestPublisher.create();
        for (String length : List.of("4", "9223372036854775808")) {
            request.setHeaders(Map.of(HttpHeaders.CONTENT_LENGTH, List.of(length)));
            assertUploadOverflow(assertThrows(AiTransportException.class, () -> transport.prepareStreaming(request, upload.flux())));
        }
        upload.assertWasNotSubscribed();

        final NettyDataBufferFactory factory = new NettyDataBufferFactory(PooledByteBufAllocator.DEFAULT);
        final NettyDataBuffer first = factory.allocateBuffer(2).write(new byte[]{'a', 'b'});
        final NettyDataBuffer rejected = factory.allocateBuffer(2).write(new byte[]{'c', 'd'});
        StepVerifier.create(transport.prepareStreaming(createRequest(), upload.flux()))
                .then(() -> upload.next(first, rejected))
                .expectErrorSatisfies(WebClientAiProxyTransportTest::assertUploadOverflow).verify();
        assertEquals(List.of("ab"), written);
        assertEquals(0, first.getNativeBuffer().refCnt());
        // 验证触发超限的缓冲区已释放，引用计数为零。
        assertEquals(0, rejected.getNativeBuffer().refCnt());
        upload.assertCancelled();
    }

    @Test
    void streamingUploadChecksDeclaredLengthBeforeAndDuringUpload() {
        final WebClientAiProxyTransport transport = streamingTransport(new AiProxyTransportProperties(),
                buffers -> buffers.doOnNext(DataBufferUtils::release).then());
        final AiUpstreamRequest request = createRequest();
        for (List<String> values : List.of(List.of("-1"), List.of("1", "3"), List.of("invalid"))) {
            request.setHeaders(Map.of(HttpHeaders.CONTENT_LENGTH, values));
            final TestPublisher<DataBuffer> upload = TestPublisher.create();
            // 检查非法声明长度和实收长度不符时返回的错误。
            assertInvalidLength(assertThrows(AiProxyException.class, () -> transport.prepareStreaming(request, upload.flux())));
            upload.assertWasNotSubscribed();
        }
        for (String length : List.of("1", "3")) {
            request.setHeaders(Map.of(HttpHeaders.CONTENT_LENGTH, List.of(length)));
            StepVerifier.create(transport.prepareStreaming(request, Flux.just(dataBuffer("ab"))))
                    .expectErrorSatisfies(WebClientAiProxyTransportTest::assertInvalidLength).verify();
        }
    }

    @Test
    void bufferedOnlyTransportRefusesCallerBodyWithoutContactingUpstream() {
        final AiProxyTransport custom = request -> Mono.error(new AssertionError("Buffered transport called"));
        final TestPublisher<DataBuffer> upload = TestPublisher.create();
        // 验证仅支持聚合发送的实现拒绝流式上传且未订阅请求体。
        assertEquals(AiProxyException.Kind.PROVIDER, assertThrows(AiProxyException.class, () -> custom.prepareStreaming(createRequest(), upload.flux())).getKind());
        upload.assertWasNotSubscribed();
    }

    private static void assertUploadOverflow(final Throwable error) {
        assertEquals(AiTransportException.Kind.REQUEST_TOO_LARGE, assertInstanceOf(AiTransportException.class, error).getKind());
    }

    private static void assertInvalidLength(final Throwable error) {
        assertEquals(AiProxyException.Kind.REQUEST, assertInstanceOf(AiProxyException.class, error).getKind());
    }

    private WebClientAiProxyTransport streamingTransport(final AiProxyTransportProperties properties, final Function<Flux<DataBuffer>, Mono<Void>> writer) {
        final WebClient streaming = WebClient.builder().exchangeFunction(request -> {
            final MockClientHttpRequest output = new MockClientHttpRequest(request.method(), request.url());
            output.setWriteHandler(writer);
            return request.writeTo(output, ExchangeStrategies.withDefaults()).thenReturn(ClientResponse.create(HttpStatus.OK).build());
        }).build();
        final WebClient buffered = WebClient.builder().exchangeFunction(request -> Mono.error(new AssertionError("Unexpected client"))).build();
        return new WebClientAiProxyTransport(buffered, properties, streaming);
    }

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
    void testRejectsOversizedRequestBeforeExchange() {
        final AtomicBoolean exchanged = new AtomicBoolean();
        final WebClient webClient = WebClient.builder()
                .exchangeFunction(request -> {
                    exchanged.set(true);
                    return Mono.just(ClientResponse.create(HttpStatus.OK).build());
                })
                .build();
        final AiProxyTransportProperties properties = new AiProxyTransportProperties();
        properties.setMaxRequestBytes(1);
        final WebClientAiProxyTransport transport = new WebClientAiProxyTransport(webClient, properties);

        StepVerifier.create(transport.execute(createRequest()))
                .expectErrorSatisfies(error -> assertEquals(AiTransportException.Kind.REQUEST_TOO_LARGE,
                        assertInstanceOf(AiTransportException.class, error).getKind()))
                .verify();
        assertEquals(false, exchanged.get());
    }

    @Test
    void testAllowsRequestExactlyAtMaximumSize() {
        final WebClient webClient = WebClient.builder()
                .exchangeFunction(request -> Mono.just(ClientResponse.create(HttpStatus.OK).build()))
                .build();
        final AiProxyTransportProperties properties = new AiProxyTransportProperties();
        properties.setMaxRequestBytes(2);
        final WebClientAiProxyTransport transport = new WebClientAiProxyTransport(webClient, properties);

        StepVerifier.create(transport.execute(createRequest()))
                .expectNextMatches(response -> response.getStatusCode() == HttpStatus.OK.value())
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

        StepVerifier.create(transport.execute(createRequest()))
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
        assertFailureKind(ReadTimeoutException.INSTANCE, AiTransportException.Kind.RESPONSE_HEADER_TIMEOUT);
        assertFailureKind(new UnknownHostException(), AiTransportException.Kind.DNS_FAILURE);
        assertFailureKind(new SSLException("handshake failed"), AiTransportException.Kind.TLS_FAILURE);
        assertFailureKind(PrematureCloseException.TEST_EXCEPTION, AiTransportException.Kind.CONNECTION_RESET);
        assertFailureKind(new DecoderException("malformed upstream response"), AiTransportException.Kind.IO_FAILURE);
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
    void testDoesNotClassifyBodyReadTimeoutAsResponseHeaderTimeout() {
        final WebClient webClient = WebClient.builder()
                .exchangeFunction(request -> Mono.just(ClientResponse.create(HttpStatus.OK)
                        .body(Flux.error(ReadTimeoutException.INSTANCE))
                        .build()))
                .build();
        final AiUpstreamResponse response = new WebClientAiProxyTransport(webClient).execute(createRequest()).block();

        StepVerifier.create(response.getBody())
                .expectErrorSatisfies(error -> assertEquals(AiTransportException.Kind.UNKNOWN,
                        assertInstanceOf(AiTransportException.class, error).getKind()))
                .verify();
    }

    @Test
    void testUpstreamTimeoutIsNotReclassifiedAsResponseHeaderTimeout() {
        final WebClient webClient = WebClient.builder()
                .exchangeFunction(request -> Mono.error(new TimeoutException("connection pool acquire timed out")))
                .build();
        final WebClientAiProxyTransport transport = new WebClientAiProxyTransport(webClient);

        StepVerifier.create(transport.execute(createRequest()))
                .expectErrorSatisfies(error -> assertEquals(AiTransportException.Kind.UNKNOWN,
                        assertInstanceOf(AiTransportException.class, error).getKind()))
                .verify();
    }

    @Test
    void testClassifiesConnectionPoolAcquireTimeout() {
        assertFailureKind(new PoolAcquireTimeoutException(Duration.ofMillis(10)),
                AiTransportException.Kind.CONNECTION_POOL_TIMEOUT);
    }

    @Test
    void testResponseBodyHonorsDownstreamBackpressure() {
        final AtomicLong requested = new AtomicLong();
        final Flux<DataBuffer> body = Flux.<DataBuffer>generate(sink -> sink.next(dataBuffer("chunk")))
                .doOnRequest(requested::addAndGet);
        final WebClient webClient = webClientWithBody(body);
        final WebClientAiProxyTransport transport = new WebClientAiProxyTransport(webClient);

        StepVerifier.create(transport.execute(createRequest()).flatMapMany(AiUpstreamResponse::getBody), 0)
                .then(() -> assertEquals(0, requested.get()))
                .thenRequest(1)
                .expectNextMatches(bytes -> "chunk".equals(new String(bytes, StandardCharsets.UTF_8)))
                .then(() -> assertEquals(1, requested.get()))
                .thenRequest(1)
                .expectNextMatches(bytes -> "chunk".equals(new String(bytes, StandardCharsets.UTF_8)))
                .then(() -> assertEquals(2, requested.get()))
                .thenCancel()
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
    void testResponseHeaderTimeoutIsEnforcedForArbitraryWebClient() {
        final WebClient webClient = WebClient.builder()
                .exchangeFunction(request -> Mono.never())
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
        assertThrows(IllegalArgumentException.class, () -> properties.setMaxRequestBytes(-1));
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
