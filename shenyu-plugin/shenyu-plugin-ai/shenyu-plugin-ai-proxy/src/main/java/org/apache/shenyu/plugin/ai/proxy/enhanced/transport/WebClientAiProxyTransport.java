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
import io.netty.handler.codec.DecoderException;
import io.netty.handler.timeout.ReadTimeoutException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.util.ClassUtils;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.netty.http.client.PrematureCloseException;

import javax.net.ssl.SSLException;
import java.io.IOException;
import java.net.UnknownHostException;
import java.time.Duration;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;

/**
 * AI transport implemented with Spring WebClient.
 */
public final class WebClientAiProxyTransport implements AiProxyTransport {

    private static final String POOL_ACQUIRE_TIMEOUT_EXCEPTION_NAME =
            "reactor.netty.internal.shaded.reactor.pool.PoolAcquireTimeoutException";

    private static final boolean OTEL_AVAILABLE = ClassUtils.isPresent(
            "io.opentelemetry.api.trace.Span", WebClientAiProxyTransport.class.getClassLoader());

    private final WebClient webClient;

    private final Duration responseHeaderTimeout;

    private final Duration streamingIdleTimeout;

    private final Duration totalStreamDuration;

    private final long maxRequestBytes;

    private final long maxResponseBytes;

    /**
     * Create a WebClient transport.
     *
     * @param webClient web client
     */
    public WebClientAiProxyTransport(final WebClient webClient) {
        this(webClient, new AiProxyTransportProperties());
    }

    /**
     * Create a WebClient transport.
     *
     * @param webClient web client
     * @param properties transport properties
     */
    public WebClientAiProxyTransport(final WebClient webClient, final AiProxyTransportProperties properties) {
        this.webClient = Objects.requireNonNull(webClient, "webClient must not be null").mutate().defaultHeaders(headers -> {
            headers.remove("traceparent");
            headers.remove("tracestate");
            headers.remove("baggage");
        }).build();
        Objects.requireNonNull(properties, "properties must not be null");
        this.responseHeaderTimeout = AiProxyTransportProperties.requirePositive(
                properties.getResponseHeaderTimeout(), "responseHeaderTimeout");
        this.streamingIdleTimeout = AiProxyTransportProperties.requirePositive(
                properties.getStreamingIdleTimeout(), "streamingIdleTimeout");
        this.totalStreamDuration = AiProxyTransportProperties.requirePositive(
                properties.getTotalStreamDuration(), "totalStreamDuration");
        this.maxRequestBytes = AiProxyTransportProperties.requireNonNegative(
                properties.getMaxRequestBytes(), "maxRequestBytes");
        this.maxResponseBytes = AiProxyTransportProperties.requireNonNegative(
                properties.getMaxResponseBytes(), "maxResponseBytes");
    }

    @Override
    public Mono<AiUpstreamResponse> execute(final AiUpstreamRequest request) {
        return Mono.deferContextual(context -> exchange(request, OTEL_AVAILABLE ? AiOutboundTraceContext.headers(context) : Map.of()));
    }

    private Mono<AiUpstreamResponse> exchange(final AiUpstreamRequest request, final Map<String, String> trace) {
        if (exceedsMaxRequestBytes(request)) {
            return Mono.error(new AiTransportException(
                    AiTransportException.Kind.REQUEST_TOO_LARGE,
                    "AI upstream request exceeded maximum size",
                    null));
        }
        final WebClient.RequestBodySpec bodySpec = webClient
                .method(HttpMethod.valueOf(request.getMethod()))
                .uri(request.getUri())
                .headers(headers -> {
                    request.getHeaders().forEach(headers::put);
                    // Never forward configured or client-carried stale telemetry.
                    headers.remove("traceparent");
                    headers.remove("tracestate");
                    headers.remove("baggage");
                    trace.forEach(headers::set);
                });
        WebClient.RequestHeadersSpec<?> headersSpec = bodySpec;
        if (Objects.nonNull(request.getBody()) && request.getBody().length > 0) {
            headersSpec = bodySpec.bodyValue(request.getBody());
        }

        return headersSpec
                .retrieve()
                .onRawStatus(status -> status >= 400, response -> Mono.empty())
                .toEntityFlux(DataBuffer.class)
                .timeout(
                        responseHeaderTimeout,
                        timeoutFailure(
                                AiTransportException.Kind.RESPONSE_HEADER_TIMEOUT,
                                "AI upstream response headers timed out"))
                .flatMap(entity -> {
                    final Flux<DataBuffer> rawBody = Objects.requireNonNull(entity.getBody());
                    if (exceedsMaxResponseBytes(entity.getHeaders())) {
                        return rawBody.take(0, false).then(Mono.error(responseTooLarge()));
                    }

                    final Flux<DataBuffer> dataBuffers = enforceTotalStreamDuration(
                            rawBody.timeout(
                                    streamingIdleTimeout,
                                    timeoutFailure(
                                            AiTransportException.Kind.STREAM_IDLE_TIMEOUT,
                                            "AI upstream response body became idle")));
                    final Flux<byte[]> bytes = dataBuffers
                            .map(buffer -> {
                                try {
                                    final byte[] result = new byte[buffer.readableByteCount()];
                                    buffer.read(result);
                                    return result;
                                } finally {
                                    DataBufferUtils.release(buffer);
                                }
                            })
                            .doOnDiscard(DataBuffer.class, DataBufferUtils::release)
                            .onErrorMap(error -> !(error instanceof AiTransportException),
                                    error -> toTransportException("AI upstream body failed", error, true));
                    return Mono.just(new AiUpstreamResponse(
                            entity.getStatusCode().value(),
                            entity.getHeaders(),
                            enforceMaxResponseBytes(bytes)));
                })
                .onErrorMap(error -> !(error instanceof AiTransportException),
                        error -> toTransportException("AI upstream exchange failed", error, false));
    }

    private <T> Mono<T> timeoutFailure(final AiTransportException.Kind kind, final String message) {
        return Mono.defer(() -> Mono.error(new AiTransportException(
                kind,
                message,
                new TimeoutException(message))));
    }

    private Flux<DataBuffer> enforceTotalStreamDuration(final Flux<DataBuffer> body) {
        return Flux.defer(() -> {
            final AtomicBoolean expired = new AtomicBoolean();
            final Mono<Long> deadline = Mono.delay(totalStreamDuration)
                    .doOnNext(ignored -> expired.set(true));
            return body.takeUntilOther(deadline)
                    .concatWith(Flux.defer(() -> expired.get()
                            ? Flux.error(new AiTransportException(
                                    AiTransportException.Kind.STREAM_DURATION_TIMEOUT,
                                    "AI upstream response body exceeded total duration",
                                    new TimeoutException("AI upstream response body exceeded total duration")))
                            : Flux.empty()));
        });
    }

    private AiTransportException toTransportException(final String message, final Throwable failure,
                                                       final boolean responseStarted) {
        return new AiTransportException(classify(failure, responseStarted), message, failure);
    }

    private AiTransportException.Kind classify(final Throwable failure, final boolean responseStarted) {
        if (hasCauseNamed(failure, POOL_ACQUIRE_TIMEOUT_EXCEPTION_NAME)) {
            return AiTransportException.Kind.CONNECTION_POOL_TIMEOUT;
        }
        if (hasCause(failure, ConnectTimeoutException.class)) {
            return AiTransportException.Kind.CONNECT_TIMEOUT;
        }
        if (!responseStarted && hasCause(failure, ReadTimeoutException.class)) {
            return AiTransportException.Kind.RESPONSE_HEADER_TIMEOUT;
        }
        if (hasCause(failure, UnknownHostException.class)) {
            return AiTransportException.Kind.DNS_FAILURE;
        }
        if (hasCause(failure, SSLException.class)) {
            return AiTransportException.Kind.TLS_FAILURE;
        }
        if (hasCause(failure, PrematureCloseException.class)) {
            return AiTransportException.Kind.CONNECTION_RESET;
        }
        if (hasCause(failure, DecoderException.class)) {
            return AiTransportException.Kind.IO_FAILURE;
        }
        if (hasCause(failure, IOException.class)) {
            return AiTransportException.Kind.IO_FAILURE;
        }
        return AiTransportException.Kind.UNKNOWN;
    }

    private boolean hasCauseNamed(final Throwable failure, final String className) {
        Throwable current = failure;
        while (Objects.nonNull(current)) {
            if (className.equals(current.getClass().getName())) {
                return true;
            }
            final Throwable cause = current.getCause();
            if (cause == current) {
                return false;
            }
            current = cause;
        }
        return false;
    }

    private boolean hasCause(final Throwable failure, final Class<? extends Throwable> type) {
        Throwable current = failure;
        while (Objects.nonNull(current)) {
            if (type.isInstance(current)) {
                return true;
            }
            Throwable cause = current.getCause();
            if (cause == current) {
                return false;
            }
            current = cause;
        }
        return false;
    }

    private Flux<byte[]> enforceMaxResponseBytes(final Flux<byte[]> body) {
        if (maxResponseBytes == 0) {
            return body;
        }
        return Flux.defer(() -> {
            final AtomicLong receivedBytes = new AtomicLong();
            return body.handle((bytes, sink) -> {
                final long received = receivedBytes.get();
                if (bytes.length > maxResponseBytes - received) {
                    sink.error(responseTooLarge());
                    return;
                }
                receivedBytes.set(received + bytes.length);
                sink.next(bytes);
            });
        });
    }

    private boolean exceedsMaxRequestBytes(final AiUpstreamRequest request) {
        return maxRequestBytes > 0
                && Objects.nonNull(request.getBody())
                && request.getBody().length > maxRequestBytes;
    }

    private boolean exceedsMaxResponseBytes(
            final HttpHeaders headers) {
        if (maxResponseBytes == 0) {
            return false;
        }

        final long contentLength = headers.getContentLength();
        return contentLength >= 0
                && contentLength > maxResponseBytes;
    }

    private AiTransportException responseTooLarge() {
        return new AiTransportException(
                AiTransportException.Kind.RESPONSE_TOO_LARGE,
                "AI upstream response exceeded maximum size",
                null);
    }
}
