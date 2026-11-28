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

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.AiProxyHandle;
import org.apache.shenyu.common.metrics.AiTerminalEvent;
import org.apache.shenyu.common.metrics.AiUsage;
import org.apache.shenyu.plugin.ai.proxy.enhanced.handler.AiProxyPluginHandler;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocolFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProviderFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.config.AiProxyConfigService;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyEngine;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyRetryProperties;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiProxyTransport;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiProxyTransportProperties;
import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.WebClientAiProxyTransport;
import org.apache.shenyu.plugin.base.utils.AiPassthroughPolicy;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.core.io.buffer.DefaultDataBufferFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.netty.DisposableServer;
import reactor.netty.http.server.HttpServer;
import reactor.test.StepVerifier;
import reactor.test.publisher.TestPublisher;

import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicInteger;

import static org.apache.shenyu.plugin.ai.proxy.enhanced.AiProxyTestUtils.protocolRouteResolver;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;

class AiProxyPassthroughTest {

    private static final String HANDLE = "{\"baseUrl\":\"https://vendor.example:9443/api/\",\"mode\":\"passthrough\","
            + "\"stripPrefix\":\"/native\",\"proxyEnabled\":\"true\",\"apiKey\":\"must-not-be-used\"}";

    private final List<AiTerminalEvent> events = new ArrayList<>();

    private final List<AiUpstreamRequest> sent = new ArrayList<>();

    private final AiProxyPluginHandler handler = new AiProxyPluginHandler();

    private final SelectorData selector = SelectorData.builder().id("native").handle(HANDLE).build();

    private AiProxyPlugin plugin(final AiProxyTransport transport) {
        return plugin(transport, new AiProxyRetryProperties());
    }

    private AiProxyPlugin plugin(final AiProxyTransport transport, final AiProxyRetryProperties retry) {
        handler.handlerSelector(selector);
        AiProxyEngine engine = AiProxyEngine.builder(new AiProxyProtocolFactory(List.of()), new AiProxyProviderFactory(List.of()), protocolRouteResolver(), transport)
                .retryProperties(retry).build();
        return new AiProxyPlugin(new AiProxyConfigService(handler), engine, handler, events::add);
    }

    @Test
    void vendorReceivesOriginalBytesAndCredentialsAndItsErrorIsForwardedOnce() {
        String body = "{ \"messages\" : [], \"vendorExtension\" : \"你好\", \"fallbackConfig\" : {} }\n";
        String error = "{\"error\":\"vendor rejected credential\"}\n";
        URI uri = URI.create("https://client.example/native/v1/models/a%2Fb?key=client%2Bkey&k=1&k=&x");
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.method(HttpMethod.POST, uri)
                .header(HttpHeaders.HOST, "client.example").header(HttpHeaders.CONNECTION, "X-Transient").header("X-Transient", "omit")
                .header("x-api-key", "caller-key").header("Anthropic-Version", "example-version").header("X-Multi", "one", "two").body(body));
        AiProxyTransport transport = new AiProxyTransport() {
            @Override
            public Mono<AiUpstreamResponse> execute(final AiUpstreamRequest request) {
                throw new AssertionError("converted transport invoked");
            }

            @Override
            public Mono<AiUpstreamResponse> prepareStreaming(final AiUpstreamRequest request, final Flux<DataBuffer> upload) {
                sent.add(request);
                return DataBufferUtils.join(upload).map(buffer -> {
                    byte[] received = new byte[buffer.readableByteCount()];
                    buffer.read(received);
                    DataBufferUtils.release(buffer);
                    assertArrayEquals(body.getBytes(StandardCharsets.UTF_8), received);
                    return new AiUpstreamResponse(401, Map.of("X-Vendor-Error", List.of("invalid")), Flux.just(error.getBytes(StandardCharsets.UTF_8)));
                });
            }
        };
        StepVerifier.create(plugin(transport).doExecute(exchange, unused -> Mono.empty(), selector, new RuleData())).verifyComplete();
        assertEquals(1, sent.size());
        AiUpstreamRequest upstream = sent.get(0);
        assertEquals("https://vendor.example:9443/api/v1/models/a%2Fb?key=client%2Bkey&k=1&k=&x", upstream.getUri().toString());
        assertNull(upstream.getBody());
        assertEquals(List.of("caller-key"), upstream.getHeaders().get("x-api-key"));
        assertEquals(List.of("example-version"), upstream.getHeaders().get("Anthropic-Version"));
        assertEquals(List.of("one", "two"), upstream.getHeaders().get("X-Multi"));
        assertFalse(upstream.getHeaders().containsKey(HttpHeaders.HOST));
        assertFalse(upstream.getHeaders().containsKey("X-Transient"));
        assertFalse(upstream.getHeaders().containsKey(HttpHeaders.AUTHORIZATION));
        assertEquals(HttpStatus.UNAUTHORIZED, exchange.getResponse().getStatusCode());
        assertEquals("invalid", exchange.getResponse().getHeaders().getFirst("X-Vendor-Error"));
        assertEquals(error, exchange.getResponse().getBodyAsString().block());
        assertEquals(1, events.size());
    }

    @Test
    void retriesRateLimitedUploadFromOriginalBytesAndReturnsOnlyVendorSse() {
        List<String> uploads = new CopyOnWriteArrayList<>();
        String first = ": heartbeat\r\n\r\ndata: 你好\n\n";
        String last = "event: stop\ndata: {}\n\n";
        DisposableServer vendor = HttpServer.create().host("127.0.0.1").port(0)
                .handle((incoming, response) -> incoming.receive().aggregate().asString(StandardCharsets.UTF_8).flatMap(content -> {
                    uploads.add(content);
                    if (uploads.size() == 1) {
                        return response.status(429).header("Retry-After", "0").sendString(Mono.just("busy")).then();
                    }
                    return response.header("Content-Type", "text/event-stream").sendString(Flux.just(first, last)).then();
                })).bindNow();
        try {
            selector.setHandle("{\"mode\":\"passthrough\",\"baseUrl\":\"http://127.0.0.1:" + vendor.port() + "\"}");
            WebClient client = WebClient.builder().build();
            AtomicInteger subscriptions = new AtomicInteger();
            Flux<DataBuffer> upload = Flux.defer(() -> {
                assertEquals(1, subscriptions.incrementAndGet());
                return Flux.just(" {", "\"message\":\"你好\"}\n")
                        .map(chunk -> DefaultDataBufferFactory.sharedInstance.wrap(chunk.getBytes(StandardCharsets.UTF_8)));
            });
            MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/v1/messages").body(upload));
            AiProxyTransport transport = new WebClientAiProxyTransport(client, new AiProxyTransportProperties(), client);
            StepVerifier.create(plugin(transport).doExecute(exchange, unused -> Mono.empty(), selector, new RuleData())).verifyComplete();
            assertEquals(List.of(" {\"message\":\"你好\"}\n", " {\"message\":\"你好\"}\n"), uploads);
            assertEquals(HttpStatus.OK, exchange.getResponse().getStatusCode());
            assertEquals(first + last, exchange.getResponse().getBodyAsString().block());
            assertEquals(1, events.size());
            assertEquals(2, events.get(0).attemptCount());
        } finally {
            vendor.disposeNow();
        }
    }

    @Test
    void returnsVendorRateLimitWhenReadingTheBodyConsumesTheRetryWindow() {
        AiProxyRetryProperties retry = new AiProxyRetryProperties();
        retry.setRequestDeadline(Duration.ofSeconds(3));
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/native/v1/messages").body("payload"));
        Flux<byte[]> body = Flux.defer(() -> Flux.just("rate ", "limited").map(value -> value.getBytes(StandardCharsets.UTF_8))
                .concatWith(Mono.delay(Duration.ofSeconds(2)).then(Mono.empty())));
        StepVerifier.withVirtualTime(() -> plugin(rateLimited(body), retry).doExecute(exchange, unused -> Mono.empty(), selector, new RuleData()))
                .thenAwait(Duration.ofSeconds(2)).expectComplete().verify(Duration.ofSeconds(5));
        assertEquals(1, sent.size());
        assertEquals(HttpStatus.TOO_MANY_REQUESTS, exchange.getResponse().getStatusCode());
        assertEquals("2", exchange.getResponse().getHeaders().getFirst("Retry-After"));
        assertEquals(List.of("one", "two"), exchange.getResponse().getHeaders().get("X-Vendor"));
        assertEquals("rate limited", exchange.getResponse().getBodyAsString().block());
    }

    @Test
    void oversizedRateLimitStreamsBufferedPrefixAndRemainingBytesOnce() {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/native/v1/messages").body("payload"));
        String large = "x".repeat(16 * 1024);
        TestPublisher<byte[]> body = TestPublisher.create();
        List<String> received = new ArrayList<>();
        exchange.getResponse().setWriteHandler(buffers -> buffers.doOnNext(buffer -> {
            received.add(buffer.toString(StandardCharsets.UTF_8));
            DataBufferUtils.release(buffer);
        }).then());
        StepVerifier.create(plugin(rateLimited(body.flux())).doExecute(exchange, unused -> Mono.empty(), selector, new RuleData()))
                .then(() -> body.next("prefix".getBytes(StandardCharsets.UTF_8), large.getBytes(StandardCharsets.UTF_8)))
                .then(() -> assertEquals("prefix" + large, String.join("", received)))
                .then(() -> body.emit("tail".getBytes(StandardCharsets.UTF_8)))
                .expectComplete().verify(Duration.ofSeconds(5));
        assertEquals(1, sent.size());
        assertEquals(1, body.subscribeCount());
        assertEquals(HttpStatus.TOO_MANY_REQUESTS, exchange.getResponse().getStatusCode());
        assertEquals("prefix" + large + "tail", String.join("", received));
    }

    private AiProxyTransport rateLimited(final Flux<byte[]> body) {
        return new AiProxyTransport() {
            @Override
            public Mono<AiUpstreamResponse> execute(final AiUpstreamRequest request) {
                throw new AssertionError("converted transport invoked");
            }

            @Override
            public Mono<AiUpstreamResponse> prepareStreaming(final AiUpstreamRequest request, final Flux<DataBuffer> upload) {
                sent.add(request);
                return upload.doOnNext(DataBufferUtils::release).then().thenReturn(new AiUpstreamResponse(429,
                        Map.of("Retry-After", List.of("2"), "X-Vendor", List.of("one", "two")), body));
            }
        };
    }

    @Test
    void cancellingDuringRetryAfterDoesNotStartAnotherCall() {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/native/v1/messages").body("body"));
        AiProxyTransport transport = new AiProxyTransport() {
            @Override
            public Mono<AiUpstreamResponse> execute(final AiUpstreamRequest request) {
                throw new AssertionError("converted transport invoked");
            }

            @Override
            public Mono<AiUpstreamResponse> prepareStreaming(final AiUpstreamRequest request, final Flux<DataBuffer> upload) {
                sent.add(request);
                return upload.doOnNext(DataBufferUtils::release).then(Mono.just(new AiUpstreamResponse(429, Map.of("Retry-After", List.of("10")), Flux.empty())));
            }
        };
        StepVerifier.withVirtualTime(() -> plugin(transport).doExecute(exchange, unused -> Mono.empty(), selector, new RuleData()))
                .expectSubscription().expectNoEvent(Duration.ofSeconds(9)).thenCancel().verify();
        assertEquals(1, sent.size());
        assertEquals(1, events.size());
    }

    @ParameterizedTest
    @CsvSource({"503,0,true,1", "429,3600,true,1", "429,invalid,true,1", "429,0,false,1", "429,0,true,2"})
    void preservesLastVendorErrorUnderReplayLimits(final int status, final String retryAfter, final boolean completeUpload, final int attempts) {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/native/v1/messages").body("payload"));
        AiProxyTransport transport = new AiProxyTransport() {
            @Override
            public Mono<AiUpstreamResponse> execute(final AiUpstreamRequest request) {
                throw new AssertionError("converted transport invoked");
            }

            @Override
            public Mono<AiUpstreamResponse> prepareStreaming(final AiUpstreamRequest request, final Flux<DataBuffer> upload) {
                sent.add(request);
                Flux<DataBuffer> consumed = completeUpload ? upload : upload.take(1);
                return consumed.doOnNext(DataBufferUtils::release).then(Mono.just(new AiUpstreamResponse(status, Map.of("Retry-After", List.of(retryAfter)),
                        Flux.just(("error " + sent.size()).getBytes(StandardCharsets.UTF_8)))));
            }
        };
        StepVerifier.create(plugin(transport).doExecute(exchange, unused -> Mono.empty(), selector, new RuleData())).verifyComplete();
        assertEquals(attempts, sent.size());
        assertEquals(status, exchange.getResponse().getStatusCode().value());
        assertEquals("error " + attempts, exchange.getResponse().getBodyAsString().block());
    }

    @ParameterizedTest
    @CsvSource({"/native-tools/v1,false,400", "/native/v1/messages,true,503", "/native/v1/messages,false,502"})
    void rejectsBeforeUploading(final String path, final boolean incompatible, final int status) {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post(path)
                .body(Flux.error(new AssertionError("upload subscribed"))));
        if (incompatible) {
            AiPassthroughPolicy.markIncompatible(exchange);
        }
        AiProxyPlugin plugin = plugin(request -> Mono.error(new AssertionError("upstream contacted")));
        StepVerifier.create(plugin.doExecute(exchange, unused -> Mono.empty(), selector, new RuleData())).verifyComplete();
        assertEquals(status, exchange.getResponse().getStatusCode().value());
        assertEquals(0, events.get(0).attemptCount());
    }

    @Test
    void declaredUploadOverflowRecordsNoVendorAttempt() {
        final WebClient client = WebClient.builder().exchangeFunction(request -> Mono.error(new AssertionError("vendor contacted"))).build();
        final AiProxyTransportProperties properties = new AiProxyTransportProperties();
        properties.setMaxRequestBytes(16);
        final MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/native/v1/messages")
                .contentLength(17).body(Flux.error(new AssertionError("upload subscribed"))));
        final AiProxyPlugin plugin = plugin(new WebClientAiProxyTransport(client, properties, client));

        StepVerifier.create(plugin.doExecute(exchange, unused -> Mono.empty(), selector, new RuleData())).verifyComplete();
        assertEquals(HttpStatus.PAYLOAD_TOO_LARGE, exchange.getResponse().getStatusCode());
        assertEquals(0, events.get(0).attemptCount());
    }

    @Test
    void recordsResponsesUsageFromFragmentedSseWithoutRewritingTheBody() {
        String first = "event: response.completed\ndata: {\"response\":{\"usage\":{\"input_tokens\":3,";
        String last = "\"output_tokens\":2,\"total_tokens\":5}}}\n\n";
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.post("/native/v1/responses").body("ignored by observer"));
        AiProxyTransport transport = new AiProxyTransport() {
            @Override
            public Mono<AiUpstreamResponse> execute(final AiUpstreamRequest request) {
                throw new AssertionError("converted transport invoked");
            }

            @Override
            public Mono<AiUpstreamResponse> prepareStreaming(final AiUpstreamRequest request, final Flux<DataBuffer> upload) {
                return upload.doOnNext(DataBufferUtils::release).then(Mono.just(new AiUpstreamResponse(200,
                        Map.of(HttpHeaders.CONTENT_TYPE, List.of("text/event-stream")), Flux.just(first, last).map(value -> value.getBytes(StandardCharsets.UTF_8)))));
            }
        };
        StepVerifier.create(plugin(transport).doExecute(exchange, unused -> Mono.empty(), selector, new RuleData())).verifyComplete();
        assertEquals(first + last, exchange.getResponse().getBodyAsString().block());
        assertEquals(1, events.get(0).attemptCount());
        assertEquals(new AiUsage(3L, 2L, 5L, AiUsage.Status.REPORTED), events.get(0).usage());
    }

    @ParameterizedTest
    @CsvSource({
        "/native,https://vendor.example:9443/api/",
        "/native/native/v1,https://vendor.example:9443/api/native/v1",
        "/native//more/parts,https://vendor.example:9443/api//more/parts"
    })
    void removesOnePrefixAndKeepsTheConfiguredAuthority(final String path, final String target) {
        AiPassthroughRequest request = AiPassthroughRequest.from(MockServerHttpRequest.method(HttpMethod.PUT, URI.create(path)).build(), AiProxyHandle.fromSelectorHandle(HANDLE));
        assertEquals(target, request.upstream().getUri().toString());
        assertEquals("PUT", request.upstream().getMethod());
    }

    @ParameterizedTest
    @CsvSource({"CONNECT,/native/v1,false", "OPTIONS,*,false", "GET,/native/v1,true"})
    void rejectsUnsupportedRequestTargetsBeforeOpeningAConnection(final String method, final String target, final boolean upgrade) {
        MockServerHttpRequest.BodyBuilder request = MockServerHttpRequest.method(HttpMethod.valueOf(method), URI.create(target));
        if (upgrade) {
            request.header(HttpHeaders.UPGRADE, "websocket");
        }
        MockServerWebExchange exchange = MockServerWebExchange.from(request);
        AiProxyPlugin plugin = plugin(ignored -> Mono.error(new AssertionError("upstream contacted")));
        StepVerifier.create(plugin.doExecute(exchange, unused -> Mono.empty(), selector, new RuleData())).verifyComplete();
        assertEquals(HttpStatus.BAD_REQUEST, exchange.getResponse().getStatusCode());
        assertEquals(0, events.get(0).attemptCount());
    }

    @ParameterizedTest
    @CsvSource({
        "/v1/chat/completions,prompt_tokens,completion_tokens,REPORTED",
        "/v1/responses,input_tokens,output_tokens,REPORTED",
        "/v1/messages,input_tokens,output_tokens,UNSUPPORTED",
        "/v1beta/models/gemini:generateContent,input_tokens,output_tokens,UNSUPPORTED"
    })
    void forwardsCompleteVendorResponseAndMetersOnlyKnownPaths(final String path, final String input, final String output, final AiUsage.Status expected) {
        String body = "{ \"usage\" : {\"" + input + "\":1,\"" + output + "\":2,\"total_tokens\":3} }";
        selector.setHandle("{\"baseUrl\":\"https://vendor.example\",\"mode\":\"passthrough\"}");
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get(path)
                .header(HttpHeaders.AUTHORIZATION, "Bearer caller").header("x-goog-api-key", "caller-google").build());
        AiProxyTransport transport = new AiProxyTransport() {
            @Override
            public Mono<AiUpstreamResponse> execute(final AiUpstreamRequest request) {
                throw new AssertionError("converted transport invoked");
            }

            @Override
            public Mono<AiUpstreamResponse> prepareStreaming(final AiUpstreamRequest request, final Flux<DataBuffer> upload) {
                assertEquals("GET", request.getMethod());
                assertEquals(path, request.getUri().getRawPath());
                assertEquals(List.of("Bearer caller"), request.getHeaders().get(HttpHeaders.AUTHORIZATION));
                assertEquals(List.of("caller-google"), request.getHeaders().get("x-goog-api-key"));
                return upload.then(Mono.just(new AiUpstreamResponse(200, Map.of(HttpHeaders.CONTENT_TYPE, List.of("application/json")),
                        Flux.just(body.getBytes(StandardCharsets.UTF_8)))));
            }
        };
        StepVerifier.create(plugin(transport).doExecute(exchange, unused -> Mono.empty(), selector, new RuleData())).verifyComplete();
        assertEquals(body, exchange.getResponse().getBodyAsString().block());
        AiUsage usage = expected == AiUsage.Status.REPORTED ? new AiUsage(1L, 2L, 3L, expected) : new AiUsage(null, null, null, expected);
        assertEquals(usage, events.get(0).usage());
    }
}
