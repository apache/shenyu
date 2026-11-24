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

package org.apache.shenyu.springboot.starter.plugin.httpclient;

import io.netty.buffer.ByteBuf;
import io.netty.buffer.Unpooled;
import io.netty.channel.Channel;
import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.ChannelOutboundHandlerAdapter;
import io.netty.channel.ChannelPromise;
import io.netty.channel.EventLoopGroup;
import io.netty.channel.local.LocalAddress;
import io.netty.channel.local.LocalChannel;
import io.netty.channel.local.LocalServerChannel;
import io.netty.channel.nio.NioEventLoopGroup;
import io.netty.channel.socket.ServerSocketChannel;
import io.netty.channel.socket.SocketChannel;
import io.netty.handler.codec.http.HttpHeaderNames;
import io.netty.handler.codec.http.HttpObjectAggregator;
import io.netty.handler.timeout.ReadTimeoutException;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.exception.StreamIdleTimeoutException;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.utils.HttpRouteDeadline;
import org.apache.shenyu.plugin.httpclient.NettyHttpClientPlugin;
import org.apache.shenyu.plugin.httpclient.ResponseBodyIdleTimeout;
import org.apache.shenyu.plugin.httpclient.WebClientPlugin;
import org.apache.shenyu.plugin.httpclient.config.HttpClientProperties;
import org.apache.shenyu.plugin.httpclient.config.UpstreamPoolGroupRegistry;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.reactivestreams.Publisher;
import org.springframework.boot.autoconfigure.web.ServerProperties;
import org.springframework.boot.test.system.CapturedOutput;
import org.springframework.boot.test.system.OutputCaptureExtension;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.core.io.buffer.NettyDataBuffer;
import org.springframework.core.io.buffer.NettyDataBufferFactory;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.http.client.reactive.ReactorClientHttpConnector;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.reactive.function.client.WebClient;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.ResponseStatusException;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;
import reactor.netty.Connection;
import reactor.netty.DisposableServer;
import reactor.netty.http.HttpProtocol;
import reactor.netty.http.client.HttpClient;
import reactor.netty.http.server.HttpServer;
import reactor.netty.http.server.HttpServerRequest;
import reactor.netty.http.server.HttpServerResponse;
import reactor.netty.resources.LoopResources;

import java.net.SocketAddress;
import java.net.SocketException;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.BiFunction;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class HttpClientStreamIdleTimeoutTest {

    private static final Duration TEST_TIMEOUT = Duration.ofSeconds(15);

    private static final String BASE_URL = "http://127.0.0.1";

    private final LocalLoops loops = new LocalLoops();

    private DisposableServer server;

    private HttpClient defaultClient;

    private UpstreamPoolGroupRegistry groups;

    @AfterEach
    void close() {
        if (Objects.nonNull(groups)) {
            groups.close();
        }
        if (Objects.nonNull(defaultClient)) {
            defaultClient.configuration().connectionProvider().disposeLater().block(TEST_TIMEOUT);
        }
        if (Objects.nonNull(server)) {
            server.disposeNow();
        }
        loops.eventLoops.shutdownGracefully(0, 1, TimeUnit.SECONDS).syncUninterruptibly();
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void overridesBothGlobalTimersForOneBodyAndRestoresThemOnReuse(final boolean webClient) {
        Set<Channel> channels = ConcurrentHashMap.newKeySet();
        start(HttpProtocol.HTTP11, (request, response) -> {
            request.withConnection(connection -> channels.add(connection.channel()));
            return switch (request.uri()) {
                case "/slow" -> stream(response, Mono.delay(Duration.ofSeconds(5)).map(tick -> "payload"));
                case "/legacy" -> stream(response, Mono.never());
                case "/empty" -> response.status(204).send();
                default -> response.sendString(Mono.just("ok"));
            };
        });
        ShenyuPlugin plugin = plugin(webClient);

        assertEquals("ok", request(plugin, webClient, "/fast", null).block(TEST_TIMEOUT));
        assertEquals("", request(plugin, webClient, "/head", 10000L).block(TEST_TIMEOUT));
        assertEquals("", request(plugin, webClient, "/empty", 10000L).block(TEST_TIMEOUT));
        assertEquals("payload", request(plugin, webClient, "/slow", 10000L).block(TEST_TIMEOUT));
        Throwable legacyTimeout = assertThrows(RuntimeException.class, () -> request(plugin, webClient, "/legacy", null).block(TEST_TIMEOUT));
        assertTrue(hasCause(legacyTimeout, ReadTimeoutException.class), legacyTimeout.toString());
        assertEquals(1, channels.size(), "all five requests must use the same physical connection");
    }

    @ParameterizedTest
    @CsvSource({"false,false", "true,false", "false,true", "true,true"})
    @ExtendWith(OutputCaptureExtension.class)
    void idleStopsUpstreamWithoutRetryAndLogsOnce(final boolean webClient, final boolean committed, final CapturedOutput logs) throws Exception {
        AtomicInteger attempts = new AtomicInteger();
        CompletableFuture<Void> upstreamClosed = new CompletableFuture<>();
        start(HttpProtocol.HTTP11, (request, response) -> {
            attempts.incrementAndGet();
            request.withConnection(connection -> connection.channel().closeFuture().addListener(done -> upstreamClosed.complete(null)));
            return committed ? sendFirstChunk(response) : stream(response, Mono.never());
        });
        ShenyuPlugin plugin = plugin(webClient);
        MockServerWebExchange exchange = exchange(webClient, "/stalled", 400L);
        exchange.getAttributes().put(Constants.HTTP_RETRY, 2);
        AtomicBoolean writerEntered = new AtomicBoolean();
        AtomicReference<ByteBuf> firstChunk = new AtomicReference<>();
        StringBuilder downstream = new StringBuilder();
        exchange.getResponse().setWriteHandler(chunks -> chunks.doOnNext(buffer -> {
            firstChunk.set(((NettyDataBuffer) buffer).getNativeBuffer());
            downstream.append(readAndRelease(buffer));
        }).then());
        Mono<Void> transfer = plugin.execute(exchange, current -> {
            writerEntered.set(true);
            return committed ? current.getResponse().writeWith(body(current)) : Mono.never();
        });
        if (committed) {
            assertThrows(StreamIdleTimeoutException.class, () -> transfer.block(TEST_TIMEOUT));
            assertTrue(exchange.getResponse().isCommitted());
            assertEquals(HttpStatus.OK, exchange.getResponse().getStatusCode());
            assertEquals("first", downstream.toString());
            assertEquals(0, firstChunk.get().refCnt());
        } else {
            ResponseStatusException error = assertThrows(ResponseStatusException.class, () -> transfer.block(TEST_TIMEOUT));
            assertTrue(hasCause(error, StreamIdleTimeoutException.class));
            assertEquals(HttpStatus.GATEWAY_TIMEOUT, error.getStatusCode());
        }
        assertTrue(writerEntered.get());
        upstreamClosed.get(5, TimeUnit.SECONDS);
        assertEquals(1, attempts.get());
        assertEquals(1L, logs.getAll().lines()
                .filter(line -> line.contains(exchange.getLogPrefix().trim()) && line.contains("Upstream response body idle timeout")).count());
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    @ExtendWith(OutputCaptureExtension.class)
    void downstreamCancelClosesUpstreamWhileTheWriterIsPaused(final boolean webClient, final CapturedOutput logs) throws Exception {
        CompletableFuture<Void> closed = new CompletableFuture<>();
        start(HttpProtocol.HTTP11, (request, response) -> {
            request.withConnection(connection -> connection.channel().closeFuture().addListener(done -> closed.complete(null)));
            return sendFirstChunk(response);
        });
        ShenyuPlugin plugin = plugin(webClient);
        CompletableFuture<Void> writerEntered = new CompletableFuture<>();
        Mono<Void> result = plugin.execute(exchange(webClient, "/cancel", 30000L), current -> {
            writerEntered.complete(null);
            return Mono.never();
        });
        CompletableFuture<Void> downstream = result.toFuture();
        writerEntered.get(5, TimeUnit.SECONDS);
        assertTrue(downstream.cancel(false));
        closed.get(5, TimeUnit.SECONDS);
        // Cancellation can schedule a body subscription back onto the channel event loop.
        loops.eventLoops.next().submit(() -> { }).sync();
        assertFalse(logs.getAll().contains("due to cancellation."));
        assertFalse(logs.getAll().contains("Rejecting additional inbound receiver"));
    }

    @Test
    void webClientAcceptsReplacementOperationAfterTransportResetBeforeSend() {
        start(HttpProtocol.HTTP11, (request, response) -> response.sendString(Mono.just("second connection")));
        defaultClient = new HttpClientFactory(new HttpClientProperties(), loops, new ServerProperties()).createInstance();
        ReactorClientHttpConnector connector = new ReactorClientHttpConnector(defaultClient);
        Set<Channel> channels = ConcurrentHashMap.newKeySet();
        WebClient client = WebClient.builder().clientConnector((method, uri, callback) -> connector.connect(method, uri, request -> {
            // Let Spring attach the operation owner, then simulate a transport reset before any request bytes are sent.
            Mono<Void> send = callback.apply(request);
            Connection operation = request.getNativeRequest();
            channels.add(operation.channel());
            return channels.size() == 1 ? Mono.error(new SocketException("Connection reset by peer")) : send;
        })).build();
        ShenyuPlugin plugin = new WebClientPlugin(client, Constants.BYTES_PER_MB);
        assertEquals("second connection", request(plugin, false, "/retry", 10000L).block(TEST_TIMEOUT));
        assertEquals(2, channels.size());
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void eachHeaderTimeoutStillAppliesToAnOptedInRequest(final boolean responseTimeout) {
        start(HttpProtocol.HTTP11, (request, response) -> Mono.never());
        HttpClientProperties properties = new HttpClientProperties();
        properties.setResponseTimeout(responseTimeout ? 400L : 0L);
        properties.setReadTimeout(responseTimeout ? 0 : 400);
        defaultClient = new HttpClientFactory(properties, loops, new ServerProperties()).createInstance();
        Mono<String> transfer = defaultClient.get().uri(BASE_URL + "/no-headers")
                .responseContent().aggregate().asString()
                .contextWrite(context -> context.put(ResponseBodyIdleTimeout.class, new ResponseBodyIdleTimeout(10000L)));
        assertThrows(ReadTimeoutException.class, () -> transfer.block(TEST_TIMEOUT));
    }

    @Test
    void earlyResponseHeadersKeepTheBodyPolicyWhenTheUploadFinishes() {
        Sinks.One<Void> finishUpload = Sinks.one();
        Sinks.One<Void> uploadSent = Sinks.one();
        start(HttpProtocol.HTTP11, (request, response) -> stream(response,
                uploadSent.asMono().then(Mono.delay(Duration.ofMillis(900))).thenReturn("early response")));
        HttpClientProperties properties = new HttpClientProperties();
        properties.setResponseTimeout(300L);
        AtomicBoolean headersBeforeUploadSent = new AtomicBoolean();
        defaultClient = new HttpClientFactory(properties, loops, new ServerProperties()).createInstance()
                .doOnRequest((request, connection) -> connection.channel().config().setAutoRead(true))
                .doAfterRequest((request, connection) -> uploadSent.tryEmitEmpty())
                .doOnResponse((response, connection) -> {
                    headersBeforeUploadSent.set(true);
                    finishUpload.tryEmitEmpty();
                });
        Flux<ByteBuf> upload = Flux.just(Unpooled.wrappedBuffer(new byte[]{1})).concatWith(finishUpload.asMono().then(Mono.empty()));
        String result = defaultClient.post().uri(BASE_URL + "/upload")
                .send((request, outbound) -> outbound.send(upload, chunk -> true))
                .responseContent().aggregate().asString()
                .contextWrite(context -> context.put(ResponseBodyIdleTimeout.class, new ResponseBodyIdleTimeout(5000L)))
                .block(TEST_TIMEOUT);
        assertTrue(headersBeforeUploadSent.get());
        assertEquals("early response", result);
    }

    @Test
    void fullHttpResponseRemovesTimerBeforeNextAcquisition() {
        Set<Channel> channels = ConcurrentHashMap.newKeySet();
        start(HttpProtocol.HTTP11, (request, response) -> response.sendString(Mono.just("full")));
        HttpClientProperties properties = new HttpClientProperties();
        properties.getPool().setMaxConnections(1);
        AtomicInteger acquisitionsWithBodyTimer = new AtomicInteger();
        defaultClient = new HttpClientFactory(properties, loops, new ServerProperties()).createInstance()
                .doOnConnected(connection -> connection.addHandlerLast("aggregate", new HttpObjectAggregator(1024)))
                .doOnRequest((request, connection) -> {
                    channels.add(connection.channel());
                    if (Objects.nonNull(connection.channel().pipeline().get(ResponseBodyIdleTimeoutHandler.class))) {
                        acquisitionsWithBodyTimer.incrementAndGet();
                    }
                });
        Mono<String> transfer = defaultClient.get().uri(BASE_URL + "/full").responseContent().aggregate().asString();
        assertEquals("full", transfer.contextWrite(context -> context.put(ResponseBodyIdleTimeout.class, new ResponseBodyIdleTimeout(1000L))).block(TEST_TIMEOUT));
        assertEquals("full", transfer.block(TEST_TIMEOUT));
        assertEquals(1, channels.size());
        assertEquals(0, acquisitionsWithBodyTimer.get());
    }

    private ShenyuPlugin plugin(final boolean webClient) {
        HttpClientProperties properties = new HttpClientProperties();
        properties.setStrategy(webClient ? "webClient" : "netty");
        properties.getPool().setMaxConnections(1);
        HttpClientProperties.Pool group = new HttpClientProperties.Pool();
        group.setName("bulk");
        group.setMaxConnections(1);
        properties.getPoolGroups().add(group);
        HttpClientFactory factory = new HttpClientFactory(properties, loops, new ServerProperties());
        groups = factory.buildGroupRegistry();
        defaultClient = factory.createInstance();
        return webClient
                ? new WebClientPlugin(WebClient.builder().clientConnector(new ReactorClientHttpConnector(defaultClient)).build(), Constants.BYTES_PER_MB, groups)
                : new NettyHttpClientPlugin(defaultClient, Constants.BYTES_PER_MB, groups);
    }

    private Mono<String> request(final ShenyuPlugin plugin, final boolean usePoolGroup, final String path, final Long idleTimeout) {
        MockServerWebExchange exchange = exchange(usePoolGroup, path, idleTimeout);
        StringBuilder received = new StringBuilder();
        return plugin.execute(exchange, current -> body(current).map(this::readAndRelease).doOnNext(received::append).then())
                .then(Mono.fromSupplier(received::toString));
    }

    private MockServerWebExchange exchange(final boolean usePoolGroup, final String path, final Long idleTimeout) {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.method("/head".equals(path) ? HttpMethod.HEAD : HttpMethod.GET, path));
        exchange.getAttributes().put(Constants.CONTEXT, new ShenyuContext());
        exchange.getAttributes().put(Constants.HTTP_URI, URI.create(BASE_URL + path));
        if (usePoolGroup) {
            exchange.getAttributes().put(Constants.HTTP_POOL_GROUP, "bulk");
        }
        if (Objects.nonNull(idleTimeout)) {
            exchange.getAttributes().put(Constants.STREAM_IDLE_TIMEOUT, idleTimeout);
        }
        return exchange;
    }

    private Flux<DataBuffer> body(final ServerWebExchange exchange) {
        Connection connection = exchange.getAttribute(Constants.CLIENT_RESPONSE_CONN_ATTR);
        if (Objects.nonNull(connection)) {
            NettyDataBufferFactory factory = new NettyDataBufferFactory(connection.channel().alloc());
            Flux<DataBuffer> content = connection.inbound().receive().retain().<DataBuffer>map(factory::wrap)
                    .doOnDiscard(NettyDataBuffer.class, DataBufferUtils::release);
            HttpRouteDeadline attempt = exchange.getAttribute(HttpRouteDeadline.ATTEMPT);
            return Objects.isNull(attempt) ? content : content.doOnComplete(attempt::received);
        }
        ResponseEntity<Flux<DataBuffer>> entity = exchange.getRequiredAttribute(Constants.CLIENT_RESPONSE_ATTR);
        return Objects.requireNonNull(entity.getBody());
    }

    private String readAndRelease(final DataBuffer buffer) {
        try {
            return buffer.toString(StandardCharsets.UTF_8);
        } finally {
            DataBufferUtils.release(buffer);
        }
    }

    private Mono<Void> sendFirstChunk(final HttpServerResponse response) {
        return response.header(HttpHeaderNames.CONTENT_TYPE, "text/plain")
                .sendGroups(Flux.just(Mono.just(Unpooled.copiedBuffer("first", StandardCharsets.UTF_8))).concatWith(Mono.never())).then();
    }

    @ParameterizedTest
    @CsvSource({"1500,0,false", "0,1500,false", "6000,0,true"})
    void http2TimeoutClosesOnlyTheAffectedStream(final int readTimeout, final long responseTimeout, final boolean ruleExpires) throws Exception {
        Sinks.One<String> release = Sinks.one();
        start(HttpProtocol.H2C, (request, response) -> stream(response, "/survivor".equals(request.uri()) ? release.asMono() : Mono.never()));
        defaultClient = http2Client(readTimeout, responseTimeout);
        CompletableFuture<Channel> survivorHeaders = new CompletableFuture<>();
        CompletableFuture<Channel> stalledHeaders = new CompletableFuture<>();
        final CompletableFuture<String> survivor = streamRequest("/survivor", ruleExpires ? null : 10000L, survivorHeaders);
        Channel survivorChannel = survivorHeaders.get(5, TimeUnit.SECONDS);
        CompletableFuture<String> stalled = streamRequest("/stalled", ruleExpires ? 400L : null, stalledHeaders);
        Channel stalledChannel = stalledHeaders.get(5, TimeUnit.SECONDS);

        assertNotSame(survivorChannel, stalledChannel);
        assertSame(survivorChannel.parent(), stalledChannel.parent());
        ExecutionException failure = assertThrows(ExecutionException.class, () -> stalled.get(5, TimeUnit.SECONDS));
        assertTrue(hasCause(failure, ruleExpires ? StreamIdleTimeoutException.class : ReadTimeoutException.class), failure.toString());
        assertFalse(survivor.isDone());
        assertTrue(survivorChannel.parent().isActive());
        release.tryEmitValue("survived");
        assertEquals("survived", survivor.get(5, TimeUnit.SECONDS));
    }

    @Test
    void dataOnProtectedStreamKeepsAQuietSiblingAliveDuringParentReadInterval() throws Exception {
        Sinks.One<String> finishQuietBody = Sinks.one();
        start(HttpProtocol.H2C, (request, response) -> {
            if ("/quiet".equals(request.uri())) {
                return stream(response, finishQuietBody.asMono());
            }
            return response.sendGroups(Flux.interval(Duration.ofMillis(150)).take(12)
                    .map(tick -> Mono.fromSupplier(() -> response.alloc().buffer(1).writeByte('x'))));
        });
        defaultClient = http2Client(900, 0);
        CompletableFuture<Channel> activeHeaders = new CompletableFuture<>();
        CompletableFuture<Channel> quietHeaders = new CompletableFuture<>();
        CompletableFuture<String> active = streamRequest("/active", 10000L, activeHeaders);
        Channel activeChannel = activeHeaders.get(5, TimeUnit.SECONDS);
        final CompletableFuture<String> quiet = streamRequest("/quiet", null, quietHeaders);
        assertSame(activeChannel.parent(), quietHeaders.get(5, TimeUnit.SECONDS).parent());

        assertEquals("xxxxxxxxxxxx", active.get(5, TimeUnit.SECONDS));
        finishQuietBody.tryEmitValue("ok");
        assertEquals("ok", quiet.get(5, TimeUnit.SECONDS));
    }

    @Test
    void http2UpstreamResetDropsProtectionAndRestoresNormalParentExpiry() throws Exception {
        CompletableFuture<Channel> serverStream = new CompletableFuture<>();
        start(HttpProtocol.H2C, (request, response) -> {
            if ("/reset".equals(request.uri())) {
                request.withConnection(connection -> serverStream.complete(connection.channel()));
            }
            return stream(response, Mono.never());
        });
        defaultClient = http2Client(1200, 0);
        CompletableFuture<Channel> optedHeaders = new CompletableFuture<>();
        CompletableFuture<String> opted = streamRequest("/reset", 10000L, optedHeaders);
        Channel optedChannel = optedHeaders.get(5, TimeUnit.SECONDS);
        CompletableFuture<Channel> legacyHeaders = new CompletableFuture<>();
        final CompletableFuture<String> legacy = streamRequest("/quiet", null, legacyHeaders);
        assertSame(optedChannel.parent(), legacyHeaders.get(5, TimeUnit.SECONDS).parent());

        serverStream.get(5, TimeUnit.SECONDS).close().sync();
        ExecutionException reset = assertThrows(ExecutionException.class, () -> opted.get(5, TimeUnit.SECONDS));
        assertFalse(hasCause(reset, StreamIdleTimeoutException.class));
        assertThrows(ExecutionException.class, () -> legacy.get(5, TimeUnit.SECONDS));
        assertTrue(optedChannel.parent().closeFuture().await(5, TimeUnit.SECONDS));
    }

    private HttpClient http2Client(final int readTimeout, final long responseTimeout) {
        HttpClientProperties properties = new HttpClientProperties();
        properties.setResponseTimeout(responseTimeout);
        properties.setReadTimeout(readTimeout);
        properties.getPool().setMaxConnections(1);
        return new HttpClientFactory(properties, loops, new ServerProperties()).createInstance().protocol(HttpProtocol.H2C);
    }

    private CompletableFuture<String> streamRequest(final String path, final Long idleTimeout, final CompletableFuture<Channel> headers) {
        Mono<String> transfer = defaultClient.get().uri(BASE_URL + path).responseConnection((response, connection) -> {
            headers.complete(connection.channel());
            return connection.inbound().receive().aggregate().asString();
        }).single();
        if (Objects.nonNull(idleTimeout)) {
            transfer = transfer.contextWrite(context -> context.put(ResponseBodyIdleTimeout.class, new ResponseBodyIdleTimeout(idleTimeout)));
        }
        return transfer.toFuture();
    }

    private void start(final HttpProtocol protocol, final BiFunction<HttpServerRequest, HttpServerResponse, Publisher<Void>> handler) {
        server = HttpServer.create().bindAddress(() -> loops.address).runOn(loops, false).protocol(protocol).handle(handler).bindNow();
    }

    private static Mono<Void> stream(final HttpServerResponse response, final Mono<String> payload) {
        return response.header(HttpHeaderNames.CONTENT_TYPE, "text/plain").chunkedTransfer(true).sendHeaders().then()
                .then(Mono.defer(() -> response.sendString(payload.flux().hide()).then()));
    }

    private static boolean hasCause(final Throwable error, final Class<? extends Throwable> expected) {
        for (Throwable cause = error; Objects.nonNull(cause); cause = cause.getCause()) {
            if (expected.isInstance(cause)) {
                return true;
            }
        }
        return false;
    }

    /** Routes ordinary HTTP URIs over Netty's in-memory transport while retaining the production pool and HTTP codecs. */
    private static final class LocalLoops implements LoopResources {

        private final EventLoopGroup eventLoops = new NioEventLoopGroup(1);

        private final LocalAddress address = new LocalAddress(UUID.randomUUID().toString());

        @Override
        public EventLoopGroup onServer(final boolean useNative) {
            return eventLoops;
        }

        @Override
        @SuppressWarnings("unchecked")
        public <C extends Channel> C onChannel(final Class<C> channelType, final EventLoopGroup group) {
            if (ServerSocketChannel.class.equals(channelType)) {
                return (C) new LocalServerChannel();
            }
            if (SocketChannel.class.equals(channelType)) {
                LocalChannel channel = new LocalChannel();
                channel.pipeline().addLast(new ChannelOutboundHandlerAdapter() {
                    @Override
                    public void connect(final ChannelHandlerContext context, final SocketAddress remote, final SocketAddress local, final ChannelPromise promise) {
                        context.connect(address, local, promise);
                    }
                });
                return (C) channel;
            }
            return LoopResources.super.onChannel(channelType, group);
        }
    }
}
