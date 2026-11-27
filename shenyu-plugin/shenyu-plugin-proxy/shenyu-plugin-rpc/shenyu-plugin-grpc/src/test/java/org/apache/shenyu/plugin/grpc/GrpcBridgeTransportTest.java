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

package org.apache.shenyu.plugin.grpc;

import com.google.protobuf.DynamicMessage;
import io.grpc.ManagedChannel;
import io.grpc.MethodDescriptor;
import io.grpc.Server;
import io.grpc.ServerCallHandler;
import io.grpc.ServerServiceDefinition;
import io.grpc.Status;
import io.grpc.netty.NettyChannelBuilder;
import io.grpc.netty.NettyServerBuilder;
import io.grpc.stub.ServerCallStreamObserver;
import io.grpc.stub.ServerCalls;
import io.grpc.stub.StreamObserver;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.plugin.GrpcRegisterConfig;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.grpc.cache.GrpcClientCache;
import org.apache.shenyu.plugin.grpc.client.ShenyuGrpcClient;
import org.apache.shenyu.plugin.grpc.exception.ShenyuGrpcException;
import org.apache.shenyu.plugin.grpc.handler.GrpcPluginDataHandler;
import org.apache.shenyu.protocol.grpc.message.JsonMessage;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.HttpStatus;
import org.springframework.http.server.reactive.ReactorHttpHandlerAdapter;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.adapter.WebHttpHandlerBuilder;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.netty.ByteBufFlux;
import reactor.netty.DisposableServer;
import reactor.netty.http.client.HttpClient;
import reactor.netty.http.server.HttpServer;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Exercises the gRPC plugin between real loopback HTTP and gRPC transports.
 */
class GrpcBridgeTransportTest {

    private static final String LOOPBACK = "127.0.0.1";

    private static final String SELECTOR_ID = "grpc-bridge-transport-test";

    private static final String SERVICE = "bridge.TransportTest";

    private static final String FIRST = "{\"value\":\"one\"}";

    private static final String SECOND = "{\"value\":\"two\"}";

    private static final String REQUEST = "{\"data\":[" + FIRST + "," + SECOND + "]}";

    private static final Duration WAIT = Duration.ofSeconds(10);

    private static final int NORMAL_DEADLINE_MILLIS = 30_000;

    private final GrpcPlugin plugin = new GrpcPlugin();

    private final CountDownLatch upstreamStarted = new CountDownLatch(1);

    private final CountDownLatch upstreamCancelled = new CountDownLatch(1);

    private final AtomicReference<Throwable> failure = new AtomicReference<>();

    private Function<ServerWebExchange, Mono<Void>> responseWriter = exchange -> exchange.getResponse().writeWith(Mono.just(resultBuffer(exchange)));

    private Server upstream;

    private ManagedChannel channel;

    private DisposableServer gateway;

    private GrpcRegisterConfig originalConfig;

    @BeforeEach
    void saveBudget() {
        originalConfig = Singleton.INST.get(GrpcRegisterConfig.class);
        Singleton.INST.remove(GrpcRegisterConfig.class);
    }

    @AfterEach
    void closeTransport() throws InterruptedException {
        try {
            if (Objects.nonNull(gateway)) {
                gateway.disposeNow(WAIT);
            }
        } finally {
            GrpcClientCache.removeClient(SELECTOR_ID);
            Singleton.INST.remove(GrpcRegisterConfig.class);
            if (Objects.nonNull(originalConfig)) {
                Singleton.INST.single(GrpcRegisterConfig.class, originalConfig);
            }
            shutdownGrpcTransport();
        }
    }

    private void shutdownGrpcTransport() throws InterruptedException {
        if (Objects.nonNull(channel)) {
            channel.shutdownNow();
        }
        if (Objects.nonNull(upstream)) {
            upstream.shutdownNow();
            upstream.awaitTermination(WAIT.toMillis(), TimeUnit.MILLISECONDS);
        }
        if (Objects.nonNull(channel)) {
            channel.awaitTermination(WAIT.toMillis(), TimeUnit.MILLISECONDS);
        }
    }

    @ParameterizedTest
    @EnumSource(value = MethodDescriptor.MethodType.class, names = {"UNARY", "SERVER_STREAMING", "CLIENT_STREAMING", "BIDI_STREAMING"})
    void aggregatesAllSupportedRpcModes(final MethodDescriptor.MethodType type) throws IOException {
        start(type, echo(type), NORMAL_DEADLINE_MILLIS);
        String expected = switch (type) {
            case UNARY -> "[" + FIRST + "]";
            case SERVER_STREAMING, BIDI_STREAMING -> "[" + FIRST + "," + SECOND + "]";
            case CLIENT_STREAMING -> "[{\"received\":\"2\"}]";
            default -> throw new IllegalArgumentException(type.name());
        };
        assertEquals(new BridgeResponse(200, expected), post(REQUEST).block(WAIT), () -> String.valueOf(failure.get()));
    }

    @Test
    void httpDisconnectCancelsPendingUpstreamCall() throws Exception {
        start(MethodDescriptor.MethodType.SERVER_STREAMING, ServerCalls.asyncServerStreamingCall(
                (request, response) -> keepPending(response)), NORMAL_DEADLINE_MILLIS);
        abortHttpRequestAfter(upstreamStarted);

        assertTrue(upstreamCancelled.await(WAIT.toMillis(), TimeUnit.MILLISECONDS), "gRPC server did not observe the HTTP disconnect");
    }

    @Test
    void httpDisconnectAfterAggregationCancelsTheResponseWriter() throws Exception {
        CountDownLatch writerPending = new CountDownLatch(1);
        CountDownLatch writerCancelled = new CountDownLatch(1);
        responseWriter = exchange -> exchange.getResponse().writeAndFlushWith(Flux.just(
                Mono.just(resultBuffer(exchange)),
                Mono.<DataBuffer>never().doOnSubscribe(ignored -> writerPending.countDown()).doOnCancel(writerCancelled::countDown)));
        start(MethodDescriptor.MethodType.SERVER_STREAMING, ServerCalls.asyncServerStreamingCall((request, response) -> {
            ((ServerCallStreamObserver<DynamicMessage>) response).setOnCancelHandler(upstreamCancelled::countDown);
            response.onNext(request);
            response.onCompleted();
        }), NORMAL_DEADLINE_MILLIS);

        abortHttpRequestAfter(writerPending);

        assertTrue(writerCancelled.await(WAIT.toMillis(), TimeUnit.MILLISECONDS));
        assertEquals(1L, upstreamCancelled.getCount(), "the completed upstream call must not be cancelled");
    }

    @Test
    void deadlineFailsWithoutReturningPartialResults() throws Exception {
        start(MethodDescriptor.MethodType.SERVER_STREAMING, ServerCalls.asyncServerStreamingCall(
                (request, response) -> keepPending(response)), 2_000);

        assertEquals(new BridgeResponse(500, ""), post(REQUEST).block(WAIT));
        assertEquals(Status.Code.DEADLINE_EXCEEDED, Status.fromThrowable(failure.get()).getCode());
        assertTrue(upstreamStarted.await(WAIT.toMillis(), TimeUnit.MILLISECONDS));
        assertTrue(upstreamCancelled.await(WAIT.toMillis(), TimeUnit.MILLISECONDS));
    }

    @Test
    void unlimitedBudgetSettingsRemainCompatibleAfterUpdates() throws IOException {
        MethodDescriptor.MethodType type = MethodDescriptor.MethodType.SERVER_STREAMING;
        start(type, echo(type), NORMAL_DEADLINE_MILLIS);
        BridgeResponse expected = new BridgeResponse(200, "[" + FIRST + "," + SECOND + "]");
        for (String config : List.of(
                "{}",
                "{\"maxAggregatedResultCount\":null,\"maxAggregatedResultBytes\":-1}",
                "{\"maxAggregatedResultCount\":-1,\"maxAggregatedResultBytes\":0}")) {
            updateBudget(config);
            assertEquals(expected, post(REQUEST).block(WAIT), () -> String.valueOf(failure.get()));
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"{\"maxAggregatedResultCount\":1}", "{\"maxAggregatedResultBytes\":2}"})
    void configuredBudgetCancelsOnlyTheOverflowingRpc(final String config) throws Exception {
        final CompletableFuture<StreamObserver<DynamicMessage>> concurrentResponse = new CompletableFuture<>();
        String concurrentPayload = "{\"value\":\"parallel\"}";
        start(MethodDescriptor.MethodType.SERVER_STREAMING, ServerCalls.asyncServerStreamingCall((request, response) -> {
            if (concurrentPayload.equals(JsonMessage.getDataFromDynamicMessage(request))) {
                concurrentResponse.complete(response);
            } else {
                keepPending(response);
                response.onNext(JsonMessage.buildJsonMessage(SECOND));
            }
        }), NORMAL_DEADLINE_MILLIS);
        updateBudget(config);

        final CompletableFuture<BridgeResponse> concurrent = post("{\"data\":[" + concurrentPayload + "]}").toFuture();
        final StreamObserver<DynamicMessage> other = concurrentResponse.get(WAIT.toMillis(), TimeUnit.MILLISECONDS);
        assertEquals(new BridgeResponse(500, ""), post(REQUEST).block(WAIT));
        ShenyuGrpcException overflow = assertInstanceOf(ShenyuGrpcException.class, failure.get().getCause());
        assertTrue(overflow.getMessage().contains("exceeded budget"));
        assertTrue(upstreamCancelled.await(WAIT.toMillis(), TimeUnit.MILLISECONDS));

        other.onNext(JsonMessage.buildJsonMessage("{}"));
        other.onCompleted();
        assertEquals(new BridgeResponse(200, "[{}]"), concurrent.get(WAIT.toMillis(), TimeUnit.MILLISECONDS));
    }

    private void keepPending(final StreamObserver<DynamicMessage> response) {
        ((ServerCallStreamObserver<DynamicMessage>) response).setOnCancelHandler(upstreamCancelled::countDown);
        response.onNext(JsonMessage.buildJsonMessage(FIRST));
        upstreamStarted.countDown();
    }

    private void abortHttpRequestAfter(final CountDownLatch ready) throws IOException, InterruptedException {
        try (Socket client = new Socket()) {
            client.connect(new InetSocketAddress(LOOPBACK, gateway.port()), (int) WAIT.toMillis());
            String request = "POST / HTTP/1.1\r\nHost: " + LOOPBACK + "\r\nContent-Length: " + REQUEST.length() + "\r\n\r\n" + REQUEST;
            client.getOutputStream().write(request.getBytes(StandardCharsets.US_ASCII));
            client.getOutputStream().flush();
            assertTrue(ready.await(WAIT.toMillis(), TimeUnit.MILLISECONDS), "the exchange never reached the expected stage");
            client.setSoLinger(true, 0);
        }
    }

    private void updateBudget(final String config) {
        PluginData pluginData = new PluginData();
        pluginData.setEnabled(true);
        pluginData.setConfig(config);
        new GrpcPluginDataHandler().handlerPlugin(pluginData);
    }

    private Mono<BridgeResponse> post(final String body) {
        return HttpClient.create().host(LOOPBACK).port(gateway.port()).post().uri("/")
                .send(ByteBufFlux.fromString(Mono.just(body)))
                .responseSingle((response, bytes) -> bytes.asString().defaultIfEmpty("")
                        .map(text -> new BridgeResponse(response.status().code(), text)));
    }

    private void start(final MethodDescriptor.MethodType type, final ServerCallHandler<DynamicMessage, DynamicMessage> handler,
                       final int deadlineMillis) throws IOException {
        String method = type.name();
        DynamicMessage message = JsonMessage.buildJsonMessage();
        MethodDescriptor<DynamicMessage, DynamicMessage> descriptor = JsonMessage.createJsonMarshallerMethodDescriptor(
                SERVICE, method, type, message, message);
        ServerServiceDefinition service = ServerServiceDefinition.builder(descriptor.getServiceName()).addMethod(descriptor, handler).build();
        upstream = NettyServerBuilder.forAddress(new InetSocketAddress(LOOPBACK, 0)).addService(service).build().start();
        channel = NettyChannelBuilder.forAddress(new InetSocketAddress(LOOPBACK, upstream.getPort())).usePlaintext().build();
        registerClient(new ShenyuGrpcClient(channel));

        MetaData metadata = MetaData.builder().id(SELECTOR_ID).serviceName(SERVICE).methodName(method)
                .rpcExt(GsonUtils.getInstance().toJson(Map.of("timeout", deadlineMillis, "methodType", method))).build();
        SelectorData selector = new SelectorData();
        selector.setId(SELECTOR_ID);
        ReactorHttpHandlerAdapter adapter = new ReactorHttpHandlerAdapter(WebHttpHandlerBuilder.webHandler(exchange ->
                        DataBufferUtils.join(exchange.getRequest().getBody())
                                .flatMap(body -> proxy(exchange, body, metadata, selector)))
                .exceptionHandler((exchange, error) -> {
                    failure.set(error);
                    exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
                    return exchange.getResponse().setComplete();
                }).build());
        gateway = HttpServer.create().host(LOOPBACK).port(0).handle(adapter).bindNow(WAIT);
    }

    private Mono<Void> proxy(final ServerWebExchange exchange, final DataBuffer body,
                             final MetaData metadata, final SelectorData selector) {
        try {
            exchange.getAttributes().put(Constants.PARAM_TRANSFORM, body.toString(StandardCharsets.UTF_8));
        } finally {
            DataBufferUtils.release(body);
        }
        ShenyuContext context = new ShenyuContext();
        context.setRpcType(RpcTypeEnum.GRPC.getName());
        exchange.getAttributes().put(Constants.CONTEXT, context);
        exchange.getAttributes().put(Constants.META_DATA, metadata);
        return plugin.doExecute(exchange, current -> Mono.defer(() -> responseWriter.apply(current)), selector, new RuleData());
    }

    private static DataBuffer resultBuffer(final ServerWebExchange exchange) {
        Object results = exchange.getRequiredAttribute(Constants.RPC_RESULT);
        byte[] bytes = GsonUtils.getInstance().toJson(results).getBytes(StandardCharsets.UTF_8);
        return exchange.getResponse().bufferFactory().wrap(bytes);
    }

    @SuppressWarnings("unchecked")
    private void registerClient(final ShenyuGrpcClient client) {
        Map<String, ShenyuGrpcClient> cache = (Map<String, ShenyuGrpcClient>) ReflectionTestUtils.getField(GrpcClientCache.class, "CLIENT_CACHE");
        Objects.requireNonNull(cache).put(SELECTOR_ID, client);
    }

    private static ServerCallHandler<DynamicMessage, DynamicMessage> echo(final MethodDescriptor.MethodType type) {
        return switch (type) {
            case UNARY -> ServerCalls.asyncUnaryCall((request, response) -> {
                response.onNext(request);
                response.onCompleted();
            });
            case SERVER_STREAMING -> ServerCalls.asyncServerStreamingCall((request, response) -> {
                response.onNext(request);
                response.onNext(JsonMessage.buildJsonMessage(SECOND));
                response.onCompleted();
            });
            case CLIENT_STREAMING -> ServerCalls.asyncClientStreamingCall(response -> streamingEcho(response, false));
            case BIDI_STREAMING -> ServerCalls.asyncBidiStreamingCall(response -> streamingEcho(response, true));
            default -> throw new IllegalArgumentException(type.name());
        };
    }

    private static StreamObserver<DynamicMessage> streamingEcho(final StreamObserver<DynamicMessage> response, final boolean echoEach) {
        return new StreamObserver<>() {

            private int received;

            @Override
            public void onNext(final DynamicMessage message) {
                received++;
                if (echoEach) {
                    response.onNext(message);
                }
            }

            @Override
            public void onError(final Throwable error) {
                response.onError(error);
            }

            @Override
            public void onCompleted() {
                if (!echoEach) {
                    response.onNext(JsonMessage.buildJsonMessage("{\"received\":\"" + received + "\"}"));
                }
                response.onCompleted();
            }
        };
    }

    private record BridgeResponse(int status, String body) {
    }
}
