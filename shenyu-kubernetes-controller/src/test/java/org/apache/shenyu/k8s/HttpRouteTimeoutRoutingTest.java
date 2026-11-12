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

package org.apache.shenyu.k8s;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import io.kubernetes.client.informer.cache.Cache;
import io.kubernetes.client.informer.cache.Lister;
import io.kubernetes.client.openapi.models.V1EndpointAddress;
import io.kubernetes.client.openapi.models.V1EndpointSubsetBuilder;
import io.kubernetes.client.openapi.models.V1Endpoints;
import io.kubernetes.client.openapi.models.V1EndpointsBuilder;
import io.kubernetes.client.util.generic.dynamic.DynamicKubernetesObject;
import io.netty.buffer.Unpooled;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.List;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.DivideRuleHandle;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.k8s.parser.HttpRouteParser;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.divide.DividePlugin;
import org.apache.shenyu.plugin.divide.handler.DividePluginDataHandler;
import org.apache.shenyu.plugin.httpclient.AbstractHttpClientPlugin;
import org.apache.shenyu.plugin.httpclient.NettyHttpClientPlugin;
import org.apache.shenyu.plugin.httpclient.WebClientPlugin;
import org.apache.shenyu.plugin.response.strategy.MessageWriter;
import org.apache.shenyu.plugin.response.strategy.NettyClientMessageWriter;
import org.apache.shenyu.plugin.response.strategy.WebClientMessageWriter;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.http.client.reactive.ReactorClientHttpConnector;
import org.springframework.http.server.reactive.ReactorHttpHandlerAdapter;
import org.springframework.web.reactive.function.client.WebClient;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.adapter.WebHttpHandlerBuilder;
import reactor.core.Disposable;
import reactor.core.Exceptions;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.netty.DisposableServer;
import reactor.netty.http.client.HttpClient;
import reactor.netty.http.client.PrematureCloseException;
import reactor.netty.http.server.HttpServer;

/** Real HTTP client and response writer, with a delayed loopback backend and an actual downstream connection. */
public final class HttpRouteTimeoutRoutingTest {

    private final AtomicInteger calls = new AtomicInteger();

    private final CountDownLatch disconnected = new CountDownLatch(1);

    private final CountDownLatch headersHandedOff = new CountDownLatch(1);

    private final CountDownLatch abandoned = new CountDownLatch(1);

    private final CountDownLatch unexpectedRetry = new CountDownLatch(1);

    private final CountDownLatch streamClosed = new CountDownLatch(1);

    private final CountDownLatch retryBodyClosed = new CountDownLatch(1);

    private final String id = UUID.randomUUID().toString();

    private final SelectorData selector = SelectorData.builder().id(id).build();

    private final RuleData rule = RuleData.builder().id(id).selectorId(id).build();

    private final DividePluginDataHandler handler = new DividePluginDataHandler();

    private DisposableServer backend;

    private DisposableServer gateway;

    private HttpClient caller;

    private void startBackend() {
        backend = HttpServer.create().host("127.0.0.1").port(0).handle((req, res) -> {
            int call = calls.incrementAndGet();
            if (req.uri().equals("/retry-body")) {
                if (call == 1) {
                    return Mono.never();
                }
                req.withConnection(conn -> conn.onDispose(retryBodyClosed::countDown));
                res.header("Content-Type", "text/event-stream");
                return res.sendGroups(Flux.just(Mono.fromSupplier(() -> Unpooled.copiedBuffer("data: start\n\n", StandardCharsets.UTF_8)),
                        Mono.delay(Duration.ofSeconds(1)).map(tick -> Unpooled.copiedBuffer("data: done\n\n", StandardCharsets.UTF_8))));
            }
            if (req.uri().equals("/hold")) {
                req.withConnection(conn -> conn.onDispose(abandoned::countDown));
                return res.sendHeaders().then().then(Mono.never());
            }
            if (req.uri().equals("/stream")) {
                req.withConnection(conn -> conn.onDispose(streamClosed::countDown));
                res.header("Content-Type", "text/event-stream");
                return res.sendGroups(Flux.interval(Duration.ZERO, Duration.ofMillis(40))
                        .map(index -> Mono.just(Unpooled.copiedBuffer("data: one\\n\\n", StandardCharsets.UTF_8))));
            }
            if (req.uri().equals("/backoff") && call > 1) {
                unexpectedRetry.countDown();
            }
            if (req.uri().equals("/delay")) {
                return Mono.delay(Duration.ofSeconds(1)).then(res.sendString(Mono.just("ok")).then());
            }
            if (req.uri().equals("/body")) {
                res.header("Set-Cookie", "backend=session").header("X-Origin", "upstream").header("ETag", "backend-tag");
                req.withConnection(conn -> conn.onDispose(disconnected::countDown));
                return res.sendHeaders().then().then(Mono.never());
            }
            if (req.uri().equals("/backoff") || req.uri().equals("/slow") || req.uri().equals("/retry") && call == 1) {
                return Mono.delay(Duration.ofSeconds(3)).then(res.sendString(Mono.just("late")).then());
            }
            return res.sendString(Mono.just("ok"));
        }).bindNow();
    }

    private void startGateway(final boolean netty) {
        startBackend();
        Upstream upstream = Upstream.builder().protocol("http://").url("127.0.0.1:" + backend.port()).weight(100).status(true).build();
        UpstreamCacheManager.getInstance().submit(id, List.of(upstream));
        HttpClient transport = HttpClient.create();
        AbstractHttpClientPlugin<?> client = netty ? new NettyHttpClientPlugin(transport)
                : new WebClientPlugin(WebClient.builder().clientConnector(new ReactorClientHttpConnector(transport)).build());
        MessageWriter writer = netty ? new NettyClientMessageWriter() : new WebClientMessageWriter();
        MatchedDivide divide = new MatchedDivide();
        gateway = HttpServer.create().host("127.0.0.1").port(0).handle(new ReactorHttpHandlerAdapter(
                WebHttpHandlerBuilder.webHandler(exchange -> {
                    exchange.getAttributes().put(Constants.CONTEXT, new ShenyuContext());
                    exchange.getResponse().getHeaders().set("X-Origin", "gateway");
                    return divide.forward(exchange, selected -> {
                        selected.getAttributes().put(Constants.HTTP_URI, URI.create(selected.getAttribute(Constants.HTTP_DOMAIN)
                                + selected.getRequest().getPath().value()));
                        return client.execute(selected, response -> {
                            if (response.getRequest().getPath().value().equals("/hold")) {
                                headersHandedOff.countDown();
                                return Mono.never();
                            }
                            return writer.writeWith(response, end -> Mono.empty());
                        });
                    }, selector, rule);
                }).exceptionHandler((exchange, failure) -> {
                    if (failure instanceof ResponseStatusException status && !exchange.getResponse().isCommitted()) {
                        exchange.getResponse().setStatusCode(status.getStatusCode());
                        return exchange.getResponse().setComplete();
                    }
                    return Mono.error(failure);
                }).build())).bindNow();
        caller = HttpClient.create().baseUrl("http://127.0.0.1:" + gateway.port());
        configure(0, 0);
        Assertions.assertEquals(200, status("/ok"));
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    public void testConformanceCases(final boolean netty) {
        startGateway(netty);
        // v1.5.1 HTTPRouteTimeoutRequest and HTTPRouteTimeoutBackendRequest each specify one field.
        for (boolean request : List.of(true, false)) {
            configure(request ? "500ms" : null, request ? null : "500ms");
            Assertions.assertEquals(200, status("/ok"));
            Assertions.assertEquals(504, status("/delay"));
            configure(request ? "0s" : null, request ? null : "0s");
            Assertions.assertEquals(200, status("/delay"));
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    public void testRetryBudgets(final boolean netty) {
        startGateway(netty);
        calls.set(0);
        configure(2500, 300);
        Assertions.assertEquals(200, status("/retry"));
        Assertions.assertEquals(2, calls.get());

        calls.set(0);
        configure(0, 150);
        Assertions.assertEquals(504, status("/slow"));
        Assertions.assertEquals(4, calls.get());

        calls.set(0);
        configure(550, 350);
        Assertions.assertEquals(504, status("/slow"));
        Assertions.assertEquals(2, calls.get());
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    public void testRequestDeadlineSpansRetryAndResponseBody(final boolean netty) throws Exception {
        startGateway(netty);
        calls.set(0);
        // The first attempt consumes 1500 ms. The retry flushes one event, then waits 1000 ms
        // for its final event: within its fresh attempt budget, beyond the shared 2000 ms.
        configure(2000, 1500);
        StringBuilder received = new StringBuilder();
        RuntimeException truncated = Assertions.assertThrows(RuntimeException.class, () -> caller.get().uri("/retry-body")
                .response((response, body) -> {
                    Assertions.assertEquals(200, response.status().code());
                    return body.asString().doOnNext(received::append).then();
                }).then().block(Duration.ofSeconds(8)));
        Assertions.assertTrue(Exceptions.unwrap(truncated) instanceof PrematureCloseException, truncated.toString());
        Assertions.assertEquals("data: start\n\n", received.toString());
        Assertions.assertEquals(2, calls.get());
        Assertions.assertTrue(retryBodyClosed.await(2, TimeUnit.SECONDS), "timed-out retry still owns its backend connection");

        calls.set(0);
        configure(5000, 1500);
        Assertions.assertEquals("data: start\n\ndata: done\n\n", caller.get().uri("/retry-body").responseSingle((response, body) -> {
            Assertions.assertEquals(200, response.status().code());
            return body.asString();
        }).block(Duration.ofSeconds(8)));
        Assertions.assertEquals(2, calls.get());
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    public void testStalledAndStreamingBodies(final boolean netty) throws Exception {
        startGateway(netty);
        calls.set(0);
        configure(0, 250);
        caller.get().uri("/body").responseSingle((response, body) -> {
            Assertions.assertEquals(504, response.status().code());
            Assertions.assertFalse(response.responseHeaders().contains("Set-Cookie"));
            Assertions.assertFalse(response.responseHeaders().contains("ETag"));
            Assertions.assertEquals("gateway", response.responseHeaders().get("X-Origin"));
            return body.then();
        }).block(Duration.ofSeconds(8));
        Assertions.assertTrue(disconnected.await(2, TimeUnit.SECONDS), "backend connection still open after body timeout");
        Assertions.assertEquals(1, calls.get());

        configure(0, 400);
        calls.set(0);
        AtomicInteger chunks = new AtomicInteger();
        RuntimeException truncated = Assertions.assertThrows(RuntimeException.class, () -> caller.get().uri("/stream")
                .response((res, body) -> body.doOnNext(chunk -> chunks.incrementAndGet()).then()).then().block(Duration.ofSeconds(4)));
        Assertions.assertTrue(Exceptions.unwrap(truncated) instanceof PrematureCloseException,
                truncated.toString());
        Assertions.assertTrue(chunks.get() > 0);
        Assertions.assertTrue(streamClosed.await(2, TimeUnit.SECONDS));
        Assertions.assertEquals(1, calls.get());
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    public void testCancellationDuringBackoffAndHeaderHandoff(final boolean netty) throws Exception {
        startGateway(netty);
        calls.set(0);
        configure(500, 200);
        DivideRuleHandle handle = GsonUtils.getGson().fromJson(rule.getHandle(), DivideRuleHandle.class);
        handle.setRetryBackOffSpec("fixed");
        rule.setHandle(GsonUtils.getGson().toJson(handle));
        handler.handlerRule(rule);
        Assertions.assertEquals(504, status("/backoff"));
        Assertions.assertFalse(unexpectedRetry.await(2100, TimeUnit.MILLISECONDS));
        Assertions.assertEquals(1, calls.get());

        configure(0, 0);
        Disposable pending = caller.get().uri("/hold").response().subscribe();
        try {
            Assertions.assertTrue(headersHandedOff.await(2, TimeUnit.SECONDS));
        } finally {
            pending.dispose();
        }
        Assertions.assertTrue(abandoned.await(2, TimeUnit.SECONDS), "abandoned backend body still owns a connection");
    }

    @AfterEach
    public void stop() {
        if (Objects.nonNull(gateway)) {
            gateway.disposeNow();
        }
        if (Objects.nonNull(backend)) {
            backend.disposeNow();
        }
        UpstreamCacheManager.getInstance().removeByKey(id);
        handler.removeRule(rule);
    }

    private void configure(final long request, final long backend) {
        configure(request + "ms", backend + "ms");
    }

    private void configure(final String request, final String backend) {
        JsonObject route = JsonParser.parseString("""
                {"apiVersion":"gateway.networking.k8s.io/v1","kind":"HTTPRoute","metadata":{"name":"test","namespace":"ns"},
                 "spec":{"rules":[{"backendRefs":[{"name":"service","port":80}]}]}}
                """).getAsJsonObject();
        JsonObject timeouts = new JsonObject();
        if (Objects.nonNull(request)) {
            timeouts.addProperty("request", request);
        }
        if (Objects.nonNull(backend)) {
            timeouts.addProperty("backendRequest", backend);
        }
        route.getAsJsonObject("spec").getAsJsonArray("rules").get(0).getAsJsonObject().add("timeouts", timeouts);
        Cache<V1Endpoints> endpoints = new Cache<>();
        endpoints.add(new V1EndpointsBuilder().withNewMetadata().withName("service").withNamespace("ns").endMetadata()
                .withSubsets(new V1EndpointSubsetBuilder().withAddresses(new V1EndpointAddress().ip("127.0.0.1")).build()).build());
        HttpRouteParser parser = new HttpRouteParser(new Lister<>(endpoints), new Lister<>(new Cache<>()), new Lister<>(new Cache<>()));
        rule.setHandle(parser.parse(new DynamicKubernetesObject(route), List.of()).getRouteConfigList().get(0).getRuleDataList().get(0).getHandle());
        handler.handlerRule(rule);
    }

    private int status(final String path) {
        return caller.get().uri(path).responseSingle((response, body) -> body.thenReturn(response.status().code()))
                .block(Duration.ofSeconds(8));
    }

    private static final class MatchedDivide extends DividePlugin {

        Mono<Void> forward(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
            return doExecute(exchange, chain, selector, rule);
        }
    }
}
