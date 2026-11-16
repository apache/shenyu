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

import io.opentelemetry.api.baggage.Baggage;
import io.opentelemetry.api.trace.Span;
import io.opentelemetry.api.trace.SpanContext;
import io.opentelemetry.api.trace.TraceFlags;
import io.opentelemetry.api.trace.TraceState;
import io.opentelemetry.context.Context;
import io.opentelemetry.context.Scope;
import org.apache.shenyu.common.tracing.AgentTraceContext;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.http.HttpStatus;
import org.springframework.web.reactive.function.client.ClientRequest;
import org.springframework.web.reactive.function.client.ClientResponse;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;

import java.net.URI;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

class AiOutboundTraceContextTest {

    private static final String TRACE = "0123456789abcdef0123456789abcdef";

    private static final String SPAN = "0123456789abcdef";

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void injectsOnlyExplicitTraceAndDoesNotModifyRequest(final boolean gatewayKey) {
        final AtomicReference<ClientRequest> sent = new AtomicReference<>();
        final WebClient client = client(sent);
        final AiUpstreamRequest request = request();
        final Context parent = Baggage.builder().put("subject", "secret").build().storeInContext(parent(SPAN));
        new WebClientAiProxyTransport(client).execute(request).flatMap(result -> result.getBody().then())
                .contextWrite(context -> context.put(gatewayKey ? AgentTraceContext.GATEWAY_REACTOR_CONTEXT_KEY : Context.class, parent)).block();
        assertEquals("00-" + TRACE + "-" + SPAN + "-01", sent.get().headers().getFirst("traceparent"));
        assertEquals("fixture=value", sent.get().headers().getFirst("tracestate"));
        assertNull(sent.get().headers().getFirst("baggage"));
        assertNull(sent.get().headers().getFirst("X-Spike-Subject"));
        assertEquals("Bearer service-key", sent.get().headers().getFirst("Authorization"));
        assertEquals(List.of("stale"), request.getHeaders().get("TraceParent"));
    }

    @Test
    void gatewayParentWinsAndInvalidGatewayFallsBackToExplicitOtelContext() {
        final Context other = parent("1111111111111111");
        final reactor.util.context.Context view = reactor.util.context.Context.of(AgentTraceContext.GATEWAY_REACTOR_CONTEXT_KEY, parent(SPAN), Context.class, other);
        assertEquals("00-" + TRACE + "-" + SPAN + "-01", AiOutboundTraceContext.headers(view).get("traceparent"));
        assertEquals("00-" + TRACE + "-1111111111111111-01",
                AiOutboundTraceContext.headers(view.put(AgentTraceContext.GATEWAY_REACTOR_CONTEXT_KEY, "invalid")).get("traceparent"));
    }

    @Test
    void ambientContextAndStaleConfiguredHeadersAreNotForwarded() {
        final AtomicReference<ClientRequest> sent = new AtomicReference<>();
        try (Scope ignored = parent(SPAN).makeCurrent()) {
            new WebClientAiProxyTransport(client(sent)).execute(request()).flatMap(result -> result.getBody().then()).block();
        }
        assertNull(sent.get().headers().getFirst("traceparent"));
        assertNull(sent.get().headers().getFirst("tracestate"));
        assertNull(sent.get().headers().getFirst("baggage"));
        assertEquals(Map.of(), AiOutboundTraceContext.headers(reactor.util.context.Context.of(Context.class, "wrong-type")));
    }

    @Test
    void sameRequestAcrossConcurrentSubscriptionsKeepsEachParentIsolated() {
        final Map<String, String> captured = new ConcurrentHashMap<>();
        final WebClient client = WebClient.builder().exchangeFunction(request -> Mono.deferContextual(context -> {
            captured.put(context.get("call"), request.headers().getFirst("traceparent"));
            return Mono.just(ClientResponse.create(HttpStatus.OK).body("{}").build());
        })).build();
        final Mono<Void> execute = new WebClientAiProxyTransport(client).execute(request()).flatMap(response -> response.getBody().then());
        Flux.range(1, 16).flatMap(number -> {
            String span = String.format("%016x", number);
            return execute.subscribeOn(Schedulers.parallel()).contextWrite(context -> context.put(Context.class, parent(span)).put("call", span));
        }).blockLast();
        assertEquals(16, captured.size());
        captured.forEach((span, trace) -> assertEquals("00-" + TRACE + "-" + span + "-01", trace));
    }

    @Test
    void transportActuallyLoadsAndExecutesWithoutAnyOtelClasses() throws Exception {
        final AtomicReference<ClientRequest> sent = new AtomicReference<>();
        final URL classes = WebClientAiProxyTransport.class.getProtectionDomain().getCodeSource().getLocation();
        try (NoOtelLoader loader = new NoOtelLoader(classes)) {
            assertThrows(ClassNotFoundException.class, () -> loader.loadClass("io.opentelemetry.api.trace.Span"));
            Class<?> isolated = loader.loadClass(WebClientAiProxyTransport.class.getName());
            assertNotSame(WebClientAiProxyTransport.class, isolated);
            Object transport = isolated.getConstructor(WebClient.class).newInstance(client(sent));
            Mono<?> execution = (Mono<?>) isolated.getMethod("execute", AiUpstreamRequest.class).invoke(transport, request());
            AiUpstreamResponse result = (AiUpstreamResponse) execution.block();
            result.getBody().then().block();
            assertEquals(200, result.getStatusCode());
            assertFalse(loader.bridgeLoaded);
            assertNull(sent.get().headers().getFirst("traceparent"));
            assertNull(sent.get().headers().getFirst("baggage"));
        }
    }

    private Context parent(final String span) {
        return Context.root().with(Span.wrap(SpanContext.create(TRACE, span, TraceFlags.getSampled(), TraceState.builder().put("fixture", "value").build())));
    }

    private WebClient client(final AtomicReference<ClientRequest> sent) {
        return WebClient.builder().defaultHeader("baggage", "configured=secret").exchangeFunction(request -> {
            sent.set(request);
            return Mono.just(ClientResponse.create(HttpStatus.OK).body("{}").build());
        }).build();
    }

    private AiUpstreamRequest request() {
        final AiUpstreamRequest request = new AiUpstreamRequest();
        request.setUri(URI.create("https://fixture.invalid/v1/chat/completions"));
        request.setMethod("POST");
        request.setHeaders(Map.of("Authorization", List.of("Bearer service-key"), "TraceParent", List.of("stale"),
                "Tracestate", List.of("stale=value"), "Baggage", List.of("subject=secret")));
        return request;
    }

    private static final class NoOtelLoader extends URLClassLoader {

        private boolean bridgeLoaded;

        private NoOtelLoader(final URL classes) {
            super(new URL[] {classes}, AiOutboundTraceContextTest.class.getClassLoader());
        }

        @Override
        protected synchronized Class<?> loadClass(final String name, final boolean resolve) throws ClassNotFoundException {
            if (name.startsWith("io.opentelemetry.")) {
                throw new ClassNotFoundException(name);
            }
            if (name.startsWith("org.apache.shenyu.plugin.ai.proxy.enhanced.transport.")) {
                bridgeLoaded |= name.equals(AiOutboundTraceContext.class.getName());
                Class<?> loaded = findLoadedClass(name);
                if (java.util.Objects.isNull(loaded)) {
                    loaded = findClass(name);
                }
                if (resolve) {
                    resolveClass(loaded);
                }
                return loaded;
            }
            return super.loadClass(name, resolve);
        }
    }
}
