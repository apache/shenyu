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

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.exception.StreamIdleTimeoutException;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.utils.HttpRouteDeadline;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.reactive.function.client.ClientResponse;
import org.springframework.web.reactive.function.client.WebClient;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;
import reactor.test.StepVerifier;

import java.net.URI;
import java.time.Duration;
import java.util.List;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

final class ResponseBodyIdleTimeoutTest {

    private final AtomicReference<ResponseBodyIdleTimeout> transportPolicy = new AtomicReference<>();

    private final AtomicInteger calls = new AtomicInteger();

    private final AtomicBoolean cancelled = new AtomicBoolean();

    private final WebClientPlugin plugin = new WebClientPlugin(WebClient.builder().exchangeFunction(request -> Mono.deferContextual(context -> {
        calls.incrementAndGet();
        transportPolicy.set(context.getOrDefault(ResponseBodyIdleTimeout.class, null));
        return Mono.just(ClientResponse.create(HttpStatus.OK)
                .header(HttpHeaders.CONTENT_TYPE, "application/octet-stream")
                .header(HttpHeaders.SET_COOKIE, "upstream=1")
                .body(Flux.<DataBuffer>never().doOnCancel(() -> cancelled.set(true))).build());
    })).build(), Constants.BYTES_PER_MB);

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void testTimeoutDuringOrBeforeBodySubscription(final boolean committed) {
        ServerWebExchange exchange = exchange();
        exchange.getAttributes().put(Constants.STREAM_IDLE_TIMEOUT, 1234L);
        exchange.getResponse().getHeaders().add(HttpHeaders.VARY, "Origin");
        StepVerifier.create(plugin.execute(exchange, current -> {
            ResponseEntity<Flux<DataBuffer>> upstream = current.getRequiredAttribute(Constants.CLIENT_RESPONSE_ATTR);
            return committed ? current.getResponse().setComplete().then(upstream.getBody().then()) : Mono.never();
        })).then(() -> {
            assertEquals(1234L, transportPolicy.get().millis());
            assertEquals(committed, exchange.getResponse().isCommitted());
            transportPolicy.get().expire();
        }).expectErrorSatisfies(error -> {
            if (committed) {
                assertInstanceOf(StreamIdleTimeoutException.class, error);
                assertEquals(HttpStatus.OK, exchange.getResponse().getStatusCode());
            } else {
                ResponseStatusException status = assertInstanceOf(ResponseStatusException.class, error);
                assertEquals(HttpStatus.GATEWAY_TIMEOUT, status.getStatusCode());
                assertInstanceOf(StreamIdleTimeoutException.class, status.getCause());
                assertFalse(exchange.getResponse().getHeaders().containsKey(HttpHeaders.SET_COOKIE));
                assertEquals("Origin", exchange.getResponse().getHeaders().getFirst(HttpHeaders.VARY));
            }
        }).verify();
        assertTrue(cancelled.get());
        assertEquals(1, calls.get());
    }

    @Test
    void testUnsetDeletesInheritedTransportPolicy() {
        StepVerifier.create(plugin.execute(exchange(), current -> Mono.empty())
                .contextWrite(context -> context.put(ResponseBodyIdleTimeout.class, new ResponseBodyIdleTimeout(1))))
                .verifyComplete();
        assertNull(transportPolicy.get());
    }

    @Test
    void testAbandonedFailoverResponseCannotExpireItsSuccessor() {
        String selector = "idle-timeout-failover";
        ServerWebExchange exchange = exchange();
        exchange.getAttributes().put(Constants.DIVIDE_SELECTOR_ID, selector);
        exchange.getAttributes().put(Constants.LOAD_BALANCE, "random");
        exchange.getAttributes().put(Constants.STREAM_IDLE_TIMEOUT, 10000L);
        exchange.getAttributes().put(Constants.HTTP_TIME_OUT, 500L);
        exchange.getAttributes().put(Constants.RETRY_STRATEGY, "failover");
        UpstreamCacheManager.getInstance().submit(selector, List.of(Upstream.builder().url("127.0.0.1:18080").healthCheckEnabled(false).build()));
        AtomicReference<ResponseBodyIdleTimeout> first = new AtomicReference<>();
        Sinks.Empty<Void> finishWrite = Sinks.empty();
        WebClient client = WebClient.builder().exchangeFunction(request -> Mono.deferContextual(context -> {
            calls.incrementAndGet();
            if (first.compareAndSet(null, context.get(ResponseBodyIdleTimeout.class))) {
                // Failover drains this 503 before it can select another upstream.
                return Mono.just(ClientResponse.create(HttpStatus.SERVICE_UNAVAILABLE)
                        .body(Flux.<DataBuffer>never().doOnCancel(() -> cancelled.set(true))).build());
            }
            return Mono.just(ClientResponse.create(HttpStatus.OK).build());
        })).build();
        try {
            WebClientPlugin retrying = new WebClientPlugin(client, Constants.BYTES_PER_MB);
            StepVerifier.withVirtualTime(() -> retrying.execute(exchange, current -> finishWrite.asMono()))
                    .thenAwait(Duration.ofMillis(501))
                    .then(() -> {
                        assertTrue(cancelled.get());
                        assertEquals(2, calls.get());
                        // The previous event loop may run its timer after the next attempt has started.
                        first.get().expire();
                        finishWrite.tryEmitEmpty();
                    })
                    .verifyComplete();
            assertEquals(HttpStatus.OK, exchange.getResponse().getStatusCode());
        } finally {
            UpstreamCacheManager.getInstance().removeByKey(selector);
        }
    }

    @Test
    void testLegacyHeaderBudgetDoesNotTimeBody() {
        ServerWebExchange exchange = exchange();
        exchange.getAttributes().put(Constants.STREAM_IDLE_TIMEOUT, 10000L);
        exchange.getAttributes().put(Constants.HTTP_TIME_OUT, 500L);
        StepVerifier.withVirtualTime(() -> plugin.execute(exchange, current -> Mono.delay(Duration.ofSeconds(2)).then()))
                .thenAwait(Duration.ofSeconds(3)).verifyComplete();
        assertTrue(cancelled.get());
    }

    @ParameterizedTest
    @ValueSource(longs = {0, 2000})
    void testHttpRouteDeadlinesSurviveBodyOverride(final long requestMillis) {
        ServerWebExchange exchange = exchange();
        exchange.getAttributes().put(Constants.STREAM_IDLE_TIMEOUT, 300000L);
        exchange.getAttributes().put(Constants.HTTP_TIME_OUT, 5000L);
        StepVerifier.withVirtualTime(() -> {
            HttpRouteDeadline request = new HttpRouteDeadline(requestMillis);
            exchange.getAttributes().put(HttpRouteDeadline.REQUEST, request);
            return request.limit(plugin.execute(exchange, current -> Mono.never()));
        }).thenAwait(Duration.ofSeconds(6)).expectErrorSatisfies(error -> {
            if (requestMillis == 0) {
                ResponseStatusException status = assertInstanceOf(ResponseStatusException.class, error);
                assertEquals(HttpStatus.GATEWAY_TIMEOUT, status.getStatusCode());
                assertInstanceOf(TimeoutException.class, status.getCause());
            } else {
                assertInstanceOf(TimeoutException.class, error);
            }
        }).verify();
        assertTrue(cancelled.get());
    }

    private ServerWebExchange exchange() {
        ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/download"));
        exchange.getAttributes().put(Constants.CONTEXT, new ShenyuContext());
        exchange.getAttributes().put(Constants.HTTP_URI, URI.create("http://upstream/download"));
        exchange.getAttributes().put(Constants.HTTP_RETRY, 2);
        return exchange;
    }
}
