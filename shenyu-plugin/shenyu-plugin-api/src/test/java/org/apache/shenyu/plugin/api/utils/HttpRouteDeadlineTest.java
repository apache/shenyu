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

package org.apache.shenyu.plugin.api.utils;

import java.time.Duration;
import java.util.List;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicInteger;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpHeaders;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

/** Response ownership and deadline boundaries at the handoff from the transport to the writer. */
public final class HttpRouteDeadlineTest {

    @Test
    public void testBodyCompletionStopsOnlyAttemptTimer() {
        StepVerifier.withVirtualTime(() -> {
            HttpRouteDeadline attempt = new HttpRouteDeadline(200);
            Mono<Void> flush = Mono.delay(Duration.ofMillis(50)).doOnNext(tick -> attempt.received())
                    .then(Mono.delay(Duration.ofSeconds(2))).then();
            return attempt.finish(flush);
        }).thenAwait(Duration.ofSeconds(3)).verifyComplete();

        StepVerifier.withVirtualTime(() -> {
            HttpRouteDeadline request = new HttpRouteDeadline(500);
            HttpRouteDeadline attempt = new HttpRouteDeadline(200);
            Mono<Void> flush = Mono.delay(Duration.ofMillis(50)).doOnNext(tick -> attempt.received()).then(Mono.never());
            return request.limit(attempt.finish(flush));
        }).thenAwait(Duration.ofSeconds(1)).expectError(TimeoutException.class).verify();
    }

    @Test
    public void testLateHeadersCannotReplaceRestoredGatewayValues() {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/"));
        HttpRouteResponseHeaders state = new HttpRouteResponseHeaders();
        exchange.getAttributes().put(HttpRouteResponseHeaders.ATTRIBUTE, state);
        HttpHeaders headers = exchange.getResponse().getHeaders();
        headers.add(HttpHeaders.VARY, "Origin");
        HttpRouteResponseHeaders.apply(exchange, () -> headers.add(HttpHeaders.VARY, "Accept"));
        HttpRouteResponseHeaders.apply(exchange, () -> headers.add(HttpHeaders.SET_COOKIE, "origin=1"));
        state.restore(exchange);
        HttpRouteResponseHeaders.apply(exchange, () -> headers.add(HttpHeaders.SET_COOKIE, "late=1"));

        Assertions.assertEquals(List.of("Origin"), headers.get(HttpHeaders.VARY));
        Assertions.assertFalse(headers.containsKey(HttpHeaders.SET_COOKIE));
    }

    @Test
    public void testAbandonedAndCompletedHandoffs() {
        AtomicInteger releases = new AtomicInteger();
        HttpRouteDeadline cancelled = new HttpRouteDeadline(0);
        cancelled.abandon();
        cancelled.onAbandon(releases::incrementAndGet);
        cancelled.abandon();
        Assertions.assertEquals(1, releases.get());

        HttpRouteDeadline completed = new HttpRouteDeadline(0);
        completed.onAbandon(releases::incrementAndGet);
        completed.received();
        completed.abandon();
        Assertions.assertEquals(1, releases.get());
    }
}
