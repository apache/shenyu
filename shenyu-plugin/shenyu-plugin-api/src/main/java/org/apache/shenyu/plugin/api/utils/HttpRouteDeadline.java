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
import java.util.Objects;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicReference;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;

/** Absolute HTTPRoute deadlines. A new backend deadline is installed on each retry subscription. */
public final class HttpRouteDeadline {

    public static final String REQUEST = HttpRouteDeadline.class.getName() + ".request";

    public static final String ATTEMPT = HttpRouteDeadline.class.getName() + ".attempt";

    private static final Runnable CLOSED = () -> { };

    private final boolean enabled;

    private final long expires;

    private final Sinks.Empty<Void> bodyComplete = Sinks.empty();

    private final AtomicReference<Runnable> cleanup = new AtomicReference<>();

    public HttpRouteDeadline(final long millis) {
        enabled = millis > 0;
        expires = System.nanoTime() + TimeUnit.MILLISECONDS.toNanos(millis);
    }

    /**
     * Check the absolute deadline.
     * @return true when the enabled deadline has expired
     */
    public boolean expired() {
        return enabled && expires - System.nanoTime() <= 0;
    }

    /**
     * Apply the remaining budget to headers or the entire transaction.
     * @param source operation to bound
     * @param <T> result type
     * @return timed operation
     */
    public <T> Mono<T> limit(final Mono<T> source) {
        if (!enabled) {
            return source;
        }
        return Mono.defer(() -> {
            long remaining = expires - System.nanoTime();
            Mono<T> failure = Mono.error(() -> new TimeoutException("HTTPRoute deadline exceeded"));
            return remaining <= 0 ? failure : source.timeout(Duration.ofNanos(remaining), failure);
        });
    }

    /**
     * Time response writing until the upstream body completes.
     * @param write downstream chain
     * @return timed response
     */
    public Mono<Void> finish(final Mono<Void> write) {
        if (!enabled) {
            return write;
        }
        Mono<Void> deadline = limit(bodyComplete.asMono()).then(Mono.never());
        return Mono.firstWithSignal(deadline, write);
    }

    /**
     * The HTTP client releases a timed attempt after cancellation reaches its body subscriber.
     * Closing its transport inside a writer cancellation hook can complete an empty response.
     * @param exchange current exchange
     * @return whether the HTTP client owns response cleanup
     */
    public static boolean ownsResponseCleanup(final ServerWebExchange exchange) {
        return Objects.nonNull(exchange.getAttribute(ATTEMPT));
    }

    /** Record complete receipt independently of downstream flush completion. */
    public void received() {
        cleanup.set(CLOSED);
        bodyComplete.tryEmitEmpty();
    }

    /**
     * Transfer ownership of a response that may arrive concurrently with cancellation.
     * @param release disposes the abandoned response
     */
    public void onAbandon(final Runnable release) {
        if (!cleanup.compareAndSet(null, release)) {
            release.run();
        }
    }

    /** Cancel a response abandoned before or during transfer to the writer. */
    public void abandon() {
        Runnable release = cleanup.getAndSet(CLOSED);
        if (Objects.nonNull(release) && release != CLOSED) {
            release.run();
        }
    }

    /**
     * Build the terminal timeout error.
     * @param cause underlying timeout
     * @return gateway timeout
     */
    public static ResponseStatusException gatewayTimeout(final Throwable cause) {
        return new ResponseStatusException(HttpStatus.GATEWAY_TIMEOUT, "HTTPRoute deadline exceeded", cause);
    }
}
