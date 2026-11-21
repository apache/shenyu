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

import java.time.Duration;
import java.util.concurrent.TimeoutException;
import org.apache.shenyu.plugin.api.utils.HttpRouteDeadline;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

/**
 * Bounds a retry sequence while allowing each attempt and its maximum backoff.
 */
final class RetryTimeoutUtils {

    private RetryTimeoutUtils() {
    }

    static <T> Mono<T> limitSequence(final Mono<T> response, final ServerWebExchange exchange, final Duration attemptTimeout,
                                     final int retryTimes, final Duration maxBackoff) {
        if (exchange.getAttributes().containsKey(HttpRouteDeadline.REQUEST)) {
            // Divide already owns the configured deadline across backoff, attempts and response writing.
            return response;
        }
        Duration total = totalTimeout(attemptTimeout, retryTimes, maxBackoff);
        return response.timeout(total, Mono.error(() -> new TimeoutException("Retry sequence took longer than timeout: " + total)));
    }

    static Duration totalTimeout(final Duration attemptTimeout, final int retryTimes, final Duration maxBackoff) {
        long retries = Math.max(0, retryTimes);
        try {
            long attempts = Math.multiplyExact(attemptTimeout.toNanos(), retries + 1);
            long backoffs = Math.multiplyExact(maxBackoff.toNanos(), retries);
            return Duration.ofNanos(Math.addExact(attempts, backoffs));
        } catch (ArithmeticException ex) {
            return Duration.ofNanos(Long.MAX_VALUE);
        }
    }
}
