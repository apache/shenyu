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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.util.retry.Retry;
import reactor.util.retry.RetryBackoffSpec;

import java.time.Duration;

/**
 * Exponential Retry Backoff Strategy.
 *
 */
public class ExponentialRetryBackoffStrategy<R> implements RetryStrategy<R> {
    private static final Logger LOG = LoggerFactory.getLogger(ExponentialRetryBackoffStrategy.class);

    private final AbstractHttpClientPlugin<R> httpClientPlugin;

    public ExponentialRetryBackoffStrategy(final AbstractHttpClientPlugin<R> httpClientPlugin) {
        this.httpClientPlugin = httpClientPlugin;
    }

    /**
     * Execute retry policy.
     *
     * @param response    The Mono object of the response
     * @param exchange    Current Server Exchange Object
     * @param duration    Timeout
     * @param retryTimes  Number of retries
     * @return Response Mono object after retry processing
     */
    public Mono<R> execute(final Mono<R> response, final ServerWebExchange exchange, final Duration duration, final int retryTimes) {
        RetryBackoffSpec retrySpec = initDefaultBackoff(retryTimes);
        return response.retryWhen(retrySpec)
                .timeout(duration, Mono.error(() -> new java.util.concurrent.TimeoutException("Response took longer than timeout: " + duration)))
                .doOnError(e -> LOG.error(e.getMessage(), e));
    }

    private RetryBackoffSpec initDefaultBackoff(final int retryTimes) {
        return Retry.backoff(retryTimes, Duration.ofMillis(500))
                .maxBackoff(Duration.ofSeconds(5))
                // Retry only for instantaneous errors
                .transientErrors(true)
                // Add 50% random jitter to the delay time of each retry
                .jitter(0.5d)
                .filter(t -> t instanceof IllegalStateException)
                // When the maximum number of retrys is reached, a specified exception is thrown
                .onRetryExhaustedThrow((retryBackoffSpecErr, retrySignal) -> {
                    throw new IllegalStateException("Retry limit exceeded");
                });
    }
}
