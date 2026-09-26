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

import io.netty.channel.ConnectTimeoutException;
import io.netty.handler.timeout.ReadTimeoutException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpMethod;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.netty.http.client.PrematureCloseException;
import reactor.util.retry.Retry;

import java.net.ConnectException;
import java.net.SocketTimeoutException;
import java.time.Duration;
import java.util.Objects;
import java.util.concurrent.TimeoutException;

/**
 * Fixed Retry Policy Class.
 *
 */
public class FixedRetryStrategy<R> implements RetryStrategy<R> {
    private static final Logger LOG = LoggerFactory.getLogger(FixedRetryStrategy.class);

    private final AbstractHttpClientPlugin<R> httpClientPlugin;

    public FixedRetryStrategy(final AbstractHttpClientPlugin<R> httpClientPlugin) {
        this.httpClientPlugin = httpClientPlugin;
    }

    /**
     * Execute retry policy.
     *
     * @param response   The Mono object of the response
     * @param exchange   Current Server Exchange Object
     * @param duration   TIMEOUT
     * @param retryTimes Number of retries
     * @return Response Mono object after retry processing
     */
    public Mono<R> execute(final Mono<R> response, final ServerWebExchange exchange, final Duration duration, final int retryTimes) {
        Retry retrySpec = initFixedBackoff(exchange, retryTimes);
        return response.retryWhen(retrySpec)
                .timeout(duration, Mono.error(() -> new TimeoutException("Response took longer than timeout: " + duration)))
                .doOnError(e -> LOG.error(e.getMessage(), e));
    }

    private Retry initFixedBackoff(final ServerWebExchange exchange, final int retryTimes) {
        return Retry.fixedDelay(retryTimes, Duration.ofSeconds(2))
                .filter(throwable -> HttpMethod.GET.equals(exchange.getRequest().getMethod()) && isTransientFailure(throwable))
                .onRetryExhaustedThrow((retrySpec, retrySignal) -> retrySignal.failure());
    }

    private boolean isTransientFailure(final Throwable throwable) {
        Throwable cause = throwable;
        while (Objects.nonNull(cause)) {
            if (cause instanceof TimeoutException
                    || cause instanceof ConnectTimeoutException
                    || cause instanceof ReadTimeoutException
                    || cause instanceof ConnectException
                    || cause instanceof SocketTimeoutException
                    || cause instanceof PrematureCloseException) {
                return true;
            }
            cause = cause.getCause();
        }
        return false;
    }
}
