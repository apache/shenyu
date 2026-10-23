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
import org.apache.shenyu.common.enums.LoadBalanceEnum;
import org.apache.shenyu.common.enums.RetryEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.UriUtils;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.plugin.api.utils.RequestUrlUtils;
import org.apache.shenyu.plugin.base.circuitbreaker.UpstreamCircuitBreaker;
import org.apache.shenyu.plugin.base.utils.LoadbalancerUtils;
import org.apache.shenyu.plugin.base.utils.UpstreamRoutingContext;
import org.apache.shenyu.plugin.httpclient.exception.ShenyuTimeoutException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.util.retry.Retry;
import reactor.util.retry.RetryBackoffSpec;

import java.net.URI;
import java.time.Duration;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.TimeoutException;
import java.util.function.Predicate;
import java.util.stream.Collectors;


/**
 * Default Retry Policy Class
 * Keep the original default request retry test without any changes.
 * @param <R> Request Response Type
 */
public class DefaultRetryStrategy<R> implements RetryStrategy<R> {
    private static final Logger LOG = LoggerFactory.getLogger(DefaultRetryStrategy.class);

    private final AbstractHttpClientPlugin<R> httpClientPlugin;

    public DefaultRetryStrategy(final AbstractHttpClientPlugin<R> httpClientPlugin) {
        this.httpClientPlugin = httpClientPlugin;
    }

    @Override
    public Mono<R> execute(final Mono<R> clientResponse, final ServerWebExchange exchange, final Duration duration, final int retryTimes) {
        final String retryStrategy = (String) Optional.ofNullable(exchange.getAttribute(Constants.RETRY_STRATEGY)).orElseGet(() -> "current");
        if (RetryEnum.CURRENT.getName().equals(retryStrategy)) {
            //old version of DividePlugin and SpringCloudPlugin will run on this
            RetryBackoffSpec retryBackoffSpec = Retry.backoff(retryTimes, Duration.ofMillis(20L))
                    .maxBackoff(Duration.ofSeconds(20L))
                    .transientErrors(true)
                    .jitter(0.5d)
                    .filter(t -> (t instanceof java.util.concurrent.TimeoutException || t instanceof io.netty.channel.ConnectTimeoutException
                            || t instanceof io.netty.handler.timeout.ReadTimeoutException || t instanceof IllegalStateException)
                            && !(t instanceof org.springframework.core.io.buffer.DataBufferLimitException))
                    .onRetryExhaustedThrow((retryBackoffSpecErr, retrySignal) -> {
                        throw new ShenyuTimeoutException("Request timeout, the maximum number of retry times has been exceeded");
                    });
            Duration totalTimeout = RetryTimeoutUtils.totalTimeout(duration, retryTimes, Duration.ofSeconds(20));
            return clientResponse.retryWhen(retryBackoffSpec)
                    .timeout(totalTimeout, Mono.error(() -> new TimeoutException("Retry sequence took longer than timeout: " + totalTimeout)))
                    .onErrorMap(ShenyuTimeoutException.class, th -> new ResponseStatusException(HttpStatus.REQUEST_TIMEOUT, th.getMessage(), th))
                    .onErrorMap(java.util.concurrent.TimeoutException.class, th -> new ResponseStatusException(HttpStatus.GATEWAY_TIMEOUT, th.getMessage(), th));
        }
        return executeFailover(clientResponse, exchange, duration, retryTimes, Duration.ZERO, th -> true);
    }

    /**
     * Retry eligible failures on another upstream after the configured delay.
     *
     * @param clientResponse original request response
     * @param exchange current exchange
     * @param duration timeout per request
     * @param retryTimes number of retries
     * @param backoff delay before selecting the next upstream
     * @param retryFilter eligible failures
     * @return response after failover
     */
    Mono<R> executeFailover(final Mono<R> clientResponse, final ServerWebExchange exchange, final Duration duration,
                           final int retryTimes, final Duration backoff, final Predicate<Throwable> retryFilter) {
        final Set<URI> exclude = new HashSet<>(Collections.singletonList(Objects.requireNonNull(exchange.getAttribute(Constants.HTTP_URI))));
        Mono<R> result = clientResponse;
        for (int i = 0; i < retryTimes; i++) {
            result = result.onErrorResume(retryFilter, th -> backoff.isZero()
                    ? resend(exchange, duration, exclude)
                    : Mono.delay(backoff).flatMap(ignored -> resend(exchange, duration, exclude)));
        }
        return result.onErrorMap(ShenyuException.class, th -> new ResponseStatusException(HttpStatus.SERVICE_UNAVAILABLE,
                        "CANNOT_FIND_HEALTHY_UPSTREAM_URL_AFTER_FAILOVER", th))
                .onErrorMap(java.util.concurrent.TimeoutException.class, th -> new ResponseStatusException(HttpStatus.GATEWAY_TIMEOUT, th.getMessage(), th));
    }

    private Mono<R> resend(final ServerWebExchange exchange,
                           final Duration duration,
                           final Set<URI> exclude) {
        final String selectorId = exchange.getAttribute(Constants.DIVIDE_SELECTOR_ID);
        final String loadBalance = exchange.getAttribute(Constants.LOAD_BALANCE);
        // Refresh health/topology without losing the request's resolved label partition.
        final UpstreamRoutingContext routing = exchange.getAttribute(UpstreamRoutingContext.ATTRIBUTE);
        final List<Upstream> availableUpstreamList = Objects.isNull(routing)
                ? UpstreamCacheManager.getInstance().findUpstreamListBySelectorId(selectorId)
                : routing.filter(selectorId, UpstreamCacheManager.getInstance().findAllUpstreamListBySelectorId(selectorId));
        if (Objects.isNull(availableUpstreamList)) {
            return Mono.error(new ShenyuException("CANNOT_FIND_HEALTHY_UPSTREAM_URL_AFTER_FAILOVER"));
        }
        final List<Upstream> upstreamList = availableUpstreamList
                .stream().filter(data -> {
                    final URI upstreamUri = URI.create(data.buildDomain());
                    for (URI needToExclude : exclude) {
                        if (isSameUpstream(needToExclude, upstreamUri)) {
                            return false;
                        }
                    }
                    return true;
                })
                //skip upstreams that are still blocked by the built-in circuit breaker
                .filter(data -> UpstreamCircuitBreaker.getInstance()
                        .isRequestAllowed(UpstreamCircuitBreaker.buildKey(selectorId, data)))
                .collect(Collectors.toList());
        if (upstreamList.isEmpty()) {
            // no need to retry anymore
            return Mono.error(new ShenyuException("CANNOT_FIND_HEALTHY_UPSTREAM_URL_AFTER_FAILOVER"));
        }
        final Upstream upstream = LoadbalancerUtils.getForExchange(upstreamList, loadBalance, exchange);
        if (Objects.isNull(upstream)) {
            // no need to retry anymore
            return Mono.error(new ShenyuException("CANNOT_FIND_HEALTHY_UPSTREAM_URL_AFTER_FAILOVER"));
        }
        final URI newUri = RequestUrlUtils.buildRequestUri(exchange, upstream.buildDomain());
        // in order not to affect the next retry call, newUri needs to be excluded
        exclude.add(newUri);
        final String breakerKey = UpstreamCircuitBreaker.buildKey(selectorId, upstream);
        Mono<R> retryResponse = httpClientPlugin
                .doRequest(exchange, exchange.getRequest().getMethod().name(), newUri, httpClientPlugin.getCachedRequestBody(exchange))
                .timeout(duration, Mono.error(() -> new TimeoutException("Response took longer than timeout: " + duration)))
                .doOnError(e -> LOG.error(e.getMessage(), e));
        if (LoadBalanceEnum.P2C.getName().equals(loadBalance)) {
            // Selecting the retry upstream above goes through the load balancer, which increments the P2C
            // inflight counter of the newly picked upstream. Divide's completion callback only captures the
            // initially selected upstream, so release this attempt's increment to keep P2C accounting paired
            // across success, failure and cancellation of the failover. See #7975.
            retryResponse = retryResponse.doFinally(signalType -> upstream.getInflight().decrementAndGet());
        } else if (LoadBalanceEnum.LEAST_ACTIVITY.getName().equals(loadBalance)) {
            // Least active compares outstanding requests but does not reserve on select, so this attempt
            // stays reserved until it terminates. See #7986.
            upstream.getInflight().incrementAndGet();
            retryResponse = retryResponse.doFinally(signalType -> upstream.getInflight().decrementAndGet());
        } else if (LoadBalanceEnum.SHORTEST_RESPONSE.getName().equals(loadBalance)) {
            // Selection already incremented this standby. Divide only releases the original upstream,
            // so release this attempt once on every terminal signal. The P2C branch above is that
            // release for p2c; do not increment here or decrement a second time.
            retryResponse = retryResponse.doFinally(signalType -> upstream.getInflight()
                    .updateAndGet(current -> current > 0 ? current - 1 : 0));
        }
        return UpstreamCircuitBreaker.recordOutcome(retryResponse, breakerKey);
    }

    private boolean isSameUpstream(final URI first, final URI second) {
        final String firstScheme = first.getScheme().toLowerCase(Locale.ROOT);
        final String secondScheme = second.getScheme().toLowerCase(Locale.ROOT);
        return firstScheme.equals(secondScheme)
                && first.getHost().equalsIgnoreCase(second.getHost())
                && UriUtils.getActualPort(firstScheme, first.getPort()) == UriUtils.getActualPort(secondScheme, second.getPort());
    }
}
