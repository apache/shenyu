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

import java.net.URI;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.TimeoutException;
import java.util.function.Function;
import java.util.function.Supplier;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.HeaderUniqueStrategyEnum;
import org.apache.shenyu.common.enums.HttpRetryBackoffSpecEnum;
import org.apache.shenyu.common.enums.RetryEnum;
import org.apache.shenyu.common.enums.UniqueHeaderEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.LogUtils;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.apache.shenyu.plugin.api.tracing.TraceContextCarrier;
import org.apache.shenyu.plugin.api.tracing.TraceContextPropagator;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.spi.ExtensionLoader;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferLimitException;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;


/**
 * The type abstract http client plugin.
 */
public abstract class AbstractHttpClientPlugin<R> implements ShenyuPlugin {

    protected static final Logger LOG = LoggerFactory.getLogger(AbstractHttpClientPlugin.class);

    /**
     * The propagator of the http protocol, null while tracing is not available.
     */
    private static final TraceContextPropagator TRACE_CONTEXT_PROPAGATOR = loadTraceContextPropagator();

    private final long maxInMemorySize;

    protected AbstractHttpClientPlugin(final long maxInMemorySize) {
        this.maxInMemorySize = maxInMemorySize;
    }

    @Override
    public final Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        final ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        Objects.requireNonNull(shenyuContext);
        final URI uri = exchange.getAttribute(Constants.HTTP_URI);
        if (Objects.isNull(uri)) {
            Object error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.CANNOT_FIND_URL);
            return WebFluxResultUtils.result(exchange, error);
        }
        final long timeout = (long) Optional.ofNullable(exchange.getAttribute(Constants.HTTP_TIME_OUT)).orElse(3000L);
        final Duration duration = Duration.ofMillis(timeout);
        final int retryTimes = (int) Optional.ofNullable(exchange.getAttribute(Constants.HTTP_RETRY)).orElse(0);
        final String retryStrategy = (String) Optional.ofNullable(exchange.getAttribute(Constants.RETRY_STRATEGY)).orElseGet(RetryEnum.CURRENT::getName);
        final String httpMethod = Objects.nonNull(exchange.getRequest().getMethod())
                ? exchange.getRequest().getMethod().name() : "UNKNOWN";
        LogUtils.debug(LOG, () -> String.format("The request urlPath is: %s, retryTimes is : %s, retryStrategy is : %s", uri, retryTimes, retryStrategy));
        final Flux<DataBuffer> requestBody = (retryTimes > 0 && isRequestBodyRequired(httpMethod))
                ? getCachedRequestBody(exchange)
                : exchange.getRequest().getBody();
        final Mono<R> response = doRequest(exchange, httpMethod, uri, requestBody)
                .timeout(duration, Mono.error(() -> new TimeoutException("Response took longer than timeout: " + duration)))
                .doOnError(e -> LOG.error(e.getMessage(), e));
        RetryStrategy<R> strategy;
        String retryStrategyType = (String) Optional.ofNullable(exchange.getAttribute(Constants.HTTP_RETRY_BACK_OFF_SPEC)).orElse(HttpRetryBackoffSpecEnum.getDefault());
        HttpRetryBackoffSpecEnum spec = HttpRetryBackoffSpecEnum.acquireByName(retryStrategyType);
        switch (spec) {
            case EXPONENTIAL_BACKOFF:
                strategy = new ExponentialRetryBackoffStrategy<>(this);
                break;
            case FIXED_BACKOFF:
                strategy = new FixedRetryStrategy<>(this);
                break;
            default:
                strategy = new DefaultRetryStrategy<>(this);
        }
        Mono<R> retriedResponse = strategy.execute(response, exchange, duration, retryTimes);
        return retriedResponse
                .onErrorMap(ShenyuException.class, th -> new ResponseStatusException(HttpStatus.SERVICE_UNAVAILABLE,
                        ShenyuResultEnum.CANNOT_FIND_HEALTHY_UPSTREAM_URL_AFTER_FAILOVER.getMsg(), th))
                .onErrorMap(java.util.concurrent.TimeoutException.class, th -> new ResponseStatusException(HttpStatus.GATEWAY_TIMEOUT, th.getMessage(), th))
                .onErrorMap(DataBufferLimitException.class, th -> new ResponseStatusException(HttpStatus.PAYLOAD_TOO_LARGE,
                        "Request body exceeds the maxInMemorySize limit and cannot be cached for retry", th))
                .flatMap((Function<Object, Mono<? extends Void>>) o -> chain.execute(exchange));
    }

    /**
     * Injects the trace context of the request into the carrier of the outgoing call, so that the
     * upstream service continues the trace of the client.
     *
     * @param exchange the current exchange
     * @param carrier the carrier of the outgoing call
     */
    protected void injectTraceContext(final ServerWebExchange exchange, final TraceContextCarrier carrier) {
        if (Objects.isNull(TRACE_CONTEXT_PROPAGATOR)) {
            return;
        }
        TRACE_CONTEXT_PROPAGATOR.inject(exchange, carrier);
    }

    /**
     * Loads the propagator of the http protocol.
     *
     * <p>Tracing is optional for a deployment, so a propagator which cannot be loaded must never break
     * the http client plugin. Either the tracing module is not on the classpath at all, which is the
     * normal case, or it is there while one of its dependencies is missing: the second case fails
     * while the implementation class is initialized, which raises an error rather than an exception,
     * and it would otherwise fail the initialization of this plugin and take every forwarded request
     * with it.</p>
     *
     * @return the trace context propagator, null while tracing is not available and the trace context
     *         of the client keeps being forwarded as it is
     */
    private static TraceContextPropagator loadTraceContextPropagator() {
        return loadTraceContextPropagator(() -> ExtensionLoader.getExtensionLoader(TraceContextPropagator.class)
                .getJoin(TraceContextPropagator.HTTP));
    }

    /**
     * Loads the propagator of the http protocol from the given lookup, which keeps the failure paths
     * of the call above testable.
     *
     * @param lookup the lookup of the propagator
     * @return the trace context propagator, null while tracing is not available
     */
    static TraceContextPropagator loadTraceContextPropagator(final Supplier<TraceContextPropagator> lookup) {
        try {
            return lookup.get();
        } catch (IllegalArgumentException ex) {
            // no propagator is registered for the http protocol, tracing is not installed
            LOG.debug("shenyu tracing is not available, the trace context of the client is forwarded as it is");
            return null;
        } catch (RuntimeException | LinkageError ex) {
            // tracing is installed but could not be loaded, forwarding is not affected by that
            LOG.warn("shenyu tracing could not load its trace context propagator, the trace context of the client is forwarded as it is", ex);
            return null;
        }
    }

    /**
     * Cache the request body as byte[] so it can be replayed during retry.
     *
     * <p>The original body Flux from the Netty channel is single-use; without caching,
     * retry attempts would send an empty body.
     *
     * <p>Idempotent: the cached Flux is stored in exchange attributes so that both
     * CURRENT (retryWhen) and FAILOVER (resend) paths share the same replayable instance.
     *
     * @param exchange the server web exchange
     * @return a replayable Flux of DataBuffer
     */
    protected Flux<DataBuffer> getCachedRequestBody(final ServerWebExchange exchange) {
        Flux<DataBuffer> cached = exchange.getAttribute(Constants.CACHED_REQUEST_BODY);
        if (Objects.nonNull(cached)) {
            return cached;
        }
        final int joinLimit = (int) Math.min(maxInMemorySize, Integer.MAX_VALUE);
        cached = DataBufferUtils.join(exchange.getRequest().getBody(), joinLimit)
                .map(dataBuffer -> {
                    byte[] bytes = new byte[dataBuffer.readableByteCount()];
                    dataBuffer.read(bytes);
                    DataBufferUtils.release(dataBuffer);
                    return bytes;
                })
                .defaultIfEmpty(new byte[0])
                .cache()
                .flatMapMany(bytes -> Flux.defer(() -> {
                    if (bytes.length == 0) {
                        return Flux.empty();
                    }
                    // ServerHttpRequest has no bufferFactory() API in Spring; the response's
                    // factory shares the same allocator, and NettyHttpClientPlugin.doRequest
                    // requires NettyDataBuffer from it.
                    return Flux.just(exchange.getResponse().bufferFactory().wrap(bytes));
                }));
        exchange.getAttributes().put(Constants.CACHED_REQUEST_BODY, cached);
        return cached;
    }


    /**
     * Process the Web request.
     *
     * @param exchange    the current server exchange
     * @param httpMethod  http method, eg.POST
     * @param uri         the request uri
     * @param body        the request body
     * @return {@code Mono<Void>} to indicate when request processing is complete
     */
    protected abstract Mono<R> doRequest(ServerWebExchange exchange, String httpMethod,
                                         URI uri, Flux<DataBuffer> body);

    protected boolean isRequestBodyRequired(final String httpMethod) {
        return !"GET".equals(httpMethod) && !"HEAD".equals(httpMethod);
    }

    protected boolean shouldFailover(final ServerWebExchange exchange, final int statusCode) {
        String retryStrategy = exchange.getAttribute(Constants.RETRY_STRATEGY);
        return RetryEnum.FAILOVER.getName().equals(retryStrategy) && statusCode >= 500 && statusCode < 600;
    }

    protected void duplicateHeaders(final ServerWebExchange exchange, final HttpHeaders headers, final UniqueHeaderEnum uniqueHeaderEnum) {
        final String duplicateHeader = exchange.getAttribute(uniqueHeaderEnum.getName());
        if (StringUtils.isEmpty(duplicateHeader)) {
            return;
        }
        List<String> duplicateHeaderList = Arrays.asList(StringUtils.split(duplicateHeader, Constants.SEPARATOR_CHARS));
        if (CollectionUtils.isEmpty(duplicateHeaderList)) {
            return;
        }
        HeaderUniqueStrategyEnum strategy = exchange.getAttributeOrDefault(uniqueHeaderEnum.getStrategy(), HeaderUniqueStrategyEnum.RETAIN_FIRST);
        for (String headerKey : duplicateHeaderList) {
            this.duplicate(headers, headerKey, strategy);
        }
    }
    
    protected void duplicate(final HttpHeaders headers, final String header, final HeaderUniqueStrategyEnum strategy) {
        List<String> headerValues = headers.get(header);
        if (Objects.isNull(headerValues) || headerValues.size() <= 1) {
            return;
        }
        switch (strategy) {
            case RETAIN_FIRST:
                headers.set(header, headerValues.get(0));
                break;
            case RETAIN_LAST:
                headers.set(header, headerValues.get(headerValues.size() - 1));
                break;
            case RETAIN_UNIQUE:
                headers.put(header, new ArrayList<>(new LinkedHashSet<>(headerValues)));
                break;
            default:
                throw new IllegalStateException("Unexpected value: " + strategy);
        }
    }
}
