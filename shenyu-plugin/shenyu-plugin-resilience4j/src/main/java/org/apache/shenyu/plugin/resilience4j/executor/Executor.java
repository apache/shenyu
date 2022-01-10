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

package org.apache.shenyu.plugin.resilience4j.executor;

import io.github.resilience4j.circuitbreaker.CallNotPermittedException;
import io.github.resilience4j.ratelimiter.RequestNotPermitted;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.common.utils.UriUtils;
import org.apache.shenyu.plugin.resilience4j.Resilience4JPlugin;
import org.apache.shenyu.plugin.resilience4j.conf.Resilience4JConf;
import org.springframework.http.HttpStatus;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.web.reactive.DispatcherHandler;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Objects;
import java.util.concurrent.TimeoutException;
import java.util.function.Function;

/**
 * Executor.
 */
public interface Executor {

    /**
     * resilience run.
     *
     * @param toRun    the toRun
     * @param fallback the fallback
     * @param conf     the conf
     * @param <T>      the t
     * @return mono
     */
    <T> Mono<T> run(Mono<T> toRun, Function<Throwable, Mono<T>> fallback, Resilience4JConf conf);

    /**
     * do fallback.
     *
     * @param exchange the exchange
     * @param uri      the uri
     * @param t        the t
     * @return Mono
     */
    default Mono<Void> fallback(ServerWebExchange exchange, String uri, Throwable t) {
        if (StringUtils.isBlank(uri)) {
            return withoutFallback(exchange, t);
        }
        DispatcherHandler dispatcherHandler = SpringBeanUtils.getInstance().getBean(DispatcherHandler.class);
        ServerHttpRequest request = exchange.getRequest().mutate().uri(Objects.requireNonNull(UriUtils.createUri(uri))).build();
        ServerWebExchange mutated = exchange.mutate().request(request).build();
        return dispatcherHandler.handle(mutated);
    }

    /**
     * do fallback with not  fallback method.
     *
     * @param exchange the exchange
     * @param throwable the throwable
     * @return Mono
     */
    default Mono<Void> withoutFallback(ServerWebExchange exchange, Throwable throwable) {
        Object error;
        if (throwable instanceof TimeoutException) {
            exchange.getResponse().setStatusCode(HttpStatus.GATEWAY_TIMEOUT);
            error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.SERVICE_TIMEOUT, null);
        } else if (throwable instanceof Resilience4JPlugin.CircuitBreakerStatusCodeException) {
            return Mono.error(throwable);
        } else if (throwable instanceof CallNotPermittedException) {
            exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
            error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.SERVICE_RESULT_ERROR, null);
        } else if (throwable instanceof RequestNotPermitted) {
            exchange.getResponse().setStatusCode(HttpStatus.TOO_MANY_REQUESTS);
            error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.TOO_MANY_REQUESTS, null);
        } else {
            exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
            error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.SERVICE_RESULT_ERROR, null);
        }
        return WebFluxResultUtils.result(exchange, error);
    }

    /**
     * default error.
     *
     * @param exchange the exchange
     * @return Mono
     */
    default Mono<Void> error(ServerWebExchange exchange) {
        exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
        Object error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.SERVICE_RESULT_ERROR, null);
        return WebFluxResultUtils.result(exchange, error);
    }
}
