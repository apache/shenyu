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

package org.apache.shenyu.plugin.resilience4j;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.Resilience4JHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.resilience4j.build.Resilience4JBuilder;
import org.apache.shenyu.plugin.resilience4j.conf.Resilience4JConf;
import org.apache.shenyu.plugin.resilience4j.executor.CombinedExecutor;
import org.apache.shenyu.plugin.resilience4j.executor.Executor;
import org.apache.shenyu.plugin.resilience4j.executor.RateLimiterExecutor;
import org.apache.shenyu.plugin.resilience4j.handler.Resilience4JHandler;
import org.springframework.http.HttpStatus;
import org.springframework.web.client.HttpStatusCodeException;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.function.Function;

/**
 * Resilience4J plugin.
 */
public class Resilience4JPlugin extends AbstractShenyuPlugin {

    private final CombinedExecutor combinedExecutor;

    private final RateLimiterExecutor ratelimiterExecutor;

    public Resilience4JPlugin(final CombinedExecutor combinedExecutor,
                              final RateLimiterExecutor ratelimiterExecutor) {
        this.combinedExecutor = combinedExecutor;
        this.ratelimiterExecutor = ratelimiterExecutor;
    }

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
        final ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        assert shenyuContext != null;
        Resilience4JHandle resilience4JHandle = Resilience4JHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(rule));
        resilience4JHandle.checkData(resilience4JHandle);
        if (resilience4JHandle.getCircuitEnable() == 1) {
            return combined(exchange, chain, rule);
        }
        return rateLimiter(exchange, chain, rule);
    }

    private Mono<Void> rateLimiter(final ServerWebExchange exchange, final ShenyuPluginChain chain, final RuleData rule) {
        return ratelimiterExecutor.run(
                chain.execute(exchange), fallback(ratelimiterExecutor, exchange, null), Resilience4JBuilder.build(rule))
                .onErrorResume(throwable -> ratelimiterExecutor.withoutFallback(exchange, throwable));
    }

    private Mono<Void> combined(final ServerWebExchange exchange, final ShenyuPluginChain chain, final RuleData rule) {
        Resilience4JConf conf = Resilience4JBuilder.build(rule);
        return combinedExecutor.run(
                chain.execute(exchange).doOnSuccess(v -> {
                    HttpStatus status = exchange.getResponse().getStatusCode();
                    if (status == null || !status.is2xxSuccessful()) {
                        exchange.getResponse().setStatusCode(null);
                        throw new CircuitBreakerStatusCodeException(status == null ? HttpStatus.INTERNAL_SERVER_ERROR : status);
                    }
                }), fallback(combinedExecutor, exchange, conf.getFallBackUri()), conf);
    }

    private Function<Throwable, Mono<Void>> fallback(final Executor executor,
                                                     final ServerWebExchange exchange, final String uri) {
        return throwable -> executor.fallback(exchange, uri, throwable);
    }

    @Override
    public int getOrder() {
        return PluginEnum.RESILIENCE4J.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.RESILIENCE4J.getName();
    }

    public static class CircuitBreakerStatusCodeException extends HttpStatusCodeException {

        public CircuitBreakerStatusCodeException(final HttpStatus statusCode) {
            super(statusCode);
        }
    }
}
