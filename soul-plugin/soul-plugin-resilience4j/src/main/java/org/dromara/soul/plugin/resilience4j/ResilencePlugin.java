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

package org.dromara.soul.plugin.resilience4j;

import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.dto.convert.ResilienceHandle;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.plugin.api.SoulPluginChain;
import org.dromara.soul.plugin.api.context.SoulContext;
import org.dromara.soul.plugin.base.AbstractSoulPlugin;
import org.dromara.soul.plugin.resilience4j.build.ResilienceBuilder;
import org.dromara.soul.plugin.resilience4j.conf.ResilienceConf;
import org.dromara.soul.plugin.resilience4j.executor.CombinedExecutor;
import org.dromara.soul.plugin.resilience4j.executor.Executor;
import org.dromara.soul.plugin.resilience4j.executor.RatelimiterExecutor;
import org.springframework.http.HttpStatus;
import org.springframework.web.client.HttpStatusCodeException;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.function.Function;

/**
 * ResilencePlugin.
 *
 * @author zhanglei
 */
public class ResilencePlugin extends AbstractSoulPlugin {

    private final CombinedExecutor combinedExecutor;

    private final RatelimiterExecutor ratelimiterExecutor;

    public ResilencePlugin(final CombinedExecutor combinedExecutor,
                           final RatelimiterExecutor ratelimiterExecutor) {
        this.combinedExecutor = combinedExecutor;
        this.ratelimiterExecutor = ratelimiterExecutor;
    }

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final SoulPluginChain chain, final SelectorData selector, final RuleData rule) {
        final SoulContext soulContext = exchange.getAttribute(Constants.CONTEXT);
        assert soulContext != null;
        ResilienceHandle resilienceHandle = GsonUtils.getGson().fromJson(rule.getHandle(), ResilienceHandle.class);
        if (resilienceHandle.getCircuitEnable() == 1) {
            return combined(exchange, chain, rule);
        }
        return ratelimiter(exchange, chain, rule);
    }

    private Mono<Void> ratelimiter(final ServerWebExchange exchange, final SoulPluginChain chain, final RuleData rule) {
        return ratelimiterExecutor.run(
                chain.execute(exchange), fallback(ratelimiterExecutor, exchange, null), ResilienceBuilder.build(rule))
                .onErrorResume(throwable -> {
                    return ratelimiterExecutor.withoutFallback(exchange, throwable);
                });
    }

    private Mono<Void> combined(final ServerWebExchange exchange, final SoulPluginChain chain, final RuleData rule) {
        ResilienceConf conf = ResilienceBuilder.build(rule);
        return combinedExecutor.run(
                chain.execute(exchange).doOnSuccess(v -> {
                    if (exchange.getResponse().getStatusCode() != HttpStatus.OK) {
                        HttpStatus status = exchange.getResponse().getStatusCode();
                        exchange.getResponse().setStatusCode(null);
                        throw new CircuitBreakerStatusCodeException(status);
                    }
                }), fallback(combinedExecutor, exchange, conf.getFallBackUri()), conf);
    }

    private Function<Throwable, Mono<Void>> fallback(final Executor executor,
                                                     final ServerWebExchange exchange, final String uri) {
        return throwable -> {
            return executor.fallback(exchange, uri, throwable);
        };
    }

    @Override
    public int getOrder() {
        return PluginEnum.Resilence4J.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.Resilence4J.getName();
    }

    public class CircuitBreakerStatusCodeException extends HttpStatusCodeException {

        public CircuitBreakerStatusCodeException(final HttpStatus statusCode) {
            super(statusCode);
        }

    }
}
