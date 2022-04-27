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

package org.apache.shenyu.plugin.ratelimiter;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.RateLimiterHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.ratelimiter.executor.RedisRateLimiter;
import org.apache.shenyu.plugin.ratelimiter.handler.RateLimiterPluginDataHandler;
import org.apache.shenyu.plugin.ratelimiter.resolver.RateLimiterKeyResolverFactory;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Optional;

/**
 * RateLimiter Plugin.
 */
public class RateLimiterPlugin extends AbstractShenyuPlugin {

    private final RedisRateLimiter redisRateLimiter;

    /**
     * Instantiates a new Rate limiter plugin.
     *
     * @param redisRateLimiter  the redis rate limiter
     */
    public RateLimiterPlugin(final RedisRateLimiter redisRateLimiter) {
        this.redisRateLimiter = redisRateLimiter;
    }

    @Override
    public String named() {
        return PluginEnum.RATE_LIMITER.getName();
    }

    @Override
    public int getOrder() {
        return PluginEnum.RATE_LIMITER.getCode();
    }

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
        RateLimiterHandle limiterHandle = RateLimiterPluginDataHandler.CACHED_HANDLE.get()
                .obtainHandle(CacheKeyUtils.INST.getKey(rule));
        String resolverKey = Optional.ofNullable(limiterHandle.getKeyResolverName())
                .flatMap(name -> Optional.of("-" + RateLimiterKeyResolverFactory.newInstance(name).resolve(exchange)))
                .orElse("");
        return redisRateLimiter.isAllowed(rule.getId() + resolverKey, limiterHandle)
                .flatMap(response -> {
                    if (!response.isAllowed()) {
                        exchange.getResponse().setStatusCode(HttpStatus.TOO_MANY_REQUESTS);
                        Object error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.TOO_MANY_REQUESTS);
                        return WebFluxResultUtils.result(exchange, error);
                    }
                    return chain.execute(exchange);
                });
    }
}
