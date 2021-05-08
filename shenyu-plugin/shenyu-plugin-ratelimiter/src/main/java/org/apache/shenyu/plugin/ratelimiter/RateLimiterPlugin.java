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
import org.apache.shenyu.common.dto.convert.RateLimiterHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.SoulPluginChain;
import org.apache.shenyu.plugin.api.result.SoulResultEnum;
import org.apache.shenyu.plugin.api.result.SoulResultWrap;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.base.AbstractSoulPlugin;
import org.apache.shenyu.plugin.ratelimiter.cache.RatelimiterRuleHandleCache;
import org.apache.shenyu.plugin.ratelimiter.executor.RedisRateLimiter;
import org.apache.shenyu.plugin.ratelimiter.handler.RateLimiterPluginDataHandler;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

/**
 * RateLimiter Plugin.
 */
public class RateLimiterPlugin extends AbstractSoulPlugin {

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
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final SoulPluginChain chain, final SelectorData selector, final RuleData rule) {
        RateLimiterHandle limiterHandle = RatelimiterRuleHandleCache.getInstance()
                .obtainHandle(RateLimiterPluginDataHandler.getCacheKeyName(rule));
        return redisRateLimiter.isAllowed(rule.getId(), limiterHandle)
                .flatMap(response -> {
                    if (!response.isAllowed()) {
                        exchange.getResponse().setStatusCode(HttpStatus.TOO_MANY_REQUESTS);
                        Object error = SoulResultWrap.error(SoulResultEnum.TOO_MANY_REQUESTS.getCode(), SoulResultEnum.TOO_MANY_REQUESTS.getMsg(), null);
                        return WebFluxResultUtils.result(exchange, error);
                    }
                    return chain.execute(exchange);
                });
    }
}
