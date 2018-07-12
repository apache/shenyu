/*
 *
 *  * Licensed to the Apache Software Foundation (ASF) under one or more
 *  * contributor license agreements.  See the NOTICE file distributed with
 *  * this work for additional information regarding copyright ownership.
 *  * The ASF licenses this file to You under the Apache License, Version 2.0
 *  * (the "License"); you may not use this file except in compliance with
 *  * the License.  You may obtain a copy of the License at
 *  *
 *  *     http://www.apache.org/licenses/LICENSE-2.0
 *  *
 *  * Unless required by applicable law or agreed to in writing, software
 *  * distributed under the License is distributed on an "AS IS" BASIS,
 *  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  * See the License for the specific language governing permissions and
 *  * limitations under the License.
 *
 */

package org.dromara.soul.web.plugin.function;

import com.hqyg.skyway.api.convert.RateLimiterHandle;
import com.hqyg.skyway.api.dto.zk.RuleZkDTO;
import com.hqyg.skyway.common.constant.Constants;
import com.hqyg.skyway.common.enums.PluginEnum;
import com.hqyg.skyway.common.enums.PluginTypeEnum;
import com.hqyg.skyway.common.utils.GSONUtils;
import com.hqyg.skyway.web.cache.DataCacheManager;
import com.hqyg.skyway.web.plugin.AbstractSkywayPlugin;
import com.hqyg.skyway.web.plugin.SkywayPluginChain;
import com.hqyg.skyway.web.plugin.ratelimter.RedisRateLimiter;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

/**
 * RateLimiter Plugin.
 * @author xiaoyu(Myth)
 */
public class RateLimiterSkywayPlugin extends AbstractSkywayPlugin {

    private final RedisRateLimiter redisRateLimiter;

    public RateLimiterSkywayPlugin(final DataCacheManager dataCacheManager,
                                   final RedisRateLimiter redisRateLimiter) {
        super(dataCacheManager);
        this.redisRateLimiter = redisRateLimiter;
    }

    @Override
    public String named() {
        return PluginEnum.RATE_LIMITER.getName();
    }

    /**
     * return plugin type.
     *
     * @return {@linkplain PluginTypeEnum}
     */
    @Override
    public PluginTypeEnum pluginType() {
        return PluginTypeEnum.FUNCTION;
    }

    @Override
    public int getOrder() {
        return PluginEnum.RATE_LIMITER.getCode();
    }

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final SkywayPluginChain chain, final RuleZkDTO rule) {

        final String handle = rule.getHandle();

        final RateLimiterHandle limiterHandle = GSONUtils.getInstance().fromJson(handle, RateLimiterHandle.class);

        return redisRateLimiter.isAllowed(rule.getId(), limiterHandle.getReplenishRate(), limiterHandle.getBurstCapacity())
                .flatMap(response -> {
                    if (!response.isAllowed()) {
                        exchange.getResponse().setStatusCode(HttpStatus.TOO_MANY_REQUESTS);
                        return exchange.getResponse().writeWith(Mono.just(exchange.getResponse().bufferFactory().wrap(Constants.TOO_MANY_REQUESTS.getBytes())));
                    }
                    return chain.execute(exchange);
                });
    }

}
