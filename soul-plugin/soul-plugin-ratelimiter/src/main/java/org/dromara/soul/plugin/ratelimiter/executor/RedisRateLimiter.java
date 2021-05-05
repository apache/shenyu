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

package org.dromara.soul.plugin.ratelimiter.executor;

import lombok.extern.slf4j.Slf4j;
import org.dromara.soul.common.dto.convert.RateLimiterHandle;
import org.dromara.soul.plugin.base.utils.Singleton;
import org.dromara.soul.plugin.ratelimiter.algorithm.RateLimiterAlgorithm;
import org.dromara.soul.plugin.ratelimiter.algorithm.RateLimiterAlgorithmFactory;
import org.dromara.soul.plugin.ratelimiter.response.RateLimiterResponse;
import org.springframework.data.redis.core.ReactiveRedisTemplate;
import org.springframework.data.redis.core.script.RedisScript;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * RedisRateLimiter.
 *
 * @author xiaoyu
 */
@Slf4j
public class RedisRateLimiter {
    
    public static final String RATE_LIMITER_ALGORITHM = "rateLimiterAlgorithm";
    
    public static final String RATE_LIMITER_KEYS = "keys";

    public static final String RATE_LIMITER_SCRIPT_ARGS = "scriptArgs";
    
    /**
     * Verify using different current limiting algorithm scripts. 
     *
     * @param exchange the current server exchange
     * @param id is rule id
     * @param limiterHandle the limiter handle
     * @return {@code Mono<RateLimiterResponse>} to indicate when request processing is complete
     */
    @SuppressWarnings("unchecked")
    public Mono<RateLimiterResponse> isAllowed(final ServerWebExchange exchange, final String id, final RateLimiterHandle limiterHandle) {
        double replenishRate = limiterHandle.getReplenishRate();
        double burstCapacity = limiterHandle.getBurstCapacity();
        double requestCount = limiterHandle.getRequestCount();
        RateLimiterAlgorithm<?> rateLimiterAlgorithm = RateLimiterAlgorithmFactory.newInstance(limiterHandle.getAlgorithmName());
        RedisScript<?> script = rateLimiterAlgorithm.getScript();
        List<String> keys = rateLimiterAlgorithm.getKeys(id);
        List<String> scriptArgs = Arrays.asList(doubleToString(replenishRate), doubleToString(burstCapacity), doubleToString(Instant.now().getEpochSecond()), doubleToString(requestCount));
        Flux<List<Long>> resultFlux = Singleton.INST.get(ReactiveRedisTemplate.class).execute(script, keys, scriptArgs);
        return resultFlux.onErrorResume(throwable -> Flux.just(Arrays.asList(1L, -1L)))
                .reduce(new ArrayList<Long>(), (longs, l) -> {
                    longs.addAll(l);
                    return longs;
                }).map(results -> {
                    boolean allowed = results.get(0) == 1L;
                    if (allowed) {
                        exchange.getAttributes().put(RATE_LIMITER_ALGORITHM, rateLimiterAlgorithm);
                        exchange.getAttributes().put(RATE_LIMITER_KEYS, keys);
                        exchange.getAttributes().put(RATE_LIMITER_SCRIPT_ARGS, scriptArgs);
                    }
                    Long tokensLeft = results.get(1);
                    return new RateLimiterResponse(allowed, tokensLeft);
                })
                .doOnError(throwable -> log.error("Error determining if user allowed from redis:{}", throwable.getMessage()));
    }
    
    private String doubleToString(final double param) {
        return String.valueOf(param);
    }
}
