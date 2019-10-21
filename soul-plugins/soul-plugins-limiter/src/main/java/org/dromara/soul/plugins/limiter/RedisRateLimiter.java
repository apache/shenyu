/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * Contributor license agreements.See the NOTICE file distributed with
 * This work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * he License.You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.dromara.soul.plugins.limiter;

import org.dromara.soul.common.utils.LogUtils;
import org.dromara.soul.plugins.limiter.jedis.JedisClient;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.time.Instant;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;

/**
 * See https://stripe.com/blog/rate-limiters and
 * https://gist.github.com/ptarjan/e38f45f2dfe601419ca3af937fff574d#file-1-check_request_rate_limiter-rb-L11-L34
 * See  https://github.com/spring-cloud/spring-cloud-gateway/blob/master/spring-cloud-gateway-core/src/main/java/org/springframework/cloud/gateway/filter/ratelimit/RedisRateLimiter.java
 * RedisRateLimiter.
 *
 * @author xiaoyu
 */
class RedisRateLimiter {

    /**
     * logger.
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(RedisRateLimiter.class);

    private AtomicBoolean initialized = new AtomicBoolean(false);

    private JedisClient jedisClient;

    private String script;

    /**
     * Instantiates a new Redis rate limiter.
     */
    RedisRateLimiter(JedisClient jedisClient) {
        initialized.compareAndSet(false, true);
        this.jedisClient = jedisClient;
        InputStream inputStream = RedisRateLimiter.class.getClassLoader()
                .getResourceAsStream("scripts/request_rate_limiter.lua");
        if (Objects.nonNull(inputStream)) {
            script = new BufferedReader(new InputStreamReader(inputStream)).lines()
                    .collect(Collectors.joining(System.lineSeparator()));
        }
    }

    /**
     * This uses a basic token bucket algorithm and relies on the fact that Redis scripts
     * execute atomically. No other operations can run between fetching the count and
     * writing the new count.
     *
     * @param id            is rule id
     * @param replenishRate replenishRate
     * @param burstCapacity burstCapacity
     * @return {@code Mono<Response>} to indicate when request processing is complete
     */
    RateLimiterResponse isAllowed(final String id, final double replenishRate, final double burstCapacity) {
        if (!this.initialized.get()) {
            throw new IllegalStateException("RedisRateLimiter is not initialized");
        }
        try {
            List<String> keys = getKeys(id);
            List<String> scriptArgs = Arrays.asList(replenishRate + "", burstCapacity + "",
                    Instant.now().getEpochSecond() + "", "1");
            return (RateLimiterResponse) jedisClient.evalsha(script, keys, scriptArgs);
        } catch (Exception e) {
            e.printStackTrace();
            LogUtils.error(LOGGER, () -> "Error determining if user allowed from redis" + e.getMessage());
        }
        return new RateLimiterResponse(true, -1);
    }

    private static List<String> getKeys(final String id) {
        String prefix = "request_rate_limiter.{" + id;
        String tokenKey = prefix + "}.tokens";
        String timestampKey = prefix + "}.timestamp";
        return Arrays.asList(tokenKey, timestampKey);
    }


}
