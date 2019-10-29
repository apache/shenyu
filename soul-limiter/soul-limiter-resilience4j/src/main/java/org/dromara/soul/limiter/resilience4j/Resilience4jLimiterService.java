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

package org.dromara.soul.limiter.resilience4j;

import io.github.resilience4j.ratelimiter.RateLimiter;
import io.github.resilience4j.ratelimiter.RateLimiterConfig;
import io.github.resilience4j.ratelimiter.RateLimiterRegistry;
import org.dromara.soul.limiter.api.LimiterService;

import java.time.Duration;

/**
 * The type Resilience 4 j limiter service.
 *
 * @author xiaoyu
 */
public class Resilience4jLimiterService implements LimiterService {

    @Override
    public Boolean isAllowed(String key, double replenishRate, double burstCapacity) {
        RateLimiterRegistry rateLimiterRegistry = RateLimiterRegistry.ofDefaults();
        RateLimiterConfig config = RateLimiterConfig.custom()
                .limitRefreshPeriod(Duration.ofMillis(1000))
                .limitForPeriod((int) replenishRate)
                .timeoutDuration(Duration.ofMillis(1000))
                .build();
        RateLimiter rateLimiter = rateLimiterRegistry
                .rateLimiter(key, config);
        return rateLimiter.acquirePermission();
    }
}
