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

package org.dromara.soul.limiter.guava;

import com.google.common.util.concurrent.RateLimiter;
import org.dromara.soul.limiter.api.LimiterService;

import java.util.concurrent.ConcurrentHashMap;

/**
 * The type Guava limiter service.
 *
 * @author xiaoyu
 */
public class GuavaLimiterService implements LimiterService {

    private static final ConcurrentHashMap<String, RateLimiter> LIMITER_MAP = new ConcurrentHashMap<>();

    @Override
    public Boolean isAllowed(String key, double replenishRate, double burstCapacity) {
        final RateLimiter rateLimiter;
        if (LIMITER_MAP.containsKey(key)) {
            rateLimiter = LIMITER_MAP.get(key);
        } else {
            rateLimiter = RateLimiter.create(replenishRate);
            LIMITER_MAP.put(key, rateLimiter);
        }
        return rateLimiter.tryAcquire();
    }

}
