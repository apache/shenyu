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

package org.apache.shenyu.plugin.ratelimiter.algorithm;

import org.apache.shenyu.common.enums.RateLimitEnum;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.spi.Join;
import org.springframework.data.redis.core.ReactiveRedisTemplate;
import org.springframework.data.redis.core.script.RedisScript;

import java.util.Arrays;
import java.util.List;

/**
 * The type Concurrent rate limiter algorithm.
 *
 * @see <a href="https://stripe.com/blog/rate-limiters">rate-limiters</a>
 * @see <a href="https://gist.github.com/ptarjan/e38f45f2dfe601419ca3af937fff574d#file-1-check_request_rate_limiter-rb-L11-L34">check_request_rate_limiter</a>
 */
@Join
public class ConcurrentRateLimiterAlgorithm extends AbstractRateLimiterAlgorithm {

    @Override
    protected String getScriptName() {
        return RateLimitEnum.CONCURRENT.getScriptName();
    }

    @Override
    protected String getKeyName() {
        return RateLimitEnum.CONCURRENT.getKeyName();
    }

    @Override
    public List<String> getKeys(final String id) {
        String tokenKey = getKeyName() + ".{" + id + "}.tokens";
        String requestKey = UUIDUtils.getInstance().generateShortUuid();
        return Arrays.asList(tokenKey, requestKey);
    }

    @Override
    @SuppressWarnings("unchecked")
    public void callback(final RedisScript<?> script, final List<String> keys, final List<String> scriptArgs) {
        Singleton.INST.get(ReactiveRedisTemplate.class).opsForZSet().remove(keys.get(0), keys.get(1)).subscribe();
    }
}
