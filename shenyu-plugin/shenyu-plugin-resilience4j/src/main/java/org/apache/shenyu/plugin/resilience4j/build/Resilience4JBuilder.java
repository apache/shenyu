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

package org.apache.shenyu.plugin.resilience4j.build;

import io.github.resilience4j.circuitbreaker.CircuitBreakerConfig;
import io.github.resilience4j.core.IntervalFunction;
import io.github.resilience4j.ratelimiter.RateLimiterConfig;
import io.github.resilience4j.timelimiter.TimeLimiterConfig;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.convert.rule.Resilience4JHandle;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.resilience4j.conf.Resilience4JConf;

import java.time.Duration;

/**
 * Resilience4j builder.
 */
public class Resilience4JBuilder {

    /**
     * build.
     *
     * @param ruleData the ruleData
     * @return Resilience4JConf
     */
    public static Resilience4JConf build(final RuleData ruleData) {
        Resilience4JHandle handle = GsonUtils.getGson().fromJson(ruleData.getHandle(), Resilience4JHandle.class);
        handle.checkData(handle);
        CircuitBreakerConfig circuitBreakerConfig = null;
        if (handle.getCircuitEnable() == 1) {
            circuitBreakerConfig = CircuitBreakerConfig.custom()
                    .recordExceptions(Throwable.class, Exception.class)
                    .failureRateThreshold(handle.getFailureRateThreshold())
                    .automaticTransitionFromOpenToHalfOpenEnabled(handle.getAutomaticTransitionFromOpenToHalfOpenEnabled())
                    .slidingWindow(handle.getSlidingWindowSize(), handle.getMinimumNumberOfCalls(),
                            handle.getSlidingWindowType() == 0
                                    ? CircuitBreakerConfig.SlidingWindowType.COUNT_BASED
                                    : CircuitBreakerConfig.SlidingWindowType.TIME_BASED).waitIntervalFunctionInOpenState(IntervalFunction
                            .of(Duration.ofSeconds(handle.getWaitIntervalFunctionInOpenState() / 1000)))
                    .permittedNumberOfCallsInHalfOpenState(handle.getPermittedNumberOfCallsInHalfOpenState()).build();
        }
        TimeLimiterConfig timeLimiterConfig = TimeLimiterConfig.custom()
                .timeoutDuration(Duration.ofSeconds(handle.getTimeoutDuration() / 1000)).build();
        RateLimiterConfig rateLimiterConfig = RateLimiterConfig.custom()
                .limitForPeriod(handle.getLimitForPeriod())
                .timeoutDuration(Duration.ofSeconds(handle.getTimeoutDurationRate() / 1000))
                .limitRefreshPeriod(Duration.ofNanos(handle.getLimitRefreshPeriod() * 1000000)).build();
        return new Resilience4JConf(CacheKeyUtils.INST.getKey(ruleData), handle.getFallbackUri(), rateLimiterConfig, timeLimiterConfig, circuitBreakerConfig);
    }
}
