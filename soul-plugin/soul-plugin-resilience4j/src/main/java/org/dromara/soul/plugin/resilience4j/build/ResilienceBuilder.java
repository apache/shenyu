package org.dromara.soul.plugin.resilience4j.build;

import io.github.resilience4j.circuitbreaker.CircuitBreakerConfig;
import io.github.resilience4j.core.IntervalFunction;
import io.github.resilience4j.ratelimiter.RateLimiterConfig;
import io.github.resilience4j.timelimiter.TimeLimiterConfig;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.convert.ResilienceHandle;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.plugin.resilience4j.conf.ResilienceConf;
import org.dromara.soul.plugin.resilience4j.handler.ResilienceHandler;

import java.time.Duration;

/**
 * Resilience4jBuilder.
 *
 * @Author zhanglei
 */
public class ResilienceBuilder {

    /**
     * build.
     *
     * @param ruleData
     * @return Resilience4jConf .
     */
    public static ResilienceConf build(final RuleData ruleData) {
        ResilienceHandle handle = GsonUtils.getGson().fromJson(ruleData.getHandle(), ResilienceHandle.class);
        CircuitBreakerConfig circuitBreakerConfig = null;
        if (handle.getCircuitEnable() == 1) {
            circuitBreakerConfig = CircuitBreakerConfig.custom()
                    .failureRateThreshold(handle.getFailureRateThreshold())
                    .automaticTransitionFromOpenToHalfOpenEnabled(handle.getAutomaticTransitionFromOpenToHalfOpenEnabled())
                    .slidingWindow(handle.getSlidingWindowSize(), handle.getMinimumNumberOfCalls(),
                            handle.getSlidingWindowType() == 0
                                    ? CircuitBreakerConfig.SlidingWindowType.COUNT_BASED
                                    : CircuitBreakerConfig.SlidingWindowType.TIME_BASED).waitIntervalFunctionInOpenState(IntervalFunction
                            .of(Duration.ofSeconds(handle.getWaitIntervalFunctionInOpenState())))
                    .permittedNumberOfCallsInHalfOpenState(handle.getPermittedNumberOfCallsInHalfOpenState()).build();
        }
        TimeLimiterConfig timeLimiterConfig = TimeLimiterConfig.custom()
                .timeoutDuration(Duration.ofSeconds(handle.getTimeoutDuration())).build();
        RateLimiterConfig rateLimiterConfig = RateLimiterConfig.custom()
                .limitForPeriod(handle.getLimitForPeriod())
                .timeoutDuration(Duration.ofSeconds(handle.getTimeoutDuration()))
                .limitRefreshPeriod(Duration.ofNanos(handle.getLimitRefreshPeriod())).build();
        return new ResilienceConf(ResilienceHandler.getResourceName(ruleData), handle.getFallbackUri(), rateLimiterConfig, timeLimiterConfig, circuitBreakerConfig);
    }
}
