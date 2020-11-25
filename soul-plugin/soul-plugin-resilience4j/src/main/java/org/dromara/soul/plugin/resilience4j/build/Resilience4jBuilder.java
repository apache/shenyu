package org.dromara.soul.plugin.resilience4j.build;

import io.github.resilience4j.circuitbreaker.CircuitBreakerConfig;
import io.github.resilience4j.ratelimiter.RateLimiterConfig;
import io.github.resilience4j.timelimiter.TimeLimiterConfig;
import org.dromara.soul.common.dto.convert.Resilience4JHandle;
import org.dromara.soul.plugin.resilience4j.conf.Resilience4jConf;

import java.time.Duration;


public class Resilience4jBuilder {

    public static Resilience4jConf circuitBreaker(Resilience4JHandle resilience4JHandle) {
       TimeLimiterConfig timeLimiterConfig = TimeLimiterConfig
                .custom()
                .timeoutDuration(Duration.ofSeconds(resilience4JHandle.getTimeoutDuration()))
                .build();
        CircuitBreakerConfig circuitBreakerConfig = CircuitBreakerConfig
                .custom()
                .build();
        return new Resilience4jConf(timeLimiterConfig,circuitBreakerConfig);
    }

    public static RateLimiterConfig rateLimiter(Resilience4JHandle resilience4JHandle){
        return RateLimiterConfig.custom().build();
    }
}
