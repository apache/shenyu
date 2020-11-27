package org.dromara.soul.plugin.resilience4j.conf;

import io.github.resilience4j.circuitbreaker.CircuitBreakerConfig;
import io.github.resilience4j.ratelimiter.RateLimiterConfig;
import io.github.resilience4j.timelimiter.TimeLimiterConfig;
import lombok.Data;

/**
 * conf.
 *
 * @author zhanglei
 */
@Data
public class ResilienceConf {

    /**
     * id.
     */
    private String id;

    /**
     * fallBackUri.
     */
    private String fallBackUri;

    /**
     * timeLimiterConfig.
     */
    private TimeLimiterConfig timeLimiterConfig;

    /**
     * circuitBreakerConfig.
     */
    private CircuitBreakerConfig circuitBreakerConfig;

    /**
     * rateLimiterConfig.
     */
    private RateLimiterConfig rateLimiterConfig;

    public ResilienceConf(final String id,
                          final String fallBackUri,
                          final RateLimiterConfig rateLimiterConfig,
                          final TimeLimiterConfig timeLimiterConfig,
                          final CircuitBreakerConfig circuitBreakerConfig) {
        this.id = id;
        this.fallBackUri = fallBackUri;
        this.rateLimiterConfig = rateLimiterConfig;
        this.timeLimiterConfig = timeLimiterConfig;
        this.circuitBreakerConfig = circuitBreakerConfig;
    }
}
