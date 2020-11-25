package org.dromara.soul.plugin.resilience4j.conf;

import io.github.resilience4j.circuitbreaker.CircuitBreakerConfig;
import io.github.resilience4j.timelimiter.TimeLimiterConfig;
import lombok.Data;

/**
 * @author zhanglei
 */
@Data
public class Resilience4jConf {

    private TimeLimiterConfig timeLimiterConfig;

    private CircuitBreakerConfig circuitBreakerConfig;


    public Resilience4jConf(final TimeLimiterConfig timeLimiterConfig,
                            final CircuitBreakerConfig circuitBreakerConfig) {
        this.timeLimiterConfig = timeLimiterConfig;
        this.circuitBreakerConfig = circuitBreakerConfig;
    }
}
