package org.dromara.soul.plugin.resilience4j.factory;

import io.github.resilience4j.circuitbreaker.CircuitBreaker;
import io.github.resilience4j.circuitbreaker.CircuitBreakerConfig;
import io.github.resilience4j.circuitbreaker.CircuitBreakerRegistry;
import io.github.resilience4j.ratelimiter.RateLimiter;
import io.github.resilience4j.ratelimiter.RateLimiterConfig;
import io.github.resilience4j.ratelimiter.RateLimiterRegistry;

/**
 * ResilienceRegistryFactory.
 *
 * @Author zhanglei
 */
public class ResilienceRegistryFactory {


    /**
     * rateLimiterRegistry.
     */
    private static final RateLimiterRegistry rateLimiterRegistry = RateLimiterRegistry.ofDefaults();

    /**
     * circuitBreakerRegistry.
     */
    private static final CircuitBreakerRegistry circuitBreakerRegistry = CircuitBreakerRegistry.ofDefaults();

    /**
     * circuitBreaker.
     *
     * @param id
     * @param circuitBreakerConfig
     * @return CircuitBreaker
     */
    public static CircuitBreaker circuitBreaker(String id, CircuitBreakerConfig circuitBreakerConfig) {
        return circuitBreakerRegistry.circuitBreaker(id, circuitBreakerConfig);
    }

    /**
     * rateLimiter.
     *
     * @param id
     * @param rateLimiterConfig
     * @return RateLimiter
     */
    public static RateLimiter rateLimiter(String id, RateLimiterConfig rateLimiterConfig) {
        return rateLimiterRegistry.rateLimiter(id, rateLimiterConfig);
    }

    public static void remove(String id) {
        rateLimiterRegistry.remove(id);
        circuitBreakerRegistry.remove(id);
    }

}
