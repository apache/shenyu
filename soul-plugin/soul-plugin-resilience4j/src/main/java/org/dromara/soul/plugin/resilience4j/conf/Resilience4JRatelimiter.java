package org.dromara.soul.plugin.resilience4j.conf;

import io.github.resilience4j.ratelimiter.RateLimiter;
import io.github.resilience4j.ratelimiter.RateLimiterConfig;
import io.github.resilience4j.ratelimiter.RateLimiterRegistry;
import io.github.resilience4j.reactor.ratelimiter.operator.RateLimiterOperator;
import reactor.core.publisher.Mono;

import java.util.function.Function;

public class Resilience4JRatelimiter implements ReactiveCall {


    private final String id;
    private final RateLimiterConfig rateLimiterConfig;
    private final RateLimiterRegistry rateLimiterRegistry;

    public Resilience4JRatelimiter(final String id,
                                   final RateLimiterConfig rateLimiterConfig,
                                   final RateLimiterRegistry rateLimiterRegistry){
        this.id = id;
        this.rateLimiterConfig = rateLimiterConfig;
        this.rateLimiterRegistry = rateLimiterRegistry;

    }
    @Override
    public <T> Mono<T> run(Mono<T> toRun, Function<Throwable, Mono<T>> fallback) {
        RateLimiter rateLimiter = rateLimiterRegistry.rateLimiter(id,rateLimiterConfig);
        return toRun.transform(RateLimiterOperator.of(rateLimiter));
    }
}
