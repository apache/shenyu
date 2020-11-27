package org.dromara.soul.plugin.resilience4j.executor;

import io.github.resilience4j.ratelimiter.RateLimiter;
import io.github.resilience4j.reactor.ratelimiter.operator.RateLimiterOperator;
import org.dromara.soul.plugin.resilience4j.factory.ResilienceRegistryFactory;
import reactor.core.publisher.Mono;

import java.util.function.Function;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import io.github.resilience4j.circuitbreaker.CircuitBreaker;
import io.github.resilience4j.reactor.circuitbreaker.operator.CircuitBreakerOperator;
import org.dromara.soul.plugin.resilience4j.conf.ResilienceConf;

/**
 * CombinedExecutor.
 *
 * @Author zhanglei
 */
public class CombinedExecutor implements Executor {

    @Override
    public <T> Mono<T> run(final Mono<T> run, final Function<Throwable, Mono<T>> fallback, final ResilienceConf resilienceConf) {
        RateLimiter rateLimiter = ResilienceRegistryFactory.rateLimiter(resilienceConf.getId(), resilienceConf.getRateLimiterConfig());
        CircuitBreaker circuitBreaker = ResilienceRegistryFactory.circuitBreaker(resilienceConf.getId(), resilienceConf.getCircuitBreakerConfig());
        Mono<T> to = run.transformDeferred(CircuitBreakerOperator.of(circuitBreaker))
                .transformDeferred(RateLimiterOperator.of(rateLimiter))
                .timeout(resilienceConf.getTimeLimiterConfig().getTimeoutDuration())
                .doOnError(TimeoutException.class,
                        t -> circuitBreaker.onError(
                                resilienceConf.getTimeLimiterConfig().getTimeoutDuration().toMillis(),
                                TimeUnit.MILLISECONDS,
                                t));
        if (fallback != null) {
            to = to.onErrorResume(fallback);
        }
        return to;
    }
}
