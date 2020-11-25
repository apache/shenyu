package org.dromara.soul.plugin.resilience4j.conf;

import io.github.resilience4j.circuitbreaker.CircuitBreaker;
import io.github.resilience4j.circuitbreaker.CircuitBreakerRegistry;
import io.github.resilience4j.reactor.circuitbreaker.operator.CircuitBreakerOperator;
import reactor.core.publisher.Mono;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.function.Function;

public class Resilience4JCircuitBreaker implements ReactiveCall {

    private final String id;
    private final Resilience4jConf resilience4jConf;
    private final CircuitBreakerRegistry circuitBreakerRegistry;

    public Resilience4JCircuitBreaker(final String id,
                                      //final String fallBackUri,
                                      final Resilience4jConf resilience4jConf,
                                      final CircuitBreakerRegistry circuitBreakerRegistry) {
        this.id = id;
        this.resilience4jConf = resilience4jConf;
        this.circuitBreakerRegistry = circuitBreakerRegistry;
    }

    @Override
    public <T> Mono<T> run(final Mono<T> run, final Function<Throwable, Mono<T>> fallback) {
        CircuitBreaker circuitBreaker = circuitBreakerRegistry.circuitBreaker(this.id, resilience4jConf.getCircuitBreakerConfig());
        Mono<T> to = run.transform(CircuitBreakerOperator.of(circuitBreaker))
                .timeout(resilience4jConf.getTimeLimiterConfig().getTimeoutDuration())
                .doOnError(TimeoutException.class,
                        t -> circuitBreaker.onError(
                                resilience4jConf.getTimeLimiterConfig().getTimeoutDuration().toMillis(),
                                TimeUnit.MILLISECONDS,
                                t));
        if (fallback != null) {
            to = to.onErrorResume(fallback);
        }
        return to;
    }

}
