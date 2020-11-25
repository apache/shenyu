package org.dromara.soul.plugin.resilience4j.factory;

import io.github.resilience4j.circuitbreaker.CallNotPermittedException;
import io.github.resilience4j.circuitbreaker.CircuitBreakerRegistry;
import io.github.resilience4j.ratelimiter.RateLimiterRegistry;
import org.dromara.soul.common.dto.convert.Resilience4JHandle;
import org.dromara.soul.plugin.resilience4j.build.Resilience4jBuilder;
import org.dromara.soul.plugin.resilience4j.conf.Resilience4JRatelimiter;
import org.dromara.soul.plugin.resilience4j.conf.ReactiveCall;
import org.dromara.soul.plugin.resilience4j.conf.Resilience4JCircuitBreaker;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.http.HttpStatus;
import org.springframework.util.Assert;
import org.springframework.web.reactive.DispatcherHandler;
import org.springframework.web.server.ResponseStatusException;
import reactor.core.publisher.Mono;

import java.util.concurrent.TimeoutException;

public class Resilience4JFactory {

    private final CircuitBreakerRegistry circuitBreakerRegistry = CircuitBreakerRegistry.ofDefaults();
    private final RateLimiterRegistry rateLimiterRegistry = RateLimiterRegistry.ofDefaults();

    private final ObjectProvider<DispatcherHandler> dispatcherHandlerProvider;

    public Resilience4JFactory(final ObjectProvider<DispatcherHandler> dispatcherHandlerProvider) {
        this.dispatcherHandlerProvider = dispatcherHandlerProvider;
    }

    public ReactiveCall circuitBreaker(final String id, Resilience4JHandle resilience4JHandle) {
        Assert.hasText(id, "CircuitBreaker must have an id.");
        return new Resilience4JCircuitBreaker(id, Resilience4jBuilder.circuitBreaker(resilience4JHandle), circuitBreakerRegistry);
    }

    public ReactiveCall ratelimiter(final String id, Resilience4JHandle resilience4JHandle) {
        Assert.hasText(id, "RateLimiter must have an id.");
        return new Resilience4JRatelimiter(id, Resilience4jBuilder.rateLimiter(resilience4JHandle), rateLimiterRegistry);
    }


    public void clearConfig(String id) {
        circuitBreakerRegistry.remove(id);
        rateLimiterRegistry.remove(id);
    }

    public Mono<Void> handleErrorWithoutFallback(Throwable t) {
        if (TimeoutException.class.isInstance(t)) {
            return Mono.error(new ResponseStatusException(HttpStatus.GATEWAY_TIMEOUT, t.getMessage(), t));
        }
        if (CallNotPermittedException.class.isInstance(t)) {
            return null;
        }
        return Mono.error(t);
    }


    public Mono<Void> handleErrorFallback(Throwable t, String fallback) {
        return null;
    }
}
