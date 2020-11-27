package org.dromara.soul.plugin.resilience4j.executor;

import java.util.Objects;
import java.util.function.Function;

import org.dromara.soul.plugin.resilience4j.factory.ResilienceRegistryFactory;
import reactor.core.publisher.Mono;
import io.github.resilience4j.ratelimiter.RateLimiter;
import org.dromara.soul.plugin.resilience4j.conf.ResilienceConf;
import io.github.resilience4j.reactor.ratelimiter.operator.RateLimiterOperator;

/**
 * RatelimiterExecutor.
 *
 * @Author zhanglei
 */
public class RatelimiterExecutor implements Executor {

    @Override
    public <T> Mono<T> run(final Mono<T> toRun, final Function<Throwable, Mono<T>> fallback, final ResilienceConf conf) {
        RateLimiter rateLimiter = ResilienceRegistryFactory.rateLimiter(conf.getId(), conf.getRateLimiterConfig());
        Mono<T> to = toRun.transform(RateLimiterOperator.of(rateLimiter));
        if (Objects.nonNull(fallback)) {
            to.onErrorResume(fallback);
        }
        return to;
    }

}
