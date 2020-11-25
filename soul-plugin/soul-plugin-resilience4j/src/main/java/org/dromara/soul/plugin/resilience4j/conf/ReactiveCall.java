package org.dromara.soul.plugin.resilience4j.conf;

import java.util.function.Function;
import reactor.core.publisher.Mono;

/**
 *
 * @author zhanglei
 */
public interface ReactiveCall {

    <T> Mono<T> run(Mono<T> toRun, Function<Throwable, Mono<T>> fallback);

}
