package org.dromara.soul.plugin.resilience4j.executor;

import io.github.resilience4j.circuitbreaker.CallNotPermittedException;
import io.github.resilience4j.ratelimiter.RequestNotPermitted;
import org.dromara.soul.plugin.api.result.SoulResultEnum;
import org.dromara.soul.plugin.base.utils.SoulResultWrap;
import org.dromara.soul.plugin.base.utils.SpringBeanUtils;
import org.dromara.soul.plugin.base.utils.UriUtils;
import org.dromara.soul.plugin.base.utils.WebFluxResultUtils;
import org.dromara.soul.plugin.resilience4j.conf.ResilienceConf;
import org.springframework.http.HttpStatus;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.web.reactive.DispatcherHandler;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Objects;
import java.util.concurrent.TimeoutException;
import java.util.function.Function;

/**
 * Executor.
 *
 * @Author zhanglei
 */
public interface Executor {

    /**
     * resilience.run
     *
     * @param toRun
     * @param fallback
     * @param conf
     * @param <T>
     * @return
     */
    <T> Mono<T> run(Mono<T> toRun, Function<Throwable, Mono<T>> fallback, ResilienceConf conf);

    /**
     * do fallback
     *
     * @param exchange
     * @param uri
     * @param throwable
     * @return
     */
    default Mono<Void> fallback(ServerWebExchange exchange, String uri, Throwable throwable) {
        if (Objects.isNull(uri)) {
            return withoutFallback(exchange, throwable);
        }
        DispatcherHandler dispatcherHandler = SpringBeanUtils.getInstance().getBean(DispatcherHandler.class);
        ServerHttpRequest request = exchange.getRequest().mutate().uri(UriUtils.createUri(uri)).build();
        ServerWebExchange mutated = exchange.mutate().request(request).build();
        return dispatcherHandler.handle(mutated);
    }

    /**
     * do fallback with not  fallback method
     *
     * @param exchange
     * @param t
     * @return
     */
    default Mono<Void> withoutFallback(ServerWebExchange exchange, Throwable t) {
        Object error;
        if (TimeoutException.class.isInstance(t)) {
            exchange.getResponse().setStatusCode(HttpStatus.GATEWAY_TIMEOUT);
            error = SoulResultWrap.error(SoulResultEnum.SERVICE_TIMEOUT.getCode(), SoulResultEnum.SERVICE_TIMEOUT.getMsg(), null);
        } else if (CallNotPermittedException.class.isInstance(t)) {
            exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
            error = SoulResultWrap.error(SoulResultEnum.SERVICE_RESULT_ERROR.getCode(), SoulResultEnum.SERVICE_RESULT_ERROR.getMsg(), null);
        } else if (RequestNotPermitted.class.isInstance(t)) {
            exchange.getResponse().setStatusCode(HttpStatus.TOO_MANY_REQUESTS);
            error = SoulResultWrap.error(SoulResultEnum.TOO_MANY_REQUESTS.getCode(), SoulResultEnum.TOO_MANY_REQUESTS.getMsg(), null);
        } else {
            exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
            error = SoulResultWrap.error(SoulResultEnum.SERVICE_RESULT_ERROR.getCode(), SoulResultEnum.SERVICE_RESULT_ERROR.getMsg(), null);
        }
        return WebFluxResultUtils.result(exchange, error);
    }

    /**
     * default error
     *
     * @param exchange
     * @return
     */
    default Mono<Void> error(ServerWebExchange exchange) {
        exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
        Object error = SoulResultWrap.error(SoulResultEnum.SERVICE_RESULT_ERROR.getCode(), SoulResultEnum.SERVICE_RESULT_ERROR.getMsg(), null);
        return WebFluxResultUtils.result(exchange, error);
    }

}
