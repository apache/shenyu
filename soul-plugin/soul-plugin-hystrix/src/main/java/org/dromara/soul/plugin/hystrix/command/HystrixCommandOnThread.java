package org.dromara.soul.plugin.hystrix.command;

import com.netflix.hystrix.HystrixCommand;
import org.dromara.soul.plugin.api.SoulPluginChain;
import org.dromara.soul.plugin.base.utils.WebFluxResultUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import rx.Observable;
import rx.RxReactiveStreams;

/**
 * hystrix command in thread isolation mode
 * @author liangziqiang
 */
public class HystrixCommandOnThread extends HystrixCommand<Mono<Void>> implements Command {

    /**
     * logger.
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(HystrixCommandOnThread.class);

    private final ServerWebExchange exchange;

    private final SoulPluginChain chain;

    /**
     * Instantiates a new Http command.
     *
     * @param setter   the setter
     * @param exchange the exchange
     * @param chain    the chain
     */
    public HystrixCommandOnThread(final HystrixCommand.Setter setter,
                          final ServerWebExchange exchange,
                          final SoulPluginChain chain) {

        super(setter);
        this.exchange = exchange;
        this.chain = chain;
    }
    @Override
    protected Mono<Void> run() {
        return chain.execute(exchange);
    }

    @Override
    protected Mono<Void> getFallback() {
        if (isFailedExecution()) {
            LOGGER.error("hystrix execute have error:", getExecutionException());
        }
        final Throwable exception = getExecutionException();
        LOGGER.debug("error exception in hystrix command thread：", exception);
        Object error;
        error = generateError(exchange,exception);
        return WebFluxResultUtils.result(exchange, error);
    }

    @Override
    public Observable<Void> fetchObservable() {
        return RxReactiveStreams.toObservable(this.execute());
    }
}
