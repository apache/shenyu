package org.dromara.soul.plugin.resilience4j;

import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.dto.convert.ResilienceHandle;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.plugin.api.SoulPluginChain;
import org.dromara.soul.plugin.api.context.SoulContext;
import org.dromara.soul.plugin.base.AbstractSoulPlugin;
import org.dromara.soul.plugin.resilience4j.build.ResilienceBuilder;
import org.dromara.soul.plugin.resilience4j.conf.ResilienceConf;
import org.dromara.soul.plugin.resilience4j.executor.CombinedExecutor;
import org.dromara.soul.plugin.resilience4j.executor.Executor;
import org.dromara.soul.plugin.resilience4j.executor.RatelimiterExecutor;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.function.Function;

/**
 * ResilencePlugin.
 *
 * @Author zhanglei
 */
public class ResilencePlugin extends AbstractSoulPlugin {

    private final CombinedExecutor combinedExecutor;
    private final RatelimiterExecutor ratelimiterExecutor;

    public ResilencePlugin(final CombinedExecutor combinedExecutor,
                           final RatelimiterExecutor ratelimiterExecutor) {
        this.combinedExecutor = combinedExecutor;
        this.ratelimiterExecutor = ratelimiterExecutor;
    }

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final SoulPluginChain chain, final SelectorData selector, final RuleData rule) {
        final SoulContext soulContext = exchange.getAttribute(Constants.CONTEXT);
        assert soulContext != null;
        ResilienceHandle resilienceHandle = GsonUtils.getGson().fromJson(rule.getHandle(), ResilienceHandle.class);
        if (resilienceHandle.getCircuitEnable() == 1) {
            return combined(exchange, chain, rule);
        }
        return ratelimiter(exchange, chain, rule);
    }

    private Mono<Void> ratelimiter(final ServerWebExchange exchange, final SoulPluginChain chain, RuleData rule) {
        ResilienceConf conf = ResilienceBuilder.build(rule);
        return ratelimiterExecutor.run(
                chain.execute(exchange), callback(Mono::error, ratelimiterExecutor, exchange, null), conf)
                .onErrorResume(throwable -> {
                    return ratelimiterExecutor.error(exchange);
                });
    }

    private Mono<Void> combined(final ServerWebExchange exchange, final SoulPluginChain chain, RuleData rule) {
        ResilienceConf conf = ResilienceBuilder.build(rule);
        return combinedExecutor.run(
                chain.execute(exchange), callback(Mono::error, combinedExecutor, exchange, conf.getFallBackUri()), conf)
                .onErrorResume(throwable -> {
                    return combinedExecutor.error(exchange);
                });
    }

    private Function<Throwable, Mono<Void>> callback(final Function<Throwable, Mono<Void>> errorFunction, final Executor Executor,
                                                     final ServerWebExchange exchange, final String uri) {
        return throwable -> {
            try {
                return Executor.fallback(exchange, uri, throwable);
            } catch (Throwable fallbackThrowable) {
                return errorFunction.apply(fallbackThrowable);
            }
        };
    }

    @Override
    public int getOrder() {
        return PluginEnum.Resilence4J.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.Resilence4J.getName();
    }
}
