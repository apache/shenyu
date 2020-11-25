package org.dromara.soul.plugin.resilience4j;

import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.plugin.api.SoulPluginChain;
import org.dromara.soul.plugin.base.AbstractSoulPlugin;
import org.dromara.soul.plugin.resilience4j.factory.Resilience4JFactory;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

public class ResilencePlugin extends AbstractSoulPlugin {


    private final Resilience4JFactory resilience4JFactory;

    public ResilencePlugin(Resilience4JFactory resilience4JFactory){
        this.resilience4JFactory = resilience4JFactory;
    }


    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange,final SoulPluginChain chain, final SelectorData selector, final RuleData rule) {


        return chain.execute(exchange);
    }

    @Override
    public int getOrder() {
        return 0;
    }
}
