package org.apache.shenyu.plugin.base.maker;

import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.provider.SelectorDataProvider;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.List;

public class SelectorDataDecisionMaker extends AbstractMatchDecisionMaker<SelectorData> {
    public SelectorDataDecisionMaker() {
        super(new SelectorDataProvider());
    }

    @Override
    public Mono<Void> handleEmpty(String pluginName, ServerWebExchange exchange, ShenyuPluginChain chain) {
        return chain.execute(exchange);
    }

    @Override
    public SelectorData matchData(ServerWebExchange exchange, List<SelectorData> dataList, String path) {
        return dataList.isEmpty() ? null : dataList.get(0);
    }

    @Override
    public boolean shouldContinue(SelectorData data) {
        return data != null && data.getEnabled() && data.getContinued();
    }
}