package org.apache.shenyu.plugin.base.maker;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.provider.PluginDataProvider;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.List;

public class PluginDataDecisionMaker extends AbstractMatchDecisionMaker<PluginData> {
    public PluginDataDecisionMaker() {
        super(new PluginDataProvider());
    }

    @Override
    public Mono<Void> handleEmpty(String pluginName, ServerWebExchange exchange, ShenyuPluginChain chain) {
        return chain.execute(exchange);
    }

    @Override
    protected PluginData matchData(ServerWebExchange exchange, List<PluginData> dataList, String path) {
        return dataList.isEmpty() ? null : dataList.get(0);
    }

    @Override
    public boolean shouldContinue(PluginData data) {
        return data != null && data.getEnabled();
    }
}