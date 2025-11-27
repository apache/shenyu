package org.apache.shenyu.plugin.base.maker;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.provider.RuleDataProvider;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.List;

public class RuleDataDecisionMaker extends AbstractMatchDecisionMaker<RuleData> {
    public RuleDataDecisionMaker() {
        super(new RuleDataProvider());
    }

    @Override
    public Mono<Void> handleEmpty(String pluginName, ServerWebExchange exchange, ShenyuPluginChain chain) {
        return chain.execute(exchange);
    }

    @Override
    public RuleData matchData(ServerWebExchange exchange, List<RuleData> dataList, String path) {
        return dataList.isEmpty() ? null : dataList.get(0);
    }

    @Override
    public boolean shouldContinue(RuleData data) {
        return data != null && data.getEnabled();
    }
}