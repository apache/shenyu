package org.apache.shenyu.plugin.base.maker;

import org.apache.shenyu.common.dto.BaseData;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.provider.DataProvider;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.List;

public abstract class AbstractMatchDecisionMaker<T extends BaseData> {
    private final DataProvider<T> dataProvider;

    protected AbstractMatchDecisionMaker(DataProvider<T> dataProvider) {
        this.dataProvider = dataProvider;
    }

    public List<T> getData(String key) {
        return dataProvider.getData(key);
    }

    protected abstract Mono<Void> handleEmpty(String pluginName, ServerWebExchange exchange, ShenyuPluginChain chain);
    
    protected abstract T matchData(ServerWebExchange exchange, List<T> dataList, String path);
    
    protected abstract boolean shouldContinue(T data);
}