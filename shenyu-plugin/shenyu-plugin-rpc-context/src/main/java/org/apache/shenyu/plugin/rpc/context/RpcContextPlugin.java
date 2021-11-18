package org.apache.shenyu.plugin.rpc.context;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.RequestHandle;
import org.apache.shenyu.common.dto.convert.rule.RpcContextHandle;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.rpc.context.handler.RpcContextPluginDataHandler;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

/**
 * RpcContextPlugin, transfer http headers to rpc context.
 */
public class RpcContextPlugin extends AbstractShenyuPlugin {

    @Override
    protected Mono<Void> doExecute(ServerWebExchange exchange, ShenyuPluginChain chain, SelectorData selector, RuleData rule) {
        RpcContextHandle rpcContextHandle = RpcContextPluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(rule));
        return null;
    }

    @Override
    public int getOrder() {
        return 0;
    }

    @Override
    public String named() {
        return super.named();
    }

    @Override
    public boolean skip(ServerWebExchange exchange) {
        return super.skip(exchange);
    }

}
