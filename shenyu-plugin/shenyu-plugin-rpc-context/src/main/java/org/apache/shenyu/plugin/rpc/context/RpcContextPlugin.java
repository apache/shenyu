/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.shenyu.plugin.rpc.context;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.RpcContextHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.rpc.context.handler.RpcContextPluginDataHandler;
import org.springframework.http.HttpHeaders;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * RpcContextPlugin, transfer http headers to rpc context.
 */
public class RpcContextPlugin extends AbstractShenyuPlugin {

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
        List<RpcContextHandle> rpcContextHandles = RpcContextPluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(rule));
        Map<String, Map<String, String>> rpcContextMap = new HashMap<>();
        HttpHeaders headers = exchange.getRequest().getHeaders();
        rpcContextHandles.stream().forEach(each -> {
            Map<String, String> rpcContextMapWithRpcType = new HashMap<>();
            final List<RpcContextHandle.RpcContextHandleContent> rpcContextHandleContents = each.getRpcContextHandleContents();
            rpcContextHandleContents.forEach(eachContents -> {
                switch (eachContents.getRpcContextType()) {
                    case Constants.ADD_RPC_CONTEXT_TYPE:
                        rpcContextMapWithRpcType.put(eachContents.getRpcContextKey(), eachContents.getRpcContextValue());
                        break;
                    case Constants.TRANSMIT_HEADER_TO_RPC_CONTEXT_TYPE:
                        rpcContextMapWithRpcType.put(StringUtils.isBlank(eachContents.getRpcContextValue()) ? eachContents.getRpcContextKey() : eachContents.getRpcContextValue(),
                                headers.getFirst(eachContents.getRpcContextKey()));
                        break;
                    default:
                        break;
                }
            });
            rpcContextMap.put(each.getRpcType(), rpcContextMapWithRpcType);
        });
        exchange.getAttributes().put(Constants.RPC_CONTEXT, rpcContextMap);
        return chain.execute(exchange);
    }

    @Override
    public int getOrder() {
        return PluginEnum.RPC_CONTEXT.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.RPC_CONTEXT.getName();
    }

    @Override
    public boolean skip(final ServerWebExchange exchange) {
        return false;
    }

}
