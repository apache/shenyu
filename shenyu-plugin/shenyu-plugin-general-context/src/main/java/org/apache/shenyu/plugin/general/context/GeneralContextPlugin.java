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

package org.apache.shenyu.plugin.general.context;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.GeneralContextHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.general.context.handler.GeneralContextPluginDataHandler;
import org.springframework.http.HttpHeaders;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * GeneralContextPlugin, transfer http headers to rpc context.
 */
public class GeneralContextPlugin extends AbstractShenyuPlugin {

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
        Map<String, List<GeneralContextHandle>> generalContextHandleMap = GeneralContextPluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(rule));
        Map<String, Map<String, String>> generalContextMap = new HashMap<>();
        HttpHeaders headers = exchange.getRequest().getHeaders();
        generalContextHandleMap.forEach((rpcType, v) -> {
            if (CollectionUtils.isEmpty(v)) {
                return;
            }
            Map<String, String> generalContextMapWithRpcType = new HashMap<>();
            v.forEach(each -> {
                if (StringUtils.isBlank(each.getGeneralContextType()) || StringUtils.isBlank(each.getGeneralContextKey())) {
                    return;
                }
                switch (each.getGeneralContextType()) {
                    case Constants.ADD_GENERAL_CONTEXT_TYPE:
                        generalContextMapWithRpcType.put(each.getGeneralContextKey(), each.getGeneralContextValue());
                        break;
                    case Constants.TRANSMIT_HEADER_TO_GENERAL_CONTEXT_TYPE:
                        final List<String> header = headers.get(each.getGeneralContextKey());
                        generalContextMapWithRpcType.put(StringUtils.isBlank(each.getGeneralContextValue()) ? each.getGeneralContextKey() : each.getGeneralContextValue(),
                                CollectionUtils.isEmpty(header) ? null : String.join(", ", header));
                        break;
                    default:
                        break;
                }
            });
            generalContextMap.put(rpcType, generalContextMapWithRpcType);
        });
        exchange.getAttributes().put(Constants.GENERAL_CONTEXT, generalContextMap);
        return chain.execute(exchange);
    }

    @Override
    public int getOrder() {
        return PluginEnum.GENERAL_CONTEXT.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.GENERAL_CONTEXT.getName();
    }

    @Override
    public boolean skip(final ServerWebExchange exchange) {
        return false;
    }

}
