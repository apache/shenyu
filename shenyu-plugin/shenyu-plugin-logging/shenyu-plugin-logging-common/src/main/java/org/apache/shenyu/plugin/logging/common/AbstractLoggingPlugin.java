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

package org.apache.shenyu.plugin.logging.common;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.HostAddressUtils;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.logging.common.utils.LogCollectConfigUtils;
import org.apache.shenyu.plugin.logging.common.utils.LogCollectUtils;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import static org.apache.shenyu.plugin.logging.common.constant.GenericLoggingConstant.HOST;
import static org.apache.shenyu.plugin.logging.common.constant.GenericLoggingConstant.USER_AGENT;

/**
 * abstract logging plugin.
 */
public abstract class AbstractLoggingPlugin extends AbstractShenyuPlugin {
    
    /**
     * log collector execute.
     *
     * @param exchange web exchange
     * @param chain shenyu plugin chain
     * @param selector selector data
     * @param rule rule data
     * @param request server http request
     * @param requestInfo request information
     * @return mono
     */
    protected abstract Mono<Void> doLogExecute(ServerWebExchange exchange, ShenyuPluginChain chain,
                                               SelectorData selector, RuleData rule,
                                               ServerHttpRequest request, ShenyuRequestLog requestInfo);

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain,
                                   final SelectorData selector, final RuleData rule) {
        ServerHttpRequest request = exchange.getRequest();
        // control sampling
        if (!LogCollectConfigUtils.isSampled(exchange.getRequest())) {
            return chain.execute(exchange);
        }
        ShenyuRequestLog requestInfo = new ShenyuRequestLog();
        requestInfo.setRequestUri(request.getURI().toString());
        requestInfo.setMethod(request.getMethodValue());
        requestInfo.setRequestHeader(LogCollectUtils.getHeaders(request.getHeaders()));
        requestInfo.setQueryParams(request.getURI().getQuery());
        requestInfo.setClientIp(HostAddressUtils.acquireIp(exchange));
        requestInfo.setUserAgent(request.getHeaders().getFirst(USER_AGENT));
        requestInfo.setHost(request.getHeaders().getFirst(HOST));
        requestInfo.setPath(request.getURI().getPath());
        return this.doLogExecute(exchange, chain, selector, rule, request, requestInfo);
    }
}
