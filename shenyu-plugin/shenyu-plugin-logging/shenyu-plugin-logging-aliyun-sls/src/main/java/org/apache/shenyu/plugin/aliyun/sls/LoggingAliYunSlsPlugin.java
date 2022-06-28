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

package org.apache.shenyu.plugin.aliyun.sls;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.aliyun.sls.body.LoggingServerHttpRequest;
import org.apache.shenyu.plugin.aliyun.sls.body.LoggingServerHttpResponse;
import org.apache.shenyu.plugin.aliyun.sls.constant.LoggingConstant;
import org.apache.shenyu.plugin.aliyun.sls.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.aliyun.sls.utils.LogCollectConfigUtils;
import org.apache.shenyu.plugin.aliyun.sls.utils.LogCollectUtils;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.HostAddressUtils;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import static org.apache.shenyu.plugin.aliyun.sls.constant.LoggingConstant.HOST;
import static org.apache.shenyu.plugin.aliyun.sls.constant.LoggingConstant.USER_AGENT;

// https://help.aliyun.com/document_detail/29008.htm?spm=a2c4g.11186623.0.0.cc7a3de5dJNKOe#reference-wgx-pwq-zdb
/**
 * LoggingAliYunSlsPlugin send log to aliyun sls service.
 */
public class LoggingAliYunSlsPlugin extends AbstractShenyuPlugin {

    @Override
    protected Mono<Void> doExecute(ServerWebExchange exchange, ShenyuPluginChain chain, SelectorData selector, RuleData rule) {
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

        LoggingServerHttpRequest loggingServerHttpRequest = new LoggingServerHttpRequest(request, requestInfo);
        LoggingServerHttpResponse loggingServerHttpResponse = new LoggingServerHttpResponse(exchange.getResponse(),
                requestInfo, DefaultLogCollector.getInstance());
        ServerWebExchange webExchange = exchange.mutate().request(loggingServerHttpRequest)
                .response(loggingServerHttpResponse).build();
        loggingServerHttpResponse.setExchange(webExchange);

        return chain.execute(webExchange).doOnError(loggingServerHttpResponse::logError);
    }

    /**
     * get plugin order.
     *
     * @return order
     */
    @Override
    public int getOrder() {
        return PluginEnum.LOGGING_ALIYUN_SLS.getCode();
    }

    /**
     * get plugin name.
     *
     * @return plugin name
     */
    @Override
    public String named() {
        return PluginEnum.LOGGING_ALIYUN_SLS.getName();
    }
}
