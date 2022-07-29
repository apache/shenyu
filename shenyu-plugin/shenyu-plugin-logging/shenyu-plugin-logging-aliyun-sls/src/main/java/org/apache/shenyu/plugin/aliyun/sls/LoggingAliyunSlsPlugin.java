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
import org.apache.shenyu.plugin.aliyun.sls.collector.AliyunSlsLogCollector;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.logging.common.AbstractLoggingPlugin;
import org.apache.shenyu.plugin.logging.common.body.LoggingServerHttpRequest;
import org.apache.shenyu.plugin.logging.common.body.LoggingServerHttpResponse;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

/**
 * LoggingAliYunSlsPlugin send log to aliyun sls service.
 */
public class LoggingAliyunSlsPlugin extends AbstractLoggingPlugin {

    @Override
    public Mono<Void> doLogExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain,
                                   final SelectorData selector, final RuleData rule,
                                   final ServerHttpRequest request, final ShenyuRequestLog requestInfo) {
        LoggingServerHttpRequest loggingServerHttpRequest = new LoggingServerHttpRequest(request, requestInfo);
        LoggingServerHttpResponse loggingServerHttpResponse = new LoggingServerHttpResponse(exchange.getResponse(),
                requestInfo, AliyunSlsLogCollector.getInstance());
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
