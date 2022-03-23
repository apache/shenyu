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

package org.apache.shenyu.plugin.logging;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.HostAddressUtils;
import org.apache.shenyu.plugin.logging.body.LoggingServerHttpRequest;
import org.apache.shenyu.plugin.logging.body.LoggingServerHttpResponse;
import org.apache.shenyu.plugin.logging.config.LogCollectConfig;
import org.apache.shenyu.plugin.logging.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.logging.rocketmq.RocketMQLogCollectClient;
import org.apache.shenyu.plugin.logging.utils.LogCollectConfigUtils;
import org.apache.shenyu.plugin.logging.utils.LogCollectUtils;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import static org.apache.shenyu.common.enums.PluginEnum.LOGGING_ROCKETMQ;

/**
 * Integrated rocketmq collect log.
 */
public class LoggingRocketMQPlugin extends AbstractShenyuPlugin {

    private static final String USER_AGENT = "User-Agent";

    private static final String HOST = "Host";

    private final LogCollectConfig logCollectConfig;

    public LoggingRocketMQPlugin(final LogCollectConfig logCollectConfig) {
        this.logCollectConfig = logCollectConfig;
        init();
    }

    private void init() {
        LogCollectConfigUtils.setLogCollectConfig(logCollectConfig);
        RocketMQLogCollectClient client = new RocketMQLogCollectClient(logCollectConfig.getRocketmqProps());
        new DefaultLogCollector(client);
    }

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
        requestInfo.setRequestHeader(LogCollectUtils.getRequestHeaders(request.getHeaders()));
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
        return LOGGING_ROCKETMQ.getCode();
    }

    /**
     * get plugin name.
     *
     * @return plugin name
     */
    @Override
    public String named() {
        return LOGGING_ROCKETMQ.getName();
    }
}
