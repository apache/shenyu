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

package org.apache.shenyu.agent.plugin.logging.common.handler;

import org.apache.shenyu.agent.api.entity.MethodResult;
import org.apache.shenyu.agent.api.entity.TargetObject;
import org.apache.shenyu.agent.api.handler.InstanceMethodHandler;
import org.apache.shenyu.agent.plugin.logging.common.DefaultLogCollector;
import org.apache.shenyu.agent.plugin.logging.common.body.LoggingServerHttpRequest;
import org.apache.shenyu.agent.plugin.logging.common.body.LoggingServerHttpResponse;
import org.apache.shenyu.agent.plugin.logging.common.utils.LogCollectConfigUtils;
import org.apache.shenyu.agent.plugin.logging.common.utils.LogCollectUtils;
import org.apache.shenyu.agent.plugin.logging.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.base.utils.HostAddressUtils;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.web.server.ServerWebExchange;

import java.lang.reflect.Method;


/**
 * default logging plugin handler.
 */
public class DefaultLoggingPluginHandler implements InstanceMethodHandler {

    private static final String USER_AGENT = "User-Agent";

    private static final String HOST = "Host";

    @Override
    public Object after(final TargetObject target, final Method method, final Object[] args,
                        final MethodResult methodResult) {
        Object result = methodResult.getResult();
        ServerWebExchange exchange = (ServerWebExchange) result;
        ServerHttpRequest request = exchange.getRequest();

        // control sampling
        if (!LogCollectConfigUtils.isSampled(request)) {
            return methodResult.getResult();
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
        return webExchange;
    }

}
