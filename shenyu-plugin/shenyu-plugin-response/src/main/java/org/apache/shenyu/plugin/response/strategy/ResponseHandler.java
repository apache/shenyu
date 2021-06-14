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

package org.apache.shenyu.plugin.response.strategy;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.response.config.HttpClientProperties;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.server.ServerWebExchange;

import java.util.Objects;

/**
 * response handler.
 */
public class ResponseHandler {

    @Autowired
    private HttpClientProperties httpClientProperties;

    /**
     * strategy dispatch.
     *
     * @param exchange exchange the current server exchange
     * @return {@code Mono<Void>} to indicate when request processing is complete
     */
    public Strategy dispatch(final ServerWebExchange exchange) {
        final ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        if (Objects.equals(RpcTypeEnum.HTTP.getName(), shenyuContext.getRpcType())
                || Objects.equals(RpcTypeEnum.SPRING_CLOUD.getName(), shenyuContext.getRpcType())) {
            return getHttpStrategy();
        } else {
            return new DefaultStrategy();
        }
    }

    private Strategy getHttpStrategy() {
        if (httpClientProperties.getStrategy().equals("webClient")) {
            return new WebClientStrategy();
        } else if (httpClientProperties.getStrategy().equals("netty")) {
            return new NettyClientStrategy();
        } else {
            return null;
        }
    }
}
