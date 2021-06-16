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

package org.apache.shenyu.plugin.response;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.response.strategy.ResponseHandler;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import java.util.Objects;

/**
 * this is rpc response plugin.
 */
public class ResponsePlugin implements ShenyuPlugin {

    @Autowired
    private ResponseHandler responseHandler;

    /**
     * Process the Web request and (optionally) delegate to the next
     * {@code WebFilter} through the given {@link ShenyuPluginChain}.
     *
     * @param exchange the current server exchange
     * @param chain    provides a way to delegate to the next filter
     * @return {@code Mono<Void>} to indicate when request processing is complete
     */
    @Override
    public Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        return responseHandler.dispatch(exchange).doExcute(exchange, chain);
    }

    /**
     * skip.
     *
     * @param exchange the current server exchange
     * @return
     */
    @Override
    public Boolean skip(final ServerWebExchange exchange) {
        final ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        assert shenyuContext != null;
        return !Objects.equals(shenyuContext.getRpcType(), RpcTypeEnum.GRPC.getName())
                && !Objects.equals(shenyuContext.getRpcType(), RpcTypeEnum.SOFA.getName())
                && !Objects.equals(shenyuContext.getRpcType(), RpcTypeEnum.DUBBO.getName())
                && !Objects.equals(shenyuContext.getRpcType(), RpcTypeEnum.TARS.getName())
                && !Objects.equals(shenyuContext.getRpcType(), RpcTypeEnum.HTTP.getName())
                && !Objects.equals(shenyuContext.getRpcType(), RpcTypeEnum.SPRING_CLOUD.getName());
    }

    /**
     * get order.
     *
     * @return
     */
    @Override
    public int getOrder() {
        return PluginEnum.RESPONSE.getCode();
    }

    /**
     * acquire plugin name.
     *
     * @return plugin name.
     */
    @Override
    public String named() {
        return PluginEnum.RESPONSE.getName();
    }
}
