/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.web.plugin.after;

import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.enums.PluginTypeEnum;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.common.exception.SoulException;
import org.dromara.soul.common.result.SoulResult;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.common.utils.JsonUtils;
import org.dromara.soul.web.plugin.SoulPlugin;
import org.dromara.soul.web.plugin.SoulPluginChain;
import org.dromara.soul.web.request.RequestDTO;
import org.springframework.http.HttpStatus;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.web.reactive.function.BodyExtractors;
import org.springframework.web.reactive.function.client.ClientResponse;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Objects;

/**
 * this is response plugin.
 *
 * @author xiaoyu
 */
public class ResponsePlugin implements SoulPlugin {

    /**
     * Process the Web request and (optionally) delegate to the next
     * {@code WebFilter} through the given {@link org.dromara.soul.web.plugin.SoulPluginChain}.
     *
     * @param exchange the current server exchange
     * @param chain    provides a way to delegate to the next filter
     * @return {@code Mono<Void>} to indicate when request processing is complete
     */
    @Override
    public Mono<Void> execute(final ServerWebExchange exchange, final SoulPluginChain chain) {

        return chain.execute(exchange).then(Mono.defer(() -> {

            ServerHttpResponse response = exchange.getResponse();

            final RequestDTO requestDTO = exchange.getAttribute(Constants.REQUESTDTO);

            assert requestDTO != null;
            if (requestDTO.getRpcType().equals(RpcTypeEnum.DUBBO.getName())) {

                final Object result = exchange.getAttribute(Constants.DUBBO_RPC_RESULT);
                try {
                    if (Objects.isNull(result)) {
                        return response.writeWith(Mono.just(exchange.getResponse()
                                .bufferFactory().wrap(Objects.requireNonNull(JsonUtils
                                        .toJson(SoulResult.error(Constants.DUBBO_ERROR_RESULT))).getBytes())));
                    }
                    return response.writeWith(Mono.just(exchange.getResponse()
                            .bufferFactory().wrap(Objects.requireNonNull(JsonUtils.dubboResultJson(result)).getBytes())));
                } catch (SoulException e) {
                    return Mono.empty();
                }
            } else {
                ClientResponse clientResponse = exchange.getAttribute(Constants.CLIENT_RESPONSE_ATTR);
                if (Objects.isNull(clientResponse) ||
                        response.getStatusCode() == HttpStatus.BAD_GATEWAY
                        || response.getStatusCode() == HttpStatus.INTERNAL_SERVER_ERROR) {
                    final String result = JsonUtils.toJson(SoulResult.error(Constants.HTTP_ERROR_RESULT));
                    return response.writeWith(Mono.just(exchange.getResponse()
                            .bufferFactory()
                            .wrap(Objects.requireNonNull(result).getBytes())));
                } else if (response.getStatusCode() == HttpStatus.GATEWAY_TIMEOUT) {
                    final String result = JsonUtils.toJson(SoulResult.timeout(Constants.TIMEOUT_RESULT));
                    return response.writeWith(Mono.just(exchange.getResponse()
                            .bufferFactory().wrap(Objects.requireNonNull(result).getBytes())));
                }
                return response.writeWith(clientResponse.body(BodyExtractors.toDataBuffers()));
            }
        }));

    }

    /**
     * return plugin type.
     *
     * @return {@linkplain PluginTypeEnum}
     */
    @Override
    public PluginTypeEnum pluginType() {
        return PluginTypeEnum.LAST;
    }

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
