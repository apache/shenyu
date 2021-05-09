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

package org.apache.shenyu.plugin.httpclient.response;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.springframework.http.HttpStatus;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.web.reactive.function.BodyExtractors;
import org.springframework.web.reactive.function.client.ClientResponse;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Objects;

/**
 * this is response plugin.
 */
public class WebClientResponsePlugin implements ShenyuPlugin {

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
        return chain.execute(exchange).then(Mono.defer(() -> {
            ServerHttpResponse response = exchange.getResponse();
            ClientResponse clientResponse = exchange.getAttribute(Constants.CLIENT_RESPONSE_ATTR);
            if (Objects.isNull(clientResponse)
                    || response.getStatusCode() == HttpStatus.BAD_GATEWAY
                    || response.getStatusCode() == HttpStatus.INTERNAL_SERVER_ERROR) {
                Object error = ShenyuResultWrap.error(ShenyuResultEnum.SERVICE_RESULT_ERROR.getCode(), ShenyuResultEnum.SERVICE_RESULT_ERROR.getMsg(), null);
                return WebFluxResultUtils.result(exchange, error);
            }
            if (response.getStatusCode() == HttpStatus.GATEWAY_TIMEOUT) {
                Object error = ShenyuResultWrap.error(ShenyuResultEnum.SERVICE_TIMEOUT.getCode(), ShenyuResultEnum.SERVICE_TIMEOUT.getMsg(), null);
                return WebFluxResultUtils.result(exchange, error);
            }
            response.setStatusCode(clientResponse.statusCode());
            response.getCookies().putAll(clientResponse.cookies());
            response.getHeaders().putAll(clientResponse.headers().asHttpHeaders());
            return response.writeWith(clientResponse.body(BodyExtractors.toDataBuffers())).doOnCancel(() -> clean(exchange));
        }));
    }

    @Override
    public int getOrder() {
        return PluginEnum.RESPONSE.getCode();
    }

    @Override
    public Boolean skip(final ServerWebExchange exchange) {
        final ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        assert shenyuContext != null;
        return !Objects.equals(RpcTypeEnum.HTTP.getName(), shenyuContext.getRpcType())
                && !Objects.equals(RpcTypeEnum.SPRING_CLOUD.getName(), shenyuContext.getRpcType());
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
    
    private void clean(final ServerWebExchange exchange) {
        ClientResponse clientResponse = exchange.getAttribute(Constants.CLIENT_RESPONSE_ATTR);
        if (clientResponse != null) {
            clientResponse.bodyToMono(Void.class).subscribe();
        }
    }
}
