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

package org.dromara.soul.plugin.apache.dubbo.param;

import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.common.utils.HttpParamConverter;
import org.dromara.soul.plugin.api.SoulPlugin;
import org.dromara.soul.plugin.api.SoulPluginChain;
import org.dromara.soul.plugin.api.context.SoulContext;
import org.springframework.http.MediaType;
import org.springframework.http.codec.HttpMessageReader;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.web.reactive.function.server.HandlerStrategies;
import org.springframework.web.reactive.function.server.ServerRequest;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Objects;

/**
 * The type Body param plugin.
 *
 * @author xiaoyu
 */
public class BodyParamPlugin implements SoulPlugin {

    private final List<HttpMessageReader<?>> messageReaders;

    /**
     * Instantiates a new Body param plugin.
     */
    public BodyParamPlugin() {
        this.messageReaders = HandlerStrategies.withDefaults().messageReaders();
    }

    @Override
    public Mono<Void> execute(final ServerWebExchange exchange, final SoulPluginChain chain) {
        final ServerHttpRequest request = exchange.getRequest();
        final SoulContext soulContext = exchange.getAttribute(Constants.CONTEXT);
        if (Objects.nonNull(soulContext) && RpcTypeEnum.DUBBO.getName().equals(soulContext.getRpcType())) {
            MediaType mediaType = request.getHeaders().getContentType();
            ServerRequest serverRequest = ServerRequest.create(exchange, messageReaders);
            if (MediaType.APPLICATION_JSON.isCompatibleWith(mediaType)) {
                return body(exchange, serverRequest, chain);
            }
            if (MediaType.APPLICATION_FORM_URLENCODED.isCompatibleWith(mediaType)) {
                return formData(exchange, serverRequest, chain);
            }
            return query(exchange, serverRequest, chain);
        }
        return chain.execute(exchange);
    }

    @Override
    public int getOrder() {
        return PluginEnum.DUBBO.getCode() - 1;
    }

    @Override
    public String named() {
        return "apache-dubbo-body-param";
    }
    
    private Mono<Void> body(final ServerWebExchange exchange, final ServerRequest serverRequest, final SoulPluginChain chain) {
        return serverRequest.bodyToMono(String.class)
                .switchIfEmpty(Mono.defer(() -> Mono.just("")))
                .flatMap(body -> {
                    exchange.getAttributes().put(Constants.DUBBO_PARAMS, body);
                    return chain.execute(exchange);
                });
    }
    
    private Mono<Void> formData(final ServerWebExchange exchange, final ServerRequest serverRequest, final SoulPluginChain chain) {
        return serverRequest.formData()
                .switchIfEmpty(Mono.defer(() -> Mono.just(new LinkedMultiValueMap<>())))
                .flatMap(map -> {
                    exchange.getAttributes().put(Constants.DUBBO_PARAMS, HttpParamConverter.toMap(() -> map));
                    return chain.execute(exchange);
                });
    }
    
    private Mono<Void> query(final ServerWebExchange exchange, final ServerRequest serverRequest, final SoulPluginChain chain) {
        exchange.getAttributes().put(Constants.DUBBO_PARAMS,
                HttpParamConverter.ofString(() -> serverRequest.uri().getQuery()));
        return chain.execute(exchange);
    }
}
