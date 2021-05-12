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

package org.apache.shenyu.plugin.base;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.HttpParamConverter;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
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
 * The param transform plugin.
 */
public class ParamTransformPlugin implements ShenyuPlugin {

    private final List<HttpMessageReader<?>> messageReaders;

    /**
     * Instantiates a new param transform plugin.
     */
    public ParamTransformPlugin() {
        this.messageReaders = HandlerStrategies.withDefaults().messageReaders();
    }

    @Override
    public Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        ServerHttpRequest request = exchange.getRequest();
        ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        if (Objects.nonNull(shenyuContext)) {
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
        return PluginEnum.PARAM_TRANSFORM.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.PARAM_TRANSFORM.getName();
    }

    private Mono<Void> body(final ServerWebExchange exchange, final ServerRequest serverRequest, final ShenyuPluginChain chain) {
        return serverRequest.bodyToMono(String.class)
                .switchIfEmpty(Mono.defer(() -> Mono.just("")))
                .flatMap(body -> {
                    exchange.getAttributes().put(Constants.PARAM_TRANSFORM, body);
                    return chain.execute(exchange);
                });
    }
    
    private Mono<Void> formData(final ServerWebExchange exchange, final ServerRequest serverRequest, final ShenyuPluginChain chain) {
        return serverRequest.formData()
                .switchIfEmpty(Mono.defer(() -> Mono.just(new LinkedMultiValueMap<>())))
                .flatMap(map -> {
                    exchange.getAttributes().put(Constants.PARAM_TRANSFORM, HttpParamConverter.toMap(() -> map));
                    return chain.execute(exchange);
                });
    }
    
    private Mono<Void> query(final ServerWebExchange exchange, final ServerRequest serverRequest, final ShenyuPluginChain chain) {
        exchange.getAttributes().put(Constants.PARAM_TRANSFORM, HttpParamConverter.ofString(() -> serverRequest.uri().getQuery()));
        return chain.execute(exchange);
    }
    
    @Override
    public Boolean skip(final ServerWebExchange exchange) {
        ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        assert shenyuContext != null;
        String rpcType = shenyuContext.getRpcType();
        return !Objects.equals(rpcType, RpcTypeEnum.DUBBO.getName()) 
                && !Objects.equals(rpcType, RpcTypeEnum.GRPC.getName()) 
                && !Objects.equals(rpcType, RpcTypeEnum.TARS.getName())
                && !Objects.equals(rpcType, RpcTypeEnum.MOTAN.getName())
                && !Objects.equals(rpcType, RpcTypeEnum.SOFA.getName());
    }
}
