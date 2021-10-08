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
import org.apache.shenyu.plugin.api.utils.BodyParamUtils;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.MediaType;
import org.springframework.http.codec.HttpMessageReader;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.web.reactive.function.server.HandlerStrategies;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Objects;

/**
 * The param transform plugin.
 */
public class RpcParamTransformPlugin implements ShenyuPlugin {

    private final List<HttpMessageReader<?>> messageReaders;

    /**
     * Instantiates a new param transform plugin.
     */
    public RpcParamTransformPlugin() {
        this.messageReaders = HandlerStrategies.withDefaults().messageReaders();
    }

    @Override
    public Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        ServerHttpRequest request = exchange.getRequest();
        ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        if (Objects.nonNull(shenyuContext)) {
            MediaType mediaType = request.getHeaders().getContentType();
            if (MediaType.APPLICATION_JSON.isCompatibleWith(mediaType)) {
                return body(exchange, request, chain);
            }
            if (MediaType.APPLICATION_FORM_URLENCODED.isCompatibleWith(mediaType)) {
                return formData(exchange, request, chain);
            }
            return query(exchange, request, chain);
        }
        return chain.execute(exchange);
    }

    @Override
    public int getOrder() {
        return PluginEnum.RPC_PARAM_TRANSFORM.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.RPC_PARAM_TRANSFORM.getName();
    }

    private Mono<Void> body(final ServerWebExchange exchange, final ServerHttpRequest serverHttpRequest, final ShenyuPluginChain chain) {
        return Mono.from(serverHttpRequest.getBody()
                .flatMap(body -> {
                    exchange.getAttributes().put(Constants.PARAM_TRANSFORM, resolveBodyFromRequest(body));
                    return chain.execute(exchange);
                }));
    }

    private Mono<Void> formData(final ServerWebExchange exchange, final ServerHttpRequest serverHttpRequest, final ShenyuPluginChain chain) {
        return Mono.from(serverHttpRequest.getBody()
                .flatMap(map -> {
                    String param = resolveBodyFromRequest(map);
                    LinkedMultiValueMap linkedMultiValueMap;
                    try {
                        linkedMultiValueMap = BodyParamUtils.buildBodyParams(URLDecoder.decode(param, StandardCharsets.UTF_8.name()));
                    } catch (UnsupportedEncodingException e) {
                        return Flux.error(e);
                    }
                    exchange.getAttributes().put(Constants.PARAM_TRANSFORM, HttpParamConverter.toMap(() -> linkedMultiValueMap));
                    return chain.execute(exchange);
                }));
    }

    private Mono<Void> query(final ServerWebExchange exchange, final ServerHttpRequest serverHttpRequest, final ShenyuPluginChain chain) {
        exchange.getAttributes().put(Constants.PARAM_TRANSFORM, HttpParamConverter.ofString(() -> serverHttpRequest.getURI().getQuery()));
        return chain.execute(exchange);
    }

    @Override
    public boolean skip(final ServerWebExchange exchange) {
        ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        assert shenyuContext != null;
        String rpcType = shenyuContext.getRpcType();
        return !Objects.equals(rpcType, RpcTypeEnum.DUBBO.getName())
                && !Objects.equals(rpcType, RpcTypeEnum.GRPC.getName())
                && !Objects.equals(rpcType, RpcTypeEnum.TARS.getName())
                && !Objects.equals(rpcType, RpcTypeEnum.MOTAN.getName())
                && !Objects.equals(rpcType, RpcTypeEnum.SOFA.getName());
    }

    private String resolveBodyFromRequest(final DataBuffer dataBuffer) {
        byte[] bytes = new byte[dataBuffer.readableByteCount()];
        dataBuffer.read(bytes);
        DataBufferUtils.release(dataBuffer);
        return new String(bytes, StandardCharsets.UTF_8);
    }
}
