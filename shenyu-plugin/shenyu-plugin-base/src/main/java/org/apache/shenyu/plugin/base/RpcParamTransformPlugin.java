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
import org.apache.shenyu.plugin.base.utils.HttpParamConverter;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.utils.BodyParamUtils;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.MediaType;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.lang.NonNull;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.util.Objects;
import java.util.Optional;

/**
 * The param transform plugin.
 */
public class RpcParamTransformPlugin implements ShenyuPlugin {

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
        return Mono.from(DataBufferUtils.join(serverHttpRequest.getBody())
                .flatMap(data -> Mono.just(Optional.of(data)))
                .defaultIfEmpty(Optional.empty())
                .flatMap(body -> {
                    body.ifPresent(dataBuffer -> exchange.getAttributes().put(Constants.PARAM_TRANSFORM, resolveBodyFromRequest(dataBuffer)));
                    return chain.execute(exchange);
                }));
    }

    private Mono<Void> formData(final ServerWebExchange exchange, final ServerHttpRequest serverHttpRequest, final ShenyuPluginChain chain) {
        return Mono.from(DataBufferUtils.join(serverHttpRequest.getBody())
                .flatMap(data -> Mono.just(Optional.of(data)))
                .defaultIfEmpty(Optional.empty())
                .flatMap(map -> {
                    if (map.isPresent()) {
                        String param = resolveBodyFromRequest(map.get());
                        LinkedMultiValueMap<String, String> linkedMultiValueMap;
                        try {
                            linkedMultiValueMap = BodyParamUtils.buildBodyParams(URLDecoder.decode(param, StandardCharsets.UTF_8.name()));
                        } catch (UnsupportedEncodingException e) {
                            return Mono.error(e);
                        }
                        exchange.getAttributes().put(Constants.PARAM_TRANSFORM, HttpParamConverter.toMap(() -> linkedMultiValueMap));
                    }
                    return chain.execute(exchange);
                }));
    }

    private Mono<Void> query(final ServerWebExchange exchange, final ServerHttpRequest serverHttpRequest, final ShenyuPluginChain chain) {
        exchange.getAttributes().put(Constants.PARAM_TRANSFORM, HttpParamConverter.ofString(() -> serverHttpRequest.getURI().getQuery()));
        return chain.execute(exchange);
    }

    @Override
    public boolean skip(final ServerWebExchange exchange) {
        return skipExcept(exchange,
                RpcTypeEnum.DUBBO,
                RpcTypeEnum.GRPC,
                RpcTypeEnum.TARS,
                RpcTypeEnum.MOTAN,
                RpcTypeEnum.SOFA,
                RpcTypeEnum.BRPC);
    }

    @NonNull
    private String resolveBodyFromRequest(final DataBuffer dataBuffer) {
        byte[] bytes = new byte[dataBuffer.readableByteCount()];
        dataBuffer.read(bytes);
        DataBufferUtils.release(dataBuffer);
        return new String(bytes, StandardCharsets.UTF_8);
    }
}
