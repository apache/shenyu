/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * Contributor license agreements.See the NOTICE file distributed with
 * This work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * he License.You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.dromara.soul.web.plugin.http;

import io.netty.handler.codec.http.DefaultHttpHeaders;
import io.netty.handler.codec.http.HttpMethod;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.enums.PluginTypeEnum;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.web.plugin.SoulPlugin;
import org.dromara.soul.web.plugin.SoulPluginChain;
import org.dromara.soul.web.plugin.hystrix.HttpCommand;
import org.dromara.soul.web.request.RequestDTO;
import org.dromara.soul.web.result.SoulResultEnum;
import org.dromara.soul.web.result.SoulResultUtils;
import org.dromara.soul.web.result.SoulResultWarp;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.buffer.NettyDataBuffer;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.server.reactive.AbstractServerHttpResponse;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.util.StringUtils;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.netty.NettyPipeline;
import reactor.netty.http.client.HttpClient;
import reactor.netty.http.client.HttpClientResponse;

import java.time.Duration;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.TimeoutException;

/**
 * The type Netty http client plugin.
 *
 * @author xiaoyu
 */
public class NettyHttpClientPlugin implements SoulPlugin {

    /**
     * logger.
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(HttpCommand.class);

    private final HttpClient httpClient;

    /**
     * Instantiates a new Netty http client plugin.
     *
     * @param httpClient the http client
     */
    public NettyHttpClientPlugin(final HttpClient httpClient) {
        this.httpClient = httpClient;
    }

    @Override
    public Mono<Void> execute(final ServerWebExchange exchange, final SoulPluginChain chain) {
        final RequestDTO body = exchange.getAttribute(Constants.REQUESTDTO);
        assert body != null;
        ServerHttpRequest request = exchange.getRequest();
        final HttpMethod method = HttpMethod.valueOf(request.getMethodValue());
        HttpHeaders filtered = request.getHeaders();
        final DefaultHttpHeaders httpHeaders = new DefaultHttpHeaders();
        filtered.forEach(httpHeaders::set);
        String url = exchange.getAttribute(Constants.HTTP_URL);
        if (StringUtils.isEmpty(url)) {
            Object error = SoulResultWarp.error(SoulResultEnum.CANNOT_FIND_URL.getCode(), SoulResultEnum.CANNOT_FIND_URL.getMsg(), null);
            return SoulResultUtils.result(exchange, error);
        }
        LOGGER.info("you request,The resulting urlPath is :{}", url);
        Flux<HttpClientResponse> responseFlux = this.httpClient.headers(headers -> headers.add(httpHeaders))
                .request(method).uri(url).send((req, nettyOutbound) ->
                        nettyOutbound.options(NettyPipeline.SendOptions::flushOnEach).send(
                                request.getBody().map(dataBuffer -> ((NettyDataBuffer) dataBuffer)
                                        .getNativeBuffer())))
                .responseConnection((res, connection) -> {
                    exchange.getAttributes().put(Constants.CLIENT_RESPONSE_ATTR, res);
                    exchange.getAttributes().put(Constants.CLIENT_RESPONSE_CONN_ATTR, connection);
                    ServerHttpResponse response = exchange.getResponse();
                    HttpHeaders headers = new HttpHeaders();
                    res.responseHeaders()
                            .forEach(entry -> headers.add(entry.getKey(), entry.getValue()));
                    String contentTypeValue = headers.getFirst(HttpHeaders.CONTENT_TYPE);
                    if (StringUtils.hasLength(contentTypeValue)) {
                        exchange.getAttributes().put(Constants.ORIGINAL_RESPONSE_CONTENT_TYPE_ATTR, contentTypeValue);
                    }
                    HttpStatus status = HttpStatus.resolve(res.status().code());
                    if (status != null) {
                        response.setStatusCode(status);
                    } else if (response instanceof AbstractServerHttpResponse) {
                        ((AbstractServerHttpResponse) response)
                                .setStatusCodeValue(res.status().code());
                    } else {
                        throw new IllegalStateException("Unable to set status code on response: "
                                + res.status().code() + ", " + response.getClass());
                    }
                    response.getHeaders().putAll(headers);

                    return Mono.just(res);
                });
        long timeout = (long) Optional.ofNullable(exchange.getAttribute(Constants.HTTP_TIME_OUT)).orElse(3000L);
        Duration duration = Duration.ofMillis(timeout);
        responseFlux = responseFlux.timeout(duration,
                Mono.error(new TimeoutException("Response took longer than timeout: "
                        + duration)))
                .onErrorMap(TimeoutException.class, th -> new ResponseStatusException(HttpStatus.GATEWAY_TIMEOUT, th.getMessage(), th));
        return responseFlux.then(chain.execute(exchange));

    }

    @Override
    public PluginTypeEnum pluginType() {
        return PluginTypeEnum.FUNCTION;
    }

    @Override
    public int getOrder() {
        return PluginEnum.DIVIDE.getCode() + 1;
    }

    @Override
    public Boolean skip(final ServerWebExchange exchange) {
        final RequestDTO requestDTO = exchange.getAttribute(Constants.REQUESTDTO);
        assert requestDTO != null;
        return !Objects.equals(RpcTypeEnum.HTTP.getName(), requestDTO.getRpcType())
                && !Objects.equals(RpcTypeEnum.SPRING_CLOUD.getName(), requestDTO.getRpcType());
    }

    @Override
    public String named() {
        return "NettyHttpClient";
    }
}
