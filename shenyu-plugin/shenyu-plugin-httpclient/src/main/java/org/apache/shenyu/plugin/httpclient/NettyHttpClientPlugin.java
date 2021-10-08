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

package org.apache.shenyu.plugin.httpclient;

import io.netty.handler.codec.http.DefaultHttpHeaders;
import io.netty.handler.codec.http.HttpMethod;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
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
import reactor.netty.http.client.HttpClient;
import reactor.netty.http.client.HttpClientResponse;

import java.time.Duration;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.TimeoutException;

/**
 * The type Netty http client plugin.
 */
public class NettyHttpClientPlugin implements ShenyuPlugin {

    private static final Logger LOG = LoggerFactory.getLogger(NettyHttpClientPlugin.class);

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
    public Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        final ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        assert shenyuContext != null;
        ServerHttpRequest request = exchange.getRequest();
        final HttpMethod method = HttpMethod.valueOf(request.getMethodValue());
        HttpHeaders filtered = request.getHeaders();
        final DefaultHttpHeaders httpHeaders = new DefaultHttpHeaders();
        filtered.forEach(httpHeaders::set);
        String url = exchange.getAttribute(Constants.HTTP_URL);
        if (StringUtils.isEmpty(url)) {
            Object error = ShenyuResultWrap.error(ShenyuResultEnum.CANNOT_FIND_URL.getCode(), ShenyuResultEnum.CANNOT_FIND_URL.getMsg(), null);
            return WebFluxResultUtils.result(exchange, error);
        }
        LOG.info("you request, The resulting urlPath is: {}", url);
        Flux<HttpClientResponse> responseFlux = this.httpClient.headers(headers -> headers.add(httpHeaders))
                .request(method).uri(url).send((req, nettyOutbound) ->
                        nettyOutbound.send(request.getBody().map(dataBuffer -> ((NettyDataBuffer) dataBuffer) .getNativeBuffer())))
                .responseConnection((res, connection) -> {
                    exchange.getAttributes().put(Constants.CLIENT_RESPONSE_ATTR, res);
                    exchange.getAttributes().put(Constants.CLIENT_RESPONSE_CONN_ATTR, connection);
                    ServerHttpResponse response = exchange.getResponse();
                    HttpHeaders headers = new HttpHeaders();
                    res.responseHeaders().forEach(entry -> headers.add(entry.getKey(), entry.getValue()));
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
                        throw new IllegalStateException("Unable to set status code on response: " + res.status().code() + ", " + response.getClass());
                    }
                    response.getHeaders().putAll(headers);

                    return Mono.just(res);
                });
        long timeout = (long) Optional.ofNullable(exchange.getAttribute(Constants.HTTP_TIME_OUT)).orElse(3000L);
        Duration duration = Duration.ofMillis(timeout);
        responseFlux = responseFlux.timeout(duration,
                Mono.error(new TimeoutException("Response took longer than timeout: " + duration)))
                .onErrorMap(TimeoutException.class, th -> new ResponseStatusException(HttpStatus.GATEWAY_TIMEOUT, th.getMessage(), th));
        return responseFlux.then(chain.execute(exchange));

    }

    @Override
    public int getOrder() {
        return PluginEnum.NETTY_HTTP_CLIENT.getCode();
    }

    @Override
    public boolean skip(final ServerWebExchange exchange) {
        final ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        assert shenyuContext != null;
        return !Objects.equals(RpcTypeEnum.HTTP.getName(), shenyuContext.getRpcType())
                && !Objects.equals(RpcTypeEnum.SPRING_CLOUD.getName(), shenyuContext.getRpcType());
    }

    @Override
    public String named() {
        return PluginEnum.NETTY_HTTP_CLIENT.getName();
    }
}
