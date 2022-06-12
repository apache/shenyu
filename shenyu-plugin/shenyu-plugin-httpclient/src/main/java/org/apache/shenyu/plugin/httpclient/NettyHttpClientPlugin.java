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

import io.netty.handler.codec.http.HttpMethod;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.PluginEnum;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.NettyDataBuffer;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.server.reactive.AbstractServerHttpResponse;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.netty.http.client.HttpClient;
import reactor.netty.http.client.HttpClientResponse;

import java.net.URI;

/**
 * The type Netty http client plugin.
 */
public class NettyHttpClientPlugin extends AbstractHttpClientPlugin<HttpClientResponse> {

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
    protected Mono<HttpClientResponse> doRequest(final ServerWebExchange exchange, final String httpMethod, final URI uri,
                                final HttpHeaders httpHeaders, final Flux<DataBuffer> body) {
        return Mono.from(httpClient.headers(headers -> httpHeaders.forEach(headers::add))
                .request(HttpMethod.valueOf(httpMethod)).uri(uri.toASCIIString())
                .send((req, nettyOutbound) -> nettyOutbound.send(body.map(dataBuffer -> ((NettyDataBuffer) dataBuffer).getNativeBuffer())))
                .responseConnection((res, connection) -> {
                    exchange.getAttributes().put(Constants.CLIENT_RESPONSE_ATTR, res);
                    exchange.getAttributes().put(Constants.CLIENT_RESPONSE_CONN_ATTR, connection);
                    ServerHttpResponse response = exchange.getResponse();
                    HttpHeaders headers = new HttpHeaders();
                    res.responseHeaders().forEach(entry -> headers.add(entry.getKey(), entry.getValue()));
                    String contentTypeValue = headers.getFirst(HttpHeaders.CONTENT_TYPE);
                    if (StringUtils.isNotBlank(contentTypeValue)) {
                        exchange.getAttributes().put(Constants.ORIGINAL_RESPONSE_CONTENT_TYPE_ATTR, contentTypeValue);
                    }
                    HttpStatus status = HttpStatus.resolve(res.status().code());
                    if (status != null) {
                        response.setStatusCode(status);
                    } else if (response instanceof AbstractServerHttpResponse) {
                        response.setRawStatusCode(res.status().code());
                    } else {
                        throw new IllegalStateException("Unable to set status code on response: " + res.status().code() + ", " + response.getClass());
                    }
                    response.getHeaders().putAll(headers);
                    return Mono.just(res);
                }));
    }

    @Override
    public int getOrder() {
        return PluginEnum.NETTY_HTTP_CLIENT.getCode();
    }

    @Override
    public boolean skip(final ServerWebExchange exchange) {
        return skipExceptHttpLike(exchange);
    }

    @Override
    public String named() {
        return PluginEnum.NETTY_HTTP_CLIENT.getName();
    }
}
