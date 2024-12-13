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

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.ResultEnum;
import org.apache.shenyu.common.enums.UniqueHeaderEnum;
import org.apache.shenyu.plugin.base.utils.MediaTypeUtils;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.web.reactive.function.client.WebClient;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.net.URI;

/**
 * The type Web client plugin.
 */
public class WebClientPlugin extends AbstractHttpClientPlugin<ResponseEntity<Flux<DataBuffer>>> {
    
    private final WebClient webClient;

    /**
     * Instantiates a new Web client plugin.
     *
     * @param webClient the web client
     */
    public WebClientPlugin(final WebClient webClient) {
        this.webClient = webClient;
    }
    
    @Override
    protected Mono<ResponseEntity<Flux<DataBuffer>>> doRequest(final ServerWebExchange exchange, final String httpMethod,
                                             final URI uri, final Flux<DataBuffer> body) {
        // springWebflux5.3 mark #exchange() deprecated. because #echange maybe make memory leak.
        // https://github.com/spring-projects/spring-framework/issues/25751
        // exchange is deprecated, so change to {@link WebClient.RequestHeadersSpec#exchangeToMono(Function)}
        ServerHttpRequest request = exchange.getRequest();
        final HttpHeaders httpHeaders = new HttpHeaders(request.getHeaders());
        this.duplicateHeaders(exchange, httpHeaders, UniqueHeaderEnum.REQ_UNIQUE_HEADER);
        final WebClient.ResponseSpec responseSpec = webClient.method(HttpMethod.valueOf(httpMethod)).uri(uri)
                .headers(headers -> {
                    headers.addAll(exchange.getRequest().getHeaders());
                    headers.remove(HttpHeaders.HOST);
                    Boolean preserveHost = exchange.getAttributeOrDefault(Constants.PRESERVE_HOST, Boolean.FALSE);
                    if (preserveHost) {
                        headers.add(HttpHeaders.HOST, request.getHeaders().getFirst(HttpHeaders.HOST));
                    }
                })
                .body((outputMessage, context) -> {
                    MediaType mediaType = exchange.getRequest().getHeaders().getContentType();
                    if (MediaTypeUtils.isByteType(mediaType)) {
                        return outputMessage.writeWith(body);
                    }
                    // fix chinese garbled code
                    return outputMessage.writeWith(DataBufferUtils.join(body));
                })
                .retrieve()
                // cover DefaultResponseSpec#DEFAULT_STATUS_HANDLER
                .onRawStatus(httpStatus -> httpStatus >= 400, clientResponse -> Mono.empty());
        return responseSpec.toEntityFlux(DataBuffer.class)
                .flatMap(fluxResponseEntity -> {
                    if (fluxResponseEntity.getStatusCode().is2xxSuccessful()) {
                        exchange.getAttributes().put(Constants.CLIENT_RESPONSE_RESULT_TYPE, ResultEnum.SUCCESS.getName());
                    } else {
                        exchange.getAttributes().put(Constants.CLIENT_RESPONSE_RESULT_TYPE, ResultEnum.ERROR.getName());
                    }
                    HttpHeaders headers = new HttpHeaders();
                    headers.addAll(fluxResponseEntity.getHeaders());
                    this.duplicateHeaders(exchange, headers, UniqueHeaderEnum.RESP_UNIQUE_HEADER);
                    exchange.getResponse().getHeaders().putAll(headers);
                    exchange.getResponse().setStatusCode(fluxResponseEntity.getStatusCode());
                    exchange.getAttributes().put(Constants.CLIENT_RESPONSE_ATTR, fluxResponseEntity);
                    return Mono.just(fluxResponseEntity);
                });
    }

    @Override
    public int getOrder() {
        return PluginEnum.WEB_CLIENT.getCode();
    }
    
    @Override
    public String named() {
        return PluginEnum.WEB_CLIENT.getName();
    }
    
    @Override
    public boolean skip(final ServerWebExchange exchange) {
        return skipExceptHttpLike(exchange);
    }
}
