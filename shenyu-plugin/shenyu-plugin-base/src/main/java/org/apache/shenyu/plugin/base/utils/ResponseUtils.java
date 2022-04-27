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

package org.apache.shenyu.plugin.base.utils;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.support.BodyInserterContext;
import org.apache.shenyu.plugin.base.support.CachedBodyOutputMessage;
import org.reactivestreams.Publisher;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ReactiveHttpOutputMessage;
import org.springframework.http.codec.HttpMessageReader;
import org.springframework.http.codec.ServerCodecConfigurer;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.web.reactive.function.BodyInserter;
import org.springframework.web.reactive.function.BodyInserters;
import org.springframework.web.reactive.function.client.ClientResponse;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Objects;
import java.util.function.Function;

/**
 * ResponseUtils.
 */
public final class ResponseUtils {
    
    private static final String CHUNKED = "chunked";
    
    private ResponseUtils() {
    }
    
    /**
     * create CachedBodyOutputMessage.
     *
     * @param exchange ServerWebExchange
     * @return CachedBodyOutputMessage.
     */
    public static CachedBodyOutputMessage newCachedBodyOutputMessage(final ServerWebExchange exchange) {
        HttpHeaders headers = new HttpHeaders();
        headers.putAll(exchange.getRequest().getHeaders());
        headers.remove(HttpHeaders.CONTENT_LENGTH);
        return new CachedBodyOutputMessage(exchange, headers);
    }
    
    /**
     * build client response with current response data.
     *
     * @param response current response
     * @param body     current response body
     * @return the client response
     */
    public static ClientResponse buildClientResponse(final ServerHttpResponse response,
                                                     final Publisher<? extends DataBuffer> body) {
        ClientResponse.Builder builder = ClientResponse.create(Objects.requireNonNull(response.getStatusCode()), getReaders());
        return builder
                .headers(headers -> headers.putAll(response.getHeaders()))
                .cookies(cookies -> response.getCookies())
                .body(Flux.from(body)).build();
    }
    
    /**
     * fix the body message.
     *
     * @param response      current response
     * @param outputMessage cache message
     * @return fixed body message
     */
    public static Mono<DataBuffer> fixBodyMessage(final ServerHttpResponse response,
                                                  final CachedBodyOutputMessage outputMessage) {
        fixHeaders(response.getHeaders());
        return DataBufferUtils.join(outputMessage.getBody());
    }
    
    /**
     * release source.
     *
     * @param outputMessage CachedBodyOutputMessage
     * @param throwable     Throwable
     * @return Mono.
     */
    public static Mono<Void> release(final CachedBodyOutputMessage outputMessage, final Throwable throwable) {
        if (Boolean.TRUE.equals(outputMessage.getCache())) {
            return outputMessage.getBody().map(DataBufferUtils::release).then(Mono.error(throwable));
        }
        return Mono.error(throwable);
    }
    
    /**
     * Chunked Headers.
     *
     * @param headers headers.
     * @return chunked headers
     */
    public static HttpHeaders chunkedHeader(final HttpHeaders headers) {
        final HttpHeaders httpHeaders = new HttpHeaders();
        httpHeaders.putAll(headers);
        fixHeaders(httpHeaders);
        return httpHeaders;
    }

    /**
     * the response write with data.
     *
     * @param clientResponse the client response
      * @param exchange the exchange
     * @param publisher the publisher
     * @param elementClass the elementClass
     * @param <T> the element type
     * @param <P> the publishing type
     * @return the response wrapper data
     */
    public static <T, P extends Publisher<T>> Mono<Void> writeWith(final ClientResponse clientResponse,
                                                                   final ServerWebExchange exchange,
                                                                   final P publisher,
                                                                   final Class<T> elementClass) {
        BodyInserter<P, ReactiveHttpOutputMessage> bodyInserter = BodyInserters.fromPublisher(publisher, elementClass);
        CachedBodyOutputMessage outputMessage = ResponseUtils.newCachedBodyOutputMessage(exchange);
        return bodyInserter.insert(outputMessage, new BodyInserterContext()).then(Mono.defer(() -> {
            Mono<DataBuffer> messageBody = ResponseUtils.fixBodyMessage(exchange.getResponse(), outputMessage);
            exchange.getAttributes().put(Constants.CLIENT_RESPONSE_ATTR, clientResponse);
            return exchange.getResponse().writeWith(messageBody);
        })).onErrorResume((Function<Throwable, Mono<Void>>) throwable -> ResponseUtils.release(outputMessage, throwable));
    }

    /**
     * Gets reads from ServerCodecConfigurer with custom the codec.
     * @return ServerCodecConfigurer readers
     */
    private static List<HttpMessageReader<?>> getReaders() {
        return SpringBeanUtils.getInstance().getBean(ServerCodecConfigurer.class).getReaders();
    }
    
    /**
     * fix headers.
     *
     * @param httpHeaders the headers
     */
    private static void fixHeaders(final HttpHeaders httpHeaders) {
        httpHeaders.remove(HttpHeaders.CONTENT_LENGTH);
        httpHeaders.set(HttpHeaders.TRANSFER_ENCODING, CHUNKED);
    }
}
