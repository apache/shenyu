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

import org.apache.shenyu.plugin.base.support.BodyInserterContext;
import org.apache.shenyu.plugin.base.support.CachedBodyOutputMessage;
import org.apache.shenyu.plugin.base.support.RequestDecorator;
import org.apache.shenyu.plugin.base.support.ResponseDecorator;
import org.springframework.http.ReactiveHttpOutputMessage;
import org.springframework.http.codec.HttpMessageReader;
import org.springframework.http.server.reactive.ServerHttpRequestDecorator;
import org.springframework.web.reactive.function.BodyInserter;
import org.springframework.web.reactive.function.BodyInserters;
import org.springframework.web.reactive.function.server.ServerRequest;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.function.Function;

public class ServerWebExchangeUtils {

    /**
     * Rewrites Request Body.
     *
     * @param exchange serverWebExchange
     * @param readers  reader to read request-body
     * @param convert  convert body to new body
     * @return Mono.
     */
    public static Mono<ServerWebExchange> rewriteRequestBody(final ServerWebExchange exchange,
                                                             final List<HttpMessageReader<?>> readers,
                                                             final Function<String, Mono<String>> convert) {

        ServerRequest serverRequest = ServerRequest.create(exchange, readers);
        CachedBodyOutputMessage outputMessage = ResponseUtils.newCachedBodyOutputMessage(exchange);

        return serverRequest.bodyToMono(String.class)
                .switchIfEmpty(Mono.defer(() -> Mono.just("")))
                .flatMap(convert)
                .flatMap(body -> {
                    BodyInserter<String, ReactiveHttpOutputMessage> bodyInserter = BodyInserters.fromValue(body);
                    return bodyInserter.insert(outputMessage, new BodyInserterContext());
                }).then(Mono.defer(() -> {
                    ServerHttpRequestDecorator decorator = new RequestDecorator(exchange, outputMessage);
                    return Mono.just(exchange.mutate().request(decorator).build());
                })).onErrorResume(throwable -> ResponseUtils.release(outputMessage, throwable));

    }

    /**
     * Rewrites Response Body.
     *
     * @param exchange serverWebExchange
     * @param convert  convert body to new body
     * @return Mono.
     */
    public static ServerWebExchange rewriteResponseBody(final ServerWebExchange exchange,
                                                        final Function<String, String> convert) {
        return exchange.mutate()
                .response(new ResponseDecorator(exchange, convert)).build();
    }
}
