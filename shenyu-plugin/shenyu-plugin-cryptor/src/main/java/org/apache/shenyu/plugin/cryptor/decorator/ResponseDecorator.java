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

package org.apache.shenyu.plugin.cryptor.decorator;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.plugin.base.support.BodyInserterContext;
import org.apache.shenyu.plugin.base.support.CachedBodyOutputMessage;
import org.apache.shenyu.plugin.base.utils.ClientResponseUtils;
import org.apache.shenyu.plugin.cryptor.dto.CryptorRuleHandle;
import org.apache.shenyu.plugin.cryptor.strategy.CryptorStrategyFactory;
import org.apache.shenyu.plugin.cryptor.utils.HttpUtil;
import org.apache.shenyu.plugin.cryptor.utils.JsonUtil;
import org.reactivestreams.Publisher;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.http.ReactiveHttpOutputMessage;
import org.springframework.http.server.reactive.ServerHttpResponseDecorator;
import org.springframework.web.reactive.function.BodyInserter;
import org.springframework.web.reactive.function.BodyInserters;
import org.springframework.web.reactive.function.client.ClientResponse;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.util.annotation.NonNull;

import java.util.function.Function;

/**
 * Build and modify the response class.
 */
public class ResponseDecorator extends ServerHttpResponseDecorator {

    private final ServerWebExchange exchange;

    private final CryptorRuleHandle ruleHandle;

    public ResponseDecorator(final ServerWebExchange exchange,
                             final CryptorRuleHandle ruleHandle) {
        super(exchange.getResponse());
        this.exchange = exchange;
        this.ruleHandle = ruleHandle;
    }

    @Override
    @NonNull
    @SuppressWarnings("unchecked")
    public Mono<Void> writeWith(@NonNull final Publisher<? extends DataBuffer> body) {
        ClientResponse clientResponse = ClientResponseUtils.buildClientResponse(this.getDelegate(), body);
        Mono<String> mono = clientResponse.bodyToMono(String.class).flatMap(originalBody ->
                        strategyMatch(originalBody, this.ruleHandle, this.exchange));
        BodyInserter<Mono<String>, ReactiveHttpOutputMessage> bodyInserter = BodyInserters.fromPublisher(mono, String.class);
        CachedBodyOutputMessage outputMessage = HttpUtil.newCachedBodyOutputMessage(exchange);
        return bodyInserter.insert(outputMessage, new BodyInserterContext())
                .then(Mono.defer(() -> {
                    Mono<DataBuffer> messageBody = ClientResponseUtils.fixBodyMessage(this.getDelegate(), outputMessage);
                    exchange.getAttributes().put(Constants.CLIENT_RESPONSE_ATTR, clientResponse);
                    return getDelegate().writeWith(messageBody);
                })).onErrorResume((Function<Throwable, Mono<Void>>) throwable -> HttpUtil.release(outputMessage, throwable));
    }

    @SuppressWarnings("rawtypes")
    private Mono strategyMatch(final String originalBody, final CryptorRuleHandle ruleHandle, final ServerWebExchange exchange) {
        String parseBody = JsonUtil.parser(originalBody, ruleHandle.getFieldNames());
        if (parseBody == null) {
            return Mono.just(originalBody);
        }
        String modifiedBody = CryptorStrategyFactory.match(ruleHandle, parseBody);
        if (modifiedBody == null) {
            return HttpUtil.fail(ruleHandle.getWay(), exchange);
        }
        return HttpUtil.success(originalBody, modifiedBody, ruleHandle.getWay(), ruleHandle.getFieldNames());
    }
}
