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

import org.apache.shenyu.plugin.base.utils.ResponseUtils;
import org.apache.shenyu.plugin.cryptor.handler.CryptorRuleHandler;
import org.apache.shenyu.plugin.cryptor.strategy.CryptorStrategyFactory;
import org.apache.shenyu.plugin.cryptor.utils.CryptorUtil;
import org.apache.shenyu.plugin.cryptor.utils.JsonUtil;
import org.reactivestreams.Publisher;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.http.server.reactive.ServerHttpResponseDecorator;
import org.springframework.web.reactive.function.client.ClientResponse;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.util.annotation.NonNull;

import java.util.Objects;

/**
 * Build and modify the response class.
 */
public class CryptorResponseDecorator extends ServerHttpResponseDecorator {

    private final ServerWebExchange exchange;

    private final CryptorRuleHandler ruleHandle;

    public CryptorResponseDecorator(final ServerWebExchange exchange,
                                    final CryptorRuleHandler ruleHandle) {
        super(exchange.getResponse());
        this.exchange = exchange;
        this.ruleHandle = ruleHandle;
    }

    @Override
    @NonNull
    @SuppressWarnings("unchecked")
    public Mono<Void> writeWith(@NonNull final Publisher<? extends DataBuffer> body) {
        ClientResponse clientResponse = ResponseUtils.buildClientResponse(this.getDelegate(), body);
        Mono<String> mono = clientResponse.bodyToMono(String.class).flatMap(originalBody ->
                        strategyMatch(originalBody, this.ruleHandle, this.exchange));
        return ResponseUtils.writeWith(clientResponse, this.exchange, mono, String.class);
    }

    @SuppressWarnings("rawtypes")
    private Mono strategyMatch(final String originalBody, final CryptorRuleHandler ruleHandle, final ServerWebExchange exchange) {
        String parseBody = JsonUtil.parser(originalBody, ruleHandle.getFieldNames());
        if (Objects.isNull(parseBody)) {
            return Mono.just(originalBody);
        }
        String modifiedBody = CryptorStrategyFactory.match(ruleHandle, parseBody);
        if (Objects.isNull(modifiedBody)) {
            return CryptorUtil.fail(ruleHandle.getWay(), exchange);
        }
        return CryptorUtil.success(originalBody, modifiedBody, ruleHandle.getWay(), ruleHandle.getFieldNames());
    }
}
