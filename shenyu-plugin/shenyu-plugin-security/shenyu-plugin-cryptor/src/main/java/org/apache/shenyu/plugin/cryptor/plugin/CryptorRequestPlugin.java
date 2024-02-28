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

package org.apache.shenyu.plugin.cryptor.plugin;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.exception.ResponsiveException;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.base.utils.ServerWebExchangeUtils;
import org.apache.shenyu.plugin.cryptor.handler.CryptorRuleHandler;
import org.springframework.http.codec.HttpMessageReader;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.List;

/**
 * Cryptor request plugin.
 */
public class CryptorRequestPlugin extends AbstractCryptorPlugin {

    private final List<HttpMessageReader<?>> messageReaders;

    /**
     * CryptorRequestPlugin.
     *
     * @param messageReaders messageReaders
     */
    public CryptorRequestPlugin(final List<HttpMessageReader<?>> messageReaders) {
        this.messageReaders = messageReaders;
    }

    @Override
    protected Mono<Void> doExecute0(final ServerWebExchange exchange, final ShenyuPluginChain chain,
                                    final SelectorData selector, final RuleData rule, final CryptorRuleHandler ruleHandle) {
        return ServerWebExchangeUtils.rewriteRequestBody(exchange, messageReaders, originalBody ->
                        Mono.just(convert(originalBody, ruleHandle, exchange))
                ).flatMap(chain::execute)
                .onErrorResume(error -> {
                    if (error instanceof ResponsiveException) {
                        return WebFluxResultUtils.failedResult((ResponsiveException) error);
                    }
                    return Mono.error(error);
                });
    }

    @Override
    protected ShenyuResultEnum checkErrorEnum() {
        return ShenyuResultEnum.CRYPTOR_REQUEST_ERROR_CONFIGURATION;
    }

    @Override
    protected String fieldErrorParse(final String originalBody, final ServerWebExchange exchange) {
        throw new ResponsiveException(ShenyuResultEnum.CRYPTOR_REQUEST_ERROR_CONFIGURATION.getCode(), ShenyuResultEnum.CRYPTOR_REQUEST_ERROR_CONFIGURATION.getMsg() + "[fieldNames]", exchange);
    }

    @Override
    public int getOrder() {
        return PluginEnum.CRYPTOR_REQUEST.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.CRYPTOR_REQUEST.getName();
    }

}
