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

package org.apache.shenyu.plugin.cryptor.request;

import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.exception.ResponsiveException;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.base.utils.ServerWebExchangeUtils;
import org.apache.shenyu.plugin.cryptor.handler.CryptorRequestPluginDataHandler;
import org.apache.shenyu.plugin.cryptor.handler.CryptorRuleHandler;
import org.apache.shenyu.plugin.cryptor.strategy.CryptorStrategyFactory;
import org.apache.shenyu.plugin.cryptor.utils.CryptorUtil;
import org.apache.shenyu.plugin.cryptor.utils.JsonUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.codec.HttpMessageReader;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Objects;
import java.util.Optional;

/**
 * Cryptor request plugin.
 */
public class CryptorRequestPlugin extends AbstractShenyuPlugin {

    private static final Logger LOG = LoggerFactory.getLogger(CryptorRequestPlugin.class);

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
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
        CryptorRuleHandler ruleHandle = CryptorRequestPluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(rule));
        if (Objects.isNull(ruleHandle)) {
            LOG.error("Cryptor request rule configuration is null :{}", rule.getId());
            return chain.execute(exchange);
        }

        Pair<Boolean, String> pair = JsonUtil.checkParam(ruleHandle);
        if (Boolean.TRUE.equals(pair.getLeft())) {
            ShenyuResultEnum resultEnum = ShenyuResultEnum.CRYPTOR_REQUEST_ERROR_CONFIGURATION;
            return WebFluxResultUtils.failedResult(resultEnum.getCode(),
                    resultEnum.getMsg() + "[" + pair.getRight() + "]", exchange);
        }

        return ServerWebExchangeUtils.rewriteRequestBody(exchange, messageReaders, originalBody ->
                Mono.just(convert(ruleHandle, originalBody, exchange))
        ).flatMap(chain::execute)
                .onErrorResume(error -> {
                    if (error instanceof ResponsiveException) {
                        return WebFluxResultUtils.failedResult((ResponsiveException) error);
                    }
                    return Mono.error(error);
                });
    }

    @Override
    public int getOrder() {
        return PluginEnum.CRYPTOR_REQUEST.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.CRYPTOR_REQUEST.getName();
    }

    private String convert(final CryptorRuleHandler ruleHandle, final String originalBody, final ServerWebExchange exchange) {
        String parseBody = JsonUtil.parser(originalBody, ruleHandle.getFieldNames());
        if (Objects.isNull(parseBody)) {
            throw new ResponsiveException(ShenyuResultEnum.CRYPTOR_REQUEST_ERROR_CONFIGURATION.getCode(), ShenyuResultEnum.CRYPTOR_REQUEST_ERROR_CONFIGURATION.getMsg() + "[fieldNames]", exchange);
        }

        String modifiedBody = CryptorStrategyFactory.match(ruleHandle, parseBody);
        if (Objects.isNull(modifiedBody)) {
            throw Optional.ofNullable(ruleHandle.getWay())
                    .filter(CryptorStrategyFactory.DECRYPT::equals)
                    .map(data -> new ResponsiveException(ShenyuResultEnum.DECRYPTION_ERROR, exchange))
                    .orElse(new ResponsiveException(ShenyuResultEnum.ENCRYPTION_ERROR, exchange));
        }
        return CryptorUtil.replace(originalBody, modifiedBody, ruleHandle.getWay(), ruleHandle.getFieldNames());
    }
}
