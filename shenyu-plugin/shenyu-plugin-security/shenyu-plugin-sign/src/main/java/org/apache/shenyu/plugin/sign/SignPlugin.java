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

package org.apache.shenyu.plugin.sign;

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
import org.apache.shenyu.plugin.sign.service.SignService;
import org.apache.shenyu.plugin.sign.api.VerifyResult;
import org.apache.shenyu.plugin.sign.handler.SignPluginDataHandler;
import org.apache.shenyu.plugin.sign.handler.SignRuleHandler;
import org.springframework.http.codec.HttpMessageReader;
import org.springframework.util.ObjectUtils;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.List;

/**
 * Sign Plugin.
 */
public class SignPlugin extends AbstractShenyuPlugin {

    private final List<HttpMessageReader<?>> messageReaders;

    private final SignService signService;

    /**
     * Instantiates a new Sign plugin.
     *
     * @param readers     the sign use readers
     * @param signService the sign service
     */
    public SignPlugin(final List<HttpMessageReader<?>> readers, final SignService signService) {
        this.signService = signService;
        messageReaders = readers;
    }

    @Override
    public String named() {
        return PluginEnum.SIGN.getName();
    }

    @Override
    public int getOrder() {
        return PluginEnum.SIGN.getCode();
    }

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selectorData, final RuleData rule) {
        SignRuleHandler ruleHandler = SignPluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(rule));
        if (ObjectUtils.isEmpty(ruleHandler) || !ruleHandler.getSignRequestBody()) {
            VerifyResult result = signService.signatureVerify(exchange);
            if (result.isFailed()) {
                return WebFluxResultUtils.failedResult(ShenyuResultEnum.SIGN_IS_NOT_PASS.getCode(),
                        result.getReason(), exchange);
            }
            return chain.execute(exchange);
        }

        return ServerWebExchangeUtils.rewriteRequestBody(exchange, messageReaders, body -> {
            VerifyResult result = signVerifyWithBody(body, exchange);
            if (result.isSuccess()) {
                return Mono.just(body);
            }
            throw new ResponsiveException(ShenyuResultEnum.SIGN_IS_NOT_PASS.getCode(), result.getReason(), exchange);
        }).flatMap(chain::execute)
                .onErrorResume(error -> {
                    if (error instanceof ResponsiveException) {
                        return WebFluxResultUtils.failedResult((ResponsiveException) error);
                    }
                    return Mono.error(error);
                });
    }

    private VerifyResult signVerifyWithBody(final String originalBody, final ServerWebExchange exchange) {
        // get url params
        return signService.signatureVerify(exchange, originalBody);
    }
}
