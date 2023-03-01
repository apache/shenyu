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

package org.apache.shenyu.plugin.cryptor.response;

import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.exception.ResponsiveException;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.base.utils.ServerWebExchangeUtils;
import org.apache.shenyu.plugin.cryptor.handler.CryptorResponsePluginDataHandler;
import org.apache.shenyu.plugin.cryptor.handler.CryptorRuleHandler;
import org.apache.shenyu.plugin.cryptor.utils.CryptorUtil;
import org.apache.shenyu.plugin.cryptor.utils.JsonUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Objects;

/**
 * Cryptor response plugin.
 */
public class CryptorResponsePlugin extends AbstractShenyuPlugin {

    private static final Logger LOG = LoggerFactory.getLogger(CryptorResponsePlugin.class);

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
        CryptorRuleHandler ruleHandle = CryptorResponsePluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(rule));
        if (Objects.isNull(ruleHandle)) {
            LOG.error("Cryptor response rule configuration is null :{}", rule.getId());
            return chain.execute(exchange);
        }
        Pair<Boolean, String> pair = CryptorUtil.checkParam(ruleHandle);
        if (Boolean.TRUE.equals(pair.getLeft())) {
            Object error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.CRYPTOR_RESPONSE_ERROR_CONFIGURATION.getCode(),
                    ShenyuResultEnum.CRYPTOR_RESPONSE_ERROR_CONFIGURATION.getMsg() + "[" + pair.getRight() + "]", null);
            return WebFluxResultUtils.result(exchange, error);
        }

        ServerWebExchange newExchange = ServerWebExchangeUtils.rewriteResponseBody(exchange, originalBody -> convert(originalBody, ruleHandle, exchange));

        return chain.execute(newExchange).onErrorResume(error -> {
            if (error instanceof ResponsiveException) {
                return WebFluxResultUtils.failedResult((ResponsiveException) error);
            }
            return Mono.error(error);
        });
    }

    private String convert(final String originalBody, final CryptorRuleHandler ruleHandle, final ServerWebExchange exchange) {

        String parseBody = JsonUtil.parser(originalBody, ruleHandle.getFieldNames());

        if (Objects.isNull(parseBody)) {
            return originalBody;
        }

        return CryptorUtil.crypt(ruleHandle, parseBody, originalBody, exchange);
    }

    @Override
    public int getOrder() {
        return PluginEnum.CRYPTOR_RESPONSE.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.CRYPTOR_RESPONSE.getName();
    }
}
