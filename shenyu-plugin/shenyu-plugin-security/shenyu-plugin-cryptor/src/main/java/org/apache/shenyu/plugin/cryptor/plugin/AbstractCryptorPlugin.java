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

import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.cryptor.handler.AbstractCryptorPluginDataHandler;
import org.apache.shenyu.plugin.cryptor.handler.CryptorRuleHandler;
import org.apache.shenyu.plugin.cryptor.strategy.MapTypeEnum;
import org.apache.shenyu.plugin.cryptor.utils.CryptorUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Objects;

/**
 * AbstractCryptorPlugin.
 */
public abstract class AbstractCryptorPlugin extends AbstractShenyuPlugin {

    private static final Logger LOG = LoggerFactory.getLogger(AbstractCryptorPlugin.class);

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
        final CryptorRuleHandler ruleHandle = AbstractCryptorPluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(rule));
        if (Objects.isNull(ruleHandle)) {
            LOG.error("{} rule configuration is null :{}", named(), rule.getId());
            return chain.execute(exchange);
        }

        Pair<Boolean, String> pair = CryptorUtil.checkParam(ruleHandle);
        if (Boolean.TRUE.equals(pair.getLeft())) {
            ShenyuResultEnum resultEnum = checkErrorEnum();
            return WebFluxResultUtils.failedResult(resultEnum.getCode(),
                    resultEnum.getMsg() + "[" + pair.getRight() + "]", exchange);
        }
        return doExecute0(exchange, chain, selector, rule, ruleHandle);
    }

    protected abstract Mono<Void> doExecute0(ServerWebExchange exchange, ShenyuPluginChain chain,
                                             SelectorData selector, RuleData rule, CryptorRuleHandler ruleHandle);
    
    protected abstract ShenyuResultEnum checkErrorEnum();

    /**
     * field parse error diff handler.
     *
     * @param originalBody originalBody
     * @param exchange exchange
     * @return String
     */
    protected abstract String fieldErrorParse(String originalBody, ServerWebExchange exchange);
    
    protected String convert(final String originalBody, final CryptorRuleHandler ruleHandle, final ServerWebExchange exchange) {
        String converted = MapTypeEnum.mapType(ruleHandle.getMapType()).convert(originalBody, ruleHandle, exchange);
        if (Objects.isNull(converted)) {
            return fieldErrorParse(originalBody, exchange);
        }
        return converted;
    }

}
