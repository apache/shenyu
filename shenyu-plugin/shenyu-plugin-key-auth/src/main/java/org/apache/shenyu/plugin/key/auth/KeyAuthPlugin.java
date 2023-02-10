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

package org.apache.shenyu.plugin.key.auth;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.KeyAuthRuleHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.key.auth.handler.KeyAuthPluginDataHandler;
import org.springframework.http.HttpHeaders;
import org.springframework.util.MultiValueMap;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Objects;

public class KeyAuthPlugin extends AbstractShenyuPlugin {

    @Override
    public int getOrder() {
        return PluginEnum.KEY_AUTH.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.KEY_AUTH.getName();
    }

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain,
                                   final SelectorData selector, final RuleData rule) {

        KeyAuthRuleHandle keyAuthRuleHandle = KeyAuthPluginDataHandler.CACHED_HANDLE.get()
                .obtainHandle(CacheKeyUtils.INST.getKey(rule));
        if (Objects.isNull(keyAuthRuleHandle) || StringUtils.isBlank(keyAuthRuleHandle.getKeyName())
                || StringUtils.isBlank(keyAuthRuleHandle.getKey())) {
            Object error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.KEY_NAME_AND_KEY_MUST_BE_CONFIGURED);
            return WebFluxResultUtils.result(exchange, error);
        }
        if (checkKey(exchange, keyAuthRuleHandle.getKeyName(), keyAuthRuleHandle.getKey())) {
            return chain.execute(exchange);
        }
        Object error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.ERROR_KEY);
        return WebFluxResultUtils.result(exchange, error);
    }

    /**
     * Check the key.
     * @param exchange exchange
     * @param keyName key attribute name
     * @param key key
     * @return whether the key is correct.
     */
    private boolean checkKey(final ServerWebExchange exchange, final String keyName, final String key) {
        HttpHeaders httpHeaders = exchange.getRequest().getHeaders();
        if (StringUtils.equals(httpHeaders.getFirst(keyName), key)) {
            return true;
        }
        MultiValueMap<String, String> multiValueMap = exchange.getRequest().getQueryParams();
        if (StringUtils.equals(multiValueMap.getFirst(keyName), key)) {
            return true;
        }
        return false;
    }

}
