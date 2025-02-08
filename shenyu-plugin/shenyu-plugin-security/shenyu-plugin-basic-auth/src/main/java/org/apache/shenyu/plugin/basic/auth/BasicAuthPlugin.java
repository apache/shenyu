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

package org.apache.shenyu.plugin.basic.auth;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.basic.auth.handle.BasicAuthPluginDataHandler;
import org.apache.shenyu.plugin.basic.auth.rule.BasicAuthRuleHandle;
import org.apache.shenyu.plugin.basic.auth.strategy.BasicAuthAuthenticationStrategy;
import org.springframework.http.HttpHeaders;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Objects;
import java.util.Optional;

/**
 * basic-auth Plugin.
 */
public class BasicAuthPlugin extends AbstractShenyuPlugin {

    /**
     * this is Template Method child has Implement your own logic.
     *
     * @param exchange exchange the current server exchange {@linkplain ServerWebExchange}
     * @param chain    chain the current chain  {@linkplain ServerWebExchange}
     * @param selector selector    {@linkplain SelectorData}
     * @param rule     rule    {@linkplain RuleData}
     * @return {@code Mono<Void>} to indicate when request handling is complete
     */
    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
        String authorization = StringUtils.defaultString(exchange.getRequest().getHeaders().getFirst(HttpHeaders.AUTHORIZATION), exchange.getRequest().getURI().getUserInfo());
        BasicAuthRuleHandle basicAuthRuleHandle = BasicAuthPluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(rule));
        BasicAuthAuthenticationStrategy authenticationStrategy = Optional.ofNullable(basicAuthRuleHandle).map(BasicAuthRuleHandle::getBasicAuthAuthenticationStrategy).orElse(null);

        if (Objects.nonNull(authenticationStrategy) && authenticationStrategy.authenticate(basicAuthRuleHandle, authorization)) {
            return chain.execute(exchange);
        }
        return WebFluxResultUtils.result(exchange, ShenyuResultWrap.error(exchange, ShenyuResultEnum.ERROR_TOKEN));
    }

    @Override
    public String named() {
        return PluginEnum.BASIC_AUTH.getName();
    }

    @Override
    public int getOrder() {
        return PluginEnum.BASIC_AUTH.getCode();
    }
}
