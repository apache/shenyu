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

package org.apache.shenyu.plugin.jwt;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.jwt.config.JwtConfig;
import org.apache.shenyu.plugin.jwt.handle.JwtPluginDataHandler;
import org.apache.shenyu.plugin.jwt.rule.JwtRuleHandle;
import org.apache.shenyu.plugin.jwt.strategy.JwtConvertStrategy;
import org.apache.shenyu.plugin.jwt.strategy.JwtConvertStrategyFactory;
import org.apache.shenyu.plugin.jwt.strategy.JwtPayloadParseStrategy;
import org.apache.shenyu.plugin.jwt.strategy.JwtPayloadParseStrategyFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Map;
import java.util.Objects;

/**
 * Jwt Plugin.
 */
public class JwtPlugin extends AbstractShenyuPlugin {

    private static final String TOKEN = "token";

    private static final String AUTH2_TOKEN = "Bearer";

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
        JwtConfig jwtConfig = Singleton.INST.get(JwtConfig.class);
        String authorization = exchange.getRequest().getHeaders().getFirst(HttpHeaders.AUTHORIZATION);
        String token = exchange.getRequest().getHeaders().getFirst(TOKEN);
        // check secreteKey
        if (StringUtils.isEmpty(jwtConfig.getSecretKey())) {
            Object error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.SECRET_KEY_MUST_BE_CONFIGURED);
            return WebFluxResultUtils.result(exchange, error);
        }
        // compatible processing
        String finalAuthorization = compatible(token, authorization);
        Map<String, Object> jwtBody = parseJwtPayload(finalAuthorization, jwtConfig);
        if (Objects.isNull(jwtBody)) {
            Object error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.ERROR_TOKEN);
            return WebFluxResultUtils.result(exchange, error);
        }
        return chain.execute(executeRuleHandle(rule, exchange, jwtBody));
    }

    @Override
    public String named() {
        return PluginEnum.JWT.getName();
    }

    @Override
    public int getOrder() {
        return PluginEnum.JWT.getCode();
    }

    private ServerWebExchange executeRuleHandle(final RuleData ruleData, final ServerWebExchange exchange, final Map<String, Object> jwtBody) {
        JwtRuleHandle jwtRuleHandle = JwtPluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(ruleData));
        if (Objects.isNull(jwtRuleHandle)) {
            return exchange;
        }
        JwtConvertStrategy convertStrategy = JwtConvertStrategyFactory.newInstance(jwtRuleHandle.getHandleType());
        return convertStrategy.convert(jwtRuleHandle, exchange, jwtBody);
    }

    /**
     * Both are compatible.
     *
     * @param token         header of token
     * @param authorization header of authorization
     * @return the authorization after processing
     */
    private String compatible(final String token, final String authorization) {
        String finalAuthorization;
        if (StringUtils.isNotEmpty(token)) {
            finalAuthorization = token;
        } else if (StringUtils.isNotEmpty(authorization)) {
            finalAuthorization = authorization;
        } else {
            return null;
        }
        return isAuth2(finalAuthorization) ? finalAuthorization.split(" ")[1] : finalAuthorization;
    }

    private boolean isAuth2(final String authorization) {
        return authorization.contains(AUTH2_TOKEN);
    }

    /**
     * parse jwt payload.
     *
     * @param authorization the authorization after processing
     * @param jwtConfig     the jwt config
     * @return Map
     */
    private Map<String, Object> parseJwtPayload(final String authorization,
                                                final JwtConfig jwtConfig) {
        if (StringUtils.isEmpty(authorization)) {
            return null;
        }
        JwtPayloadParseStrategy payloadParseStrategy = JwtPayloadParseStrategyFactory.newInstance(jwtConfig.getHandleType());
        return payloadParseStrategy.parse(jwtConfig.getSecretKey(), authorization);
    }
}
