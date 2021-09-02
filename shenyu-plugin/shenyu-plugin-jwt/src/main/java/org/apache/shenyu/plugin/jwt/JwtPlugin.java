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

import io.jsonwebtoken.JwtParser;
import io.jsonwebtoken.Jwts;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.CollectionUtils;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.jwt.config.JwtConfig;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.jwt.exception.ThrowingFunction;
import org.springframework.http.HttpHeaders;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.nio.charset.StandardCharsets;
import java.util.List;

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
        // filter path
        String path = exchange.getRequest().getURI().getPath();
        List<String> filterPath = jwtConfig.getFilterPath();
        if (CollectionUtils.isNotEmpty(filterPath)) {
            if (filterPath.contains(path)) {
                return chain.execute(exchange);
            }
        }
        // check secreteKey
        if (StringUtils.isEmpty(jwtConfig.getSecretKey())) {
            Object error = ShenyuResultWrap.error(ShenyuResultEnum.SECRET_KEY_MUST_BE_CONFIGURED.getCode(), ShenyuResultEnum.SECRET_KEY_MUST_BE_CONFIGURED.getMsg(), null);
            return WebFluxResultUtils.result(exchange, error);
        }
        // compatible processing
        String finalAuthorization = compatible(token, authorization);
        return checkAuthorization(exchange, chain, finalAuthorization, jwtConfig.getSecretKey());
    }

    @Override
    public String named() {
        return PluginEnum.JWT.getName();
    }

    @Override
    public int getOrder() {
        return PluginEnum.JWT.getCode();
    }

    private String compatible(final String token, final String authorization) {
        String finalAuthorization;
        if (StringUtils.isNotEmpty(token)) {
            finalAuthorization = token;
        } else if (StringUtils.isNotEmpty(authorization)) {
            finalAuthorization = authorization;
        } else {
            return null;
        }
        return finalAuthorization.contains(AUTH2_TOKEN) ? finalAuthorization.split(" ")[1] : finalAuthorization;
    }

    private Mono<Void> checkAuthorization(final ServerWebExchange exchange,
                                          final ShenyuPluginChain chain,
                                          final String authorization,
                                          final String secretKey) {
        Object error = ShenyuResultWrap.error(ShenyuResultEnum.ERROR_TOKEN.getCode(), ShenyuResultEnum.ERROR_TOKEN.getMsg(), null);
        if (StringUtils.isEmpty(authorization)) {
            return WebFluxResultUtils.result(exchange, error);
        }
        JwtParser jwtParser = Jwts.parser();
        if (jwtParser.isSigned(authorization)) {
            jwtParser.setSigningKey(secretKey.getBytes(StandardCharsets.UTF_8));
            return ThrowingFunction.wrap(() -> jwtParser.parse(authorization)) == null ? WebFluxResultUtils.result(exchange, error) : chain.execute(exchange);
        }
        return WebFluxResultUtils.result(exchange, error);
    }
}
