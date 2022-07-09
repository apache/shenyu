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

import io.jsonwebtoken.Jwt;
import io.jsonwebtoken.JwtParser;
import io.jsonwebtoken.JwtParserBuilder;
import io.jsonwebtoken.Jwts;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.JwtRuleHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.jwt.config.JwtConfig;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.jwt.exception.ThrowingFunction;
import org.springframework.http.HttpHeaders;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

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
        Map<String, Object> jwtBody = checkAuthorization(finalAuthorization, jwtConfig.getSecretKey());
        if (jwtBody != null) {
            JwtRuleHandle ruleHandle = GsonUtils.getInstance().fromJson(rule.getHandle(), JwtRuleHandle.class);
            if (ruleHandle == null) {
                return chain.execute(exchange);
            }
            return chain.execute(converter(exchange, jwtBody, ruleHandle.getConverter()));
        }
        Object error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.ERROR_TOKEN);
        return WebFluxResultUtils.result(exchange, error);
    }

    @Override
    public String named() {
        return PluginEnum.JWT.getName();
    }

    @Override
    public int getOrder() {
        return PluginEnum.JWT.getCode();
    }

    /**
     * Both are compatible.
     * @param token header of token
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
     * check Authorization.
     * @param authorization the authorization after processing
     * @param secretKey secretKey of authorization
     * @return Map
     */
    private Map<String, Object> checkAuthorization(final String authorization,
                                                   final String secretKey) {

        if (StringUtils.isEmpty(authorization)) {
            return null;
        }
        JwtParserBuilder jwtParserBuilder = Jwts.parserBuilder();
        JwtParser jwtParser = jwtParserBuilder.build();
        if (jwtParser.isSigned(authorization)) {
            jwtParserBuilder.setSigningKey(secretKey.getBytes(StandardCharsets.UTF_8));
            JwtParser jwtParserExec = jwtParserBuilder.build();
            Jwt jwt = ThrowingFunction.wrap(() -> jwtParserExec.parse(authorization));
            if (jwt == null) {
                return null;
            }
            return (Map<String, Object>) jwt.getBody();
        }
        return null;
    }

    /**
     * The parameters in token are converted to request header.
     *
     * @param exchange exchange
     * @return ServerWebExchange exchange.
     */
    private ServerWebExchange converter(final ServerWebExchange exchange,
                                        final Map<String, Object> jwtBody,
                                        final List<JwtRuleHandle.Convert> converters) {
        ServerHttpRequest modifiedRequest = exchange.getRequest().mutate().headers(httpHeaders -> this.addHeader(httpHeaders, jwtBody, converters)).build();

        return exchange.mutate().request(modifiedRequest).build();
    }

    /**
     * add header.
     *
     * @param headers headers
     * @param body body
     * @param converters converters
     */
    private void addHeader(final HttpHeaders headers,
                           final Map<String, Object> body,
                           final List<JwtRuleHandle.Convert> converters) {
        for (JwtRuleHandle.Convert converter : converters) {
            if (converter.getJwtVal().contains(".")) {
                headers.add(converter.getHeaderVal(), parse(body, converter.getJwtVal().split("\\."), new AtomicInteger(0)));
            } else {
                headers.add(converter.getHeaderVal(), String.valueOf(body.get(converter.getJwtVal())));
            }
        }
    }

    /**
     * Parsing multi-level tokens.
     *
     * @param body token
     * @param split jwt of key
     * @param deep level default 0
     * @return token of val
     */
    private String parse(final Map<String, Object> body,
                        final String[] split,
                        final AtomicInteger deep) {
        for (Map.Entry<String, Object> entry : body.entrySet()) {

            if (deep.get() == split.length - 1) {
                return String.valueOf(body.get(split[deep.get()]));
            }

            if (entry.getKey().equals(split[deep.get()])) {
                if (entry.getValue() instanceof Map) {
                    deep.incrementAndGet();
                    return parse((Map<String, Object>) entry.getValue(), split, deep);
                }
            }
        }
        return String.valueOf(body.get(split[deep.get()]));
    }

}
