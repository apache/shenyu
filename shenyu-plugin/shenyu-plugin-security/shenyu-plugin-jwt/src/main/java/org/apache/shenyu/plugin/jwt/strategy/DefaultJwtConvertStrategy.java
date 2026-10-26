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

package org.apache.shenyu.plugin.jwt.strategy;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.jwt.rule.DefaultJwtRuleHandle;
import org.apache.shenyu.plugin.jwt.rule.JwtRuleHandle;
import org.apache.shenyu.spi.Join;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.web.server.ServerWebExchange;

import java.util.List;
import java.util.Map;
import java.util.Objects;

@Join
public class DefaultJwtConvertStrategy implements JwtConvertStrategy {

    private static final Logger LOG = LoggerFactory.getLogger(DefaultJwtConvertStrategy.class);

    @Override
    public DefaultJwtRuleHandle parseHandleJson(final String handleJson) {
        try {
            return GsonUtils.getInstance().fromJson(handleJson, DefaultJwtRuleHandle.class);
        } catch (Exception exception) {
            LOG.error("Failed to parse json , please check json format", exception);
            return null;
        }
    }

    @Override
    public ServerWebExchange convert(final JwtRuleHandle jwtRuleHandle, final ServerWebExchange exchange, final Map<String, Object> jwtBody) {
        final DefaultJwtRuleHandle defaultJwtRuleHandle = (DefaultJwtRuleHandle) jwtRuleHandle;
        if (CollectionUtils.isEmpty(defaultJwtRuleHandle.getConverter())) {
            return exchange;
        }
        return convert(exchange, jwtBody, defaultJwtRuleHandle.getConverter());
    }

    /**
     * The parameters in token are converted to request header.
     *
     * @param exchange exchange
     * @return ServerWebExchange exchange.
     */
    private ServerWebExchange convert(final ServerWebExchange exchange,
                                      final Map<String, Object> jwtBody, final List<DefaultJwtRuleHandle.Convert> converters) {
        ServerHttpRequest modifiedRequest = exchange.getRequest().mutate().headers(httpHeaders -> this.addHeader(httpHeaders, jwtBody, converters)).build();
        return exchange.mutate().request(modifiedRequest).build();
    }

    /**
     * add header.
     *
     * @param headers    headers
     * @param body       body
     * @param converters converters
     */
    private void addHeader(final HttpHeaders headers,
                           final Map<String, Object> body,
                           final List<DefaultJwtRuleHandle.Convert> converters) {
        for (DefaultJwtRuleHandle.Convert converter : converters) {

            if (StringUtils.isEmpty(converter.getHeaderVal()) || StringUtils.isEmpty(converter.getJwtVal())) {
                continue;
            }

            final Object value = converter.getJwtVal().contains(".")
                    ? parse(body, converter.getJwtVal().split("\\."))
                    : body.get(converter.getJwtVal());
            // overwrite rather than append, so a client-supplied header with the same name can not spoof the claim
            if (Objects.isNull(value)) {
                headers.remove(converter.getHeaderVal());
            } else {
                headers.set(converter.getHeaderVal(), String.valueOf(value));
            }

        }
    }

    /**
     * Parsing multi-level tokens.
     *
     * @param body token
     * @param path jwt of key split by "."
     * @return token of val, or null if any node on the path is absent or not a map
     */
    private Object parse(final Map<String, Object> body, final String[] path) {
        Object current = body;
        for (String key : path) {
            if (!(current instanceof Map)) {
                return null;
            }
            current = ((Map<?, ?>) current).get(key);
        }
        return current;
    }
}
