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
import java.util.concurrent.atomic.AtomicInteger;

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

            if (converter.getJwtVal().contains(".")) {
                headers.add(converter.getHeaderVal(), parse(body, converter.getJwtVal().split("\\."), new AtomicInteger(0)));
            }
            headers.add(converter.getHeaderVal(), String.valueOf(body.get(converter.getJwtVal())));

        }
    }

    /**
     * Parsing multi-level tokens.
     *
     * @param body  token
     * @param split jwt of key
     * @param deep  level default 0
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
