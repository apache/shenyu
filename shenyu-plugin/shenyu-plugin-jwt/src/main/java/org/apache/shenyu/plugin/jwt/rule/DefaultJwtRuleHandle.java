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

package org.apache.shenyu.plugin.jwt.rule;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.common.utils.GsonUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.web.server.ServerWebExchange;

import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Default Jwt Rule Handle.
 */
public class DefaultJwtRuleHandle implements JwtRuleHandle {

    /**
     * converter, Jwt's body content is assigned to the header.
     */
    private List<Convert> converter;

    @Override
    public String toString() {
        return "DefaultJwtRuleHandle{"
                + "converter=" + converter
                + '}';
    }

    @Override
    public void init(final String handleJson) {
        try {
            Map<String, List<Convert>> handleData = GsonUtils.getInstance()
                    .toObjectMapList(handleJson, Convert.class);
            converter = handleData.get("converter");
        } catch (Exception ignore) {
            //ignore wrong json format or alert client
        }
    }

    @Override
    public ServerWebExchange execute(final ServerWebExchange exchange, final Map<String, Object> jwtBody) {
        if (CollectionUtils.isEmpty(converter)) {
            return exchange;
        }
        return convert(exchange, jwtBody);
    }


    /**
     * The parameters in token are converted to request header.
     *
     * @param exchange exchange
     * @return ServerWebExchange exchange.
     */
    private ServerWebExchange convert(final ServerWebExchange exchange,
                                      final Map<String, Object> jwtBody) {
        ServerHttpRequest modifiedRequest = exchange.getRequest().mutate().headers(httpHeaders -> this.addHeader(httpHeaders, jwtBody, converter)).build();

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
                           final List<Convert> converters) {
        for (Convert converter : converters) {
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

    private static class Convert {

        /**
         * jwt of body name.
         */
        private String jwtVal;

        /**
         * header name.
         */
        private String headerVal;

        /**
         * get jwtVal.
         *
         * @return jwtVal
         */
        public String getJwtVal() {
            return jwtVal;
        }

        /**
         * set jwtVal.
         *
         * @param jwtVal jwtVal
         */
        public void setJwtVal(final String jwtVal) {
            this.jwtVal = jwtVal;
        }

        /**
         * get headerVal.
         *
         * @return headerVal
         */
        public String getHeaderVal() {
            return headerVal;
        }

        /**
         * set headerVal.
         *
         * @param headerVal headerVal
         */
        public void setHeaderVal(final String headerVal) {
            this.headerVal = headerVal;
        }

        @Override
        public String toString() {
            return "Convert{"
                    + "jwtVal='" + jwtVal + '\''
                    + ", headerVal='" + headerVal + '\''
                    + '}';
        }
    }

}
