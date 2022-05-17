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

package org.apache.shenyu.plugin.api.utils;

import org.apache.commons.lang3.StringUtils;
import org.springframework.util.MultiValueMap;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.util.UriUtils;

import java.nio.charset.StandardCharsets;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * RequestQueryCodecUtil.
 */
public final class RequestQueryCodecUtil {

    private RequestQueryCodecUtil() {
    }

    /**
     * Gets codec query string.
     *
     * @param exchange the exchange
     * @return codec query string
     */
    public static String getCodecQuery(final ServerWebExchange exchange) {
        if (!exchange.getRequest().getURI().getRawQuery().contains("%")) {
            return exchange.getRequest().getURI().getQuery();
        }
        MultiValueMap<String, String> queryParams = exchange.getRequest().getQueryParams();
        return queryParams.keySet().stream()
                .map(key -> queryParams.get(key).stream()
                        .map(item -> Optional.ofNullable(item)
                                .map(value -> String.join("=",
                                        UriUtils.encode(key, StandardCharsets.UTF_8),
                                        UriUtils.encode(value, StandardCharsets.UTF_8)))
                                .orElse(UriUtils.encode(key, StandardCharsets.UTF_8)))
                        .filter(StringUtils::isNoneBlank)
                        .collect(Collectors.joining("&")))
                .collect(Collectors.joining("&")).trim();
    }
}
