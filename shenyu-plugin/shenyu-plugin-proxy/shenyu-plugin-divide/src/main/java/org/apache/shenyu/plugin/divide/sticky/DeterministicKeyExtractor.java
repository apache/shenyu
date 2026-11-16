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

package org.apache.shenyu.plugin.divide.sticky;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.dto.convert.rule.sticky.DeterministicKeyConfig;
import org.springframework.web.server.ServerWebExchange;

import java.util.Objects;

/**
 * Extracts the deterministic hash key from a single, explicitly configured request
 * location — one header or one cookie — addressed by name.
 *
 * <p>Nothing else about the request is read or copied: the extraction returns at most
 * one value, and a missing or blank key yields null so the request falls back to the
 * load-balancing algorithm. Forwarding headers are not trusted: only the name the
 * operator configured is consulted (#8037).
 */
public class DeterministicKeyExtractor {

    /**
     * Extract the deterministic key for the request.
     *
     * @param exchange the current server exchange
     * @param config the key configuration
     * @return the key, or null when the configured location is missing or blank
     */
    public String extract(final ServerWebExchange exchange, final DeterministicKeyConfig config) {
        if (Objects.isNull(config) || !config.isConfigured()) {
            return null;
        }
        final String key;
        if (DeterministicKeyConfig.KeyType.COOKIE == config.getType()) {
            key = Objects.nonNull(exchange.getRequest().getCookies().getFirst(config.getName()))
                    ? exchange.getRequest().getCookies().getFirst(config.getName()).getValue() : null;
        } else {
            key = exchange.getRequest().getHeaders().getFirst(config.getName());
        }
        return StringUtils.isBlank(key) ? null : key;
    }
}
