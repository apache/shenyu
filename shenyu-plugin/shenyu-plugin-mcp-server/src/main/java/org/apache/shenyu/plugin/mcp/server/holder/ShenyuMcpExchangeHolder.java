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

package org.apache.shenyu.plugin.mcp.server.holder;

import org.springframework.web.server.ServerWebExchange;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public final class ShenyuMcpExchangeHolder {

    private static final Map<String, ServerWebExchange> EXCHANGE_MAP = new ConcurrentHashMap<>();

    private ShenyuMcpExchangeHolder() {
    }

    public static void put(final String sessionId, final ServerWebExchange exchange) {
        EXCHANGE_MAP.put(sessionId, exchange);
    }

    public static ServerWebExchange get(final String sessionId) {
        return EXCHANGE_MAP.get(sessionId);
    }

    public static void remove(final String sessionId) {
        EXCHANGE_MAP.remove(sessionId);
    }

    public static void clear() {
        EXCHANGE_MAP.clear();
    }
}
