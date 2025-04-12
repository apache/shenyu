package org.apache.shenyu.plugin.mcp.server;

import org.springframework.web.server.ServerWebExchange;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class ShenyuMcpExchangeHolder {

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
