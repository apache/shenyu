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

package org.apache.shenyu.plugin.mcp.server;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.web.filter.AbstractWebFilter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.reactive.DispatcherHandler;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebFilterChain;
import reactor.core.publisher.Mono;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

/**
 * The type mcp server filter.
 */
public final class McpServerFilter extends AbstractWebFilter {
    
    private static final Logger LOG = LoggerFactory.getLogger(McpServerFilter.class);

    private final DispatcherHandler dispatcherHandler;

    private final Set<String> paths;

    /**
     * Instantiates a new Mcp server filter.
     *
     * @param dispatcherHandler  the dispatcher handler
     * @param sseMessageEndpoint the sseMessageEndpoint
     */
    public McpServerFilter(final DispatcherHandler dispatcherHandler, final String sseMessageEndpoint) {
        this.dispatcherHandler = dispatcherHandler;
        if (StringUtils.isEmpty(sseMessageEndpoint)) {
            this.paths = new HashSet<>(Arrays.asList("/sse"));
        } else {
            this.paths = new HashSet<>(Arrays.asList("/sse", sseMessageEndpoint));
        }
    }

    @Override
    protected Mono<Boolean> doMatcher(final ServerWebExchange exchange, final WebFilterChain chain) {
        return Mono.just(paths.stream().anyMatch(path -> exchange.getRequest().getURI().getRawPath().startsWith(path)));
    }

    @Override
    protected Mono<Void> doFilter(final ServerWebExchange exchange) {
        LOG.debug("mcp server filter, path: {}", exchange.getRequest().getURI().getPath());
        LOG.debug("mcp server filter, headers: {}", GsonUtils.getGson().toJson(exchange.getRequest().getHeaders().toSingleValueMap()));
        if (exchange.getRequest().getQueryParams().containsKey("sessionId")) {
            String sessionId = exchange.getRequest().getQueryParams().getFirst("sessionId");
            if (StringUtils.isNotEmpty(sessionId)) {
                ShenyuMcpExchangeHolder.put(sessionId, exchange);
                return dispatcherHandler.handle(exchange).doFinally(signalType -> {
                    if (exchange.getRequest().getQueryParams().containsKey("sessionId")) {
                        if (StringUtils.isNotEmpty(sessionId)) {
                            ShenyuMcpExchangeHolder.remove(sessionId);
                        }
                    }
                });
            }
        }
        return dispatcherHandler.handle(exchange);
    }
}
