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
import org.apache.shenyu.web.filter.AbstractWebFilter;
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
    
    private final DispatcherHandler dispatcherHandler;
    
    private final Set<String> paths;
    
    /**
     * Instantiates a new Mcp server filter.
     *
     * @param dispatcherHandler the dispatcher handler
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
        return dispatcherHandler.handle(exchange);
    }
}
