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

package org.apache.shenyu.web.filter;

import org.springframework.http.HttpStatus;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.util.AntPathMatcher;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebFilter;
import org.springframework.web.server.WebFilterChain;
import reactor.core.publisher.Mono;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * exclude url filter.
 */
public class ExcludeFilter implements WebFilter {

    private static final AntPathMatcher MATCHER = new AntPathMatcher();

    private List<String> paths;
    
    /**
     * Instantiates a new Exclude filter.
     *
     * @param paths the paths
     */
    public ExcludeFilter(final List<String> paths) {
        this.paths = paths;
    }

    @Override
    public Mono<Void> filter(final ServerWebExchange exchange, final WebFilterChain chain) {
        ServerHttpRequest request = exchange.getRequest();
        String path = request.getURI().getPath();
        Set<String> excludedPaths = Collections.unmodifiableSet(new HashSet<>(paths));
        boolean match = excludedPaths.stream().anyMatch(url -> reg(url, path));
        if (match) {
            ServerHttpResponse response = exchange.getResponse();
            response.setStatusCode(HttpStatus.OK);
            return Mono.empty();
        }
        return chain.filter(exchange);
    }

    private static boolean reg(final String pattern, final String path) {
        return MATCHER.match(pattern, path);
    }
}
