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

import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.lang.NonNull;
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
 * FallbackFilter.
 */
public class FallbackFilter implements WebFilter {

    /**
     * logger.
     */
    private static final Logger LOG = LoggerFactory.getLogger(FallbackFilter.class);

    private static final AntPathMatcher MATCHER = new AntPathMatcher();

    private static final String HYSTRIX = "/fallback/hystrix";

    private static final String RESILIENCE4J = "/fallback/resilience4j";

    private final Set<String> paths;

    public FallbackFilter(final List<String> paths) {
        if (CollectionUtils.isNotEmpty(paths)) {
            this.paths = new HashSet<>(paths);
        } else {
            this.paths = new HashSet<>();
        }
        this.paths.add(HYSTRIX);
        this.paths.add(RESILIENCE4J);
    }

    @Override
    @NonNull
    public Mono<Void> filter(@NonNull final ServerWebExchange exchange, @NonNull final WebFilterChain chain) {
        ServerHttpRequest request = exchange.getRequest();
        String path = request.getURI().getPath();
        Set<String> fallbackPaths = Collections.unmodifiableSet(this.paths);
        boolean match = fallbackPaths.stream().anyMatch(url -> reg(url, path));
        if (match) {
            Object error = this.getError(exchange, path);
            return WebFluxResultUtils.result(exchange, error);
        }
        return chain.filter(exchange);
    }

    private static boolean reg(final String pattern, final String path) {
        return MATCHER.match(pattern, path);
    }

    private Object getError(final ServerWebExchange exchange, final String path) {
        if (HYSTRIX.equals(path)) {
            LOG.error("the fallback for hystrix");
            return ShenyuResultWrap.error(exchange, ShenyuResultEnum.HYSTRIX_PLUGIN_FALLBACK, null);
        }
        if (RESILIENCE4J.equals(path)) {
            LOG.error("the fallback for resilience4j");
            return ShenyuResultWrap.error(exchange, ShenyuResultEnum.RESILIENCE4J_PLUGIN_FALLBACK, null);
        }
        return ShenyuResultWrap.error(exchange, ShenyuResultEnum.DEFAULT_FALLBACK, null);
    }
}
