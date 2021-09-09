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

import org.apache.shenyu.common.config.ShenyuConfig.CrossFilterConfig;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.web.cors.reactive.CorsUtils;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebFilter;
import org.springframework.web.server.WebFilterChain;
import reactor.core.publisher.Mono;

/**
 * The type Cross filter.
 */
public class CrossFilter implements WebFilter {

    private final CrossFilterConfig filterConfig;

    public CrossFilter(final CrossFilterConfig filterConfig) {
        this.filterConfig = filterConfig;
    }

    @Override
    @SuppressWarnings("all")
    public Mono<Void> filter(final ServerWebExchange exchange, final WebFilterChain chain) {
        ServerHttpRequest request = exchange.getRequest();
        if (CorsUtils.isCorsRequest(request)) {
            ServerHttpResponse response = exchange.getResponse();
            HttpHeaders headers = response.getHeaders();
            headers.add("Access-Control-Allow-Origin", this.filterConfig.getAllowedOrigin());
            headers.add("Access-Control-Allow-Methods", this.filterConfig.getAllowedMethods());
            headers.add("Access-Control-Max-Age", this.filterConfig.getMaxAge());
            headers.add("Access-Control-Allow-Headers", this.filterConfig.getAllowedHeaders());
            headers.add("Access-Control-Expose-Headers", this.filterConfig.getAllowedExpose());
            headers.add("Access-Control-Allow-Credentials", String.valueOf(this.filterConfig.isAllowCredentials()));
            if (request.getMethod() == HttpMethod.OPTIONS) {
                response.setStatusCode(HttpStatus.OK);
                return Mono.empty();
            }
        }
        return chain.filter(exchange);
    }
}
