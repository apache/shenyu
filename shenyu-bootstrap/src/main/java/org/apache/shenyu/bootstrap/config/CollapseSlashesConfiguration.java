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

package org.apache.shenyu.bootstrap.config;

import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebFilter;
import org.springframework.web.server.WebFilterChain;

import java.net.URI;

/**
 * Tolerant Path Slash.
 */
@Configuration
public class CollapseSlashesConfiguration {

    /**
     * declaration collapseSlashesFilter.
     * @return collapseSlashesFilter
     */
    @Bean
    @ConditionalOnProperty(
            value = "shenyu.switchConfig.collapseSlashes",
            havingValue = "true"
    )
    public WebFilter collapseSlashesFilter() {
        return (ServerWebExchange exchange, WebFilterChain chain) -> {
            ServerHttpRequest request = exchange.getRequest();
            String newPath = request.getURI().getRawPath().replaceAll("/{2,}", "/");
            if (!request.getURI().getRawPath().equals(newPath)) {
                URI newUri = request.getURI().resolve(newPath);
                ServerHttpRequest newRequest = request.mutate().uri(newUri).build();
                return chain.filter(exchange.mutate().request(newRequest).build());
            }
            return chain.filter(exchange);
        };
    }
}


