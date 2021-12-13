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

package org.apache.shenyu.bootstrap.filter;

import org.apache.shenyu.common.utils.JsonUtils;
import org.springframework.boot.actuate.health.Health;
import org.springframework.core.annotation.Order;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Component;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebFilter;
import org.springframework.web.server.WebFilterChain;
import reactor.core.publisher.Mono;

import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

/**
 * The type health filter.
 */
@Component
@Order(-99)
public final class HealthFilter implements WebFilter {

    private static final List<String> URL_PATTERNS = Arrays.asList("/actuator/health", "/health_check");

    @Override
    @NonNull
    public Mono<Void> filter(@NonNull final ServerWebExchange exchange, @NonNull final WebFilterChain chain) {
        String urlPath = exchange.getRequest().getURI().getPath();
        return URL_PATTERNS.contains(urlPath) ? writeHealthInfo(exchange) : Objects.requireNonNull(chain).filter(exchange);
    }

    private Mono<Void> writeHealthInfo(final ServerWebExchange exchange) {
        String result = JsonUtils.toJson(new Health.Builder().up().build());
        DataBuffer dataBuffer = exchange.getResponse().bufferFactory().wrap(result.getBytes(StandardCharsets.UTF_8));
        return exchange.getResponse().writeWith(Mono.just(dataBuffer));
    }
}
