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

import org.apache.shenyu.web.configuration.properties.ShenyuConfig;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.springframework.http.HttpStatus;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebFilterChain;
import reactor.core.publisher.Mono;

/**
 * this is visit time verify filter.
 */
public class TimeWebFilter extends AbstractWebFilter {

    private ShenyuConfig shenyuConfig;

    public TimeWebFilter(final ShenyuConfig shenyuConfig) {
        this.shenyuConfig = shenyuConfig;
    }

    @Override
    protected Mono<Boolean> doFilter(final ServerWebExchange exchange, final WebFilterChain chain) {
       /* final LocalDateTime start = requestDTO.getStartDateTime();
        final LocalDateTime now = LocalDateTime.now();
        final long between = DateUtils.acquireMinutesBetween(start, now);
        if (between < soulConfig.getFilterTime()) {
            return Mono.just(true);
        }*/
        return Mono.just(true);
    }

    @Override
    protected Mono<Void> doDenyResponse(final ServerWebExchange exchange) {
        ServerHttpResponse response = exchange.getResponse();
        response.setStatusCode(HttpStatus.REQUEST_TIMEOUT);
        Object error = ShenyuResultWrap.error(ShenyuResultEnum.TIME_ERROR.getCode(), ShenyuResultEnum.TIME_ERROR.getMsg(), null);
        return WebFluxResultUtils.result(exchange, error);
    }
}
