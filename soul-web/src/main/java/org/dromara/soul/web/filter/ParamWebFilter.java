/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.web.filter;

import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.web.result.SoulResultEnum;
import org.dromara.soul.web.result.SoulResultUtils;
import org.dromara.soul.web.result.SoulResultWarp;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebFilterChain;
import reactor.core.publisher.Mono;

/**
 * this is http post param verify filter.
 *
 * @author xiaoyu(Myth)
 */
public class ParamWebFilter extends AbstractWebFilter {

    private ParamService paramService;

    public ParamWebFilter(final ParamService paramService) {
        this.paramService = paramService;
    }

    @Override
    protected Mono<Boolean> doFilter(final ServerWebExchange exchange, final WebFilterChain chain) {
        final ServerHttpRequest request = exchange.getRequest();
        final HttpHeaders headers = request.getHeaders();
        final String upgrade = headers.getFirst("Upgrade");
        if (StringUtils.isBlank(upgrade) || !RpcTypeEnum.WEB_SOCKET.getName().equals(upgrade)) {
            return paramService.doParam(exchange);
        }
        return Mono.just(true);
    }

    @Override
    protected Mono<Void> doDenyResponse(final ServerWebExchange exchange) {
        ServerHttpResponse response = exchange.getResponse();
        response.setStatusCode(HttpStatus.BAD_REQUEST);
        Object error = SoulResultWarp.error(SoulResultEnum.PARAM_ERROR.getCode(), SoulResultEnum.PARAM_ERROR.getMsg(), null);
        return SoulResultUtils.result(exchange, error);
    }

}
