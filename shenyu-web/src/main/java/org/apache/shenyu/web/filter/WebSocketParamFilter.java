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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.util.MultiValueMap;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebFilterChain;
import reactor.core.publisher.Mono;

/**
 * this is http post param verify filter.
 */
public class WebSocketParamFilter extends AbstractWebFilter {

    private static final String UPGRADE = "Upgrade";

    @Override
    protected Mono<Boolean> doFilter(final ServerWebExchange exchange, final WebFilterChain chain) {
        ServerHttpRequest request = exchange.getRequest();
        HttpHeaders headers = request.getHeaders();
        String upgrade = headers.getFirst(UPGRADE);
        if (StringUtils.isNoneBlank(upgrade) && RpcTypeEnum.WEB_SOCKET.getName().equals(upgrade)) {
            return Mono.just(verify(request.getQueryParams()));
        }
        return Mono.just(true);
    }

    @Override
    protected Mono<Void> doDenyResponse(final ServerWebExchange exchange) {
        ServerHttpResponse response = exchange.getResponse();
        response.setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
        Object error = ShenyuResultWrap.error(ShenyuResultEnum.PARAM_ERROR.getCode(), ShenyuResultEnum.PARAM_ERROR.getMsg(), null);
        return WebFluxResultUtils.result(exchange, error);
    }

    private Boolean verify(final MultiValueMap<String, String> queryParams) {
        return !StringUtils.isBlank(queryParams.getFirst(Constants.MODULE))
                && !StringUtils.isBlank(queryParams.getFirst(Constants.METHOD))
                && !StringUtils.isBlank(queryParams.getFirst(Constants.RPC_TYPE));
    }
}
