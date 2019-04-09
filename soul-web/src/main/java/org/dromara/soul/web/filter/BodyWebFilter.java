/*
 * Copyright 2013-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.dromara.soul.web.filter;

import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.springframework.http.MediaType;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.web.reactive.function.server.ServerRequest;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebFilter;
import org.springframework.web.server.WebFilterChain;
import reactor.core.publisher.Mono;

import java.util.Map;

/**
 * The type Body web filter.
 *
 * @author xiaoyu(Myth)
 */
public class BodyWebFilter implements WebFilter {

    @Override
    public Mono<Void> filter(final ServerWebExchange exchange, final WebFilterChain chain) {
        ServerRequest serverRequest = new DefaultServerRequest(exchange);
        final ServerHttpRequest request = exchange.getRequest();
        final String rpcType = request.getHeaders().getFirst(Constants.RPC_TYPE);
        if (RpcTypeEnum.DUBBO.getName().equals(rpcType)) {
            MediaType mediaType = request.getHeaders().getContentType();
            return serverRequest.bodyToMono(String.class)
                    .flatMap(body -> {
                        if (MediaType.APPLICATION_JSON.isCompatibleWith(mediaType)) {
                            final Map<String, Object> paramMap = GsonUtils.getInstance().toObjectMap(body);
                            exchange.getAttributes().put(Constants.DUBBO_PARAMS, paramMap);
                        }
                        return chain.filter(exchange);
                    });
        }
        return chain.filter(exchange);
    }
}
