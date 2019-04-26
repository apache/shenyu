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
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.enums.HttpMethodEnum;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.common.result.SoulResult;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.web.request.RequestDTO;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebFilterChain;
import reactor.core.publisher.Mono;

import java.util.Objects;

/**
 * this is http post param verify filter.
 *
 * @author xiaoyu(Myth)
 */
public class ParamWebFilter extends AbstractWebFilter {

    @Override
    protected Mono<Boolean> doFilter(final ServerWebExchange exchange, final WebFilterChain chain) {
        final ServerHttpRequest request = exchange.getRequest();
        final HttpHeaders headers = request.getHeaders();
        final String upgrade = headers.getFirst("Upgrade");
        if (StringUtils.isBlank(upgrade) || !RpcTypeEnum.WEB_SOCKET.getName().equals(upgrade)) {
            final RequestDTO requestDTO = RequestDTO.transform(request);
            if (verify(requestDTO, exchange)) {
                exchange.getAttributes().put(Constants.REQUESTDTO, requestDTO);
            } else {
                return Mono.just(false);
            }
        }
        return Mono.just(true);
    }

    @Override
    protected Mono<Void> doDenyResponse(final ServerWebExchange exchange) {
        ServerHttpResponse response = exchange.getResponse();
        response.setStatusCode(HttpStatus.BAD_REQUEST);
        final SoulResult result = SoulResult.error("you param is error please check with doc!");
        return response.writeWith(Mono.just(response.bufferFactory()
                .wrap(GsonUtils.getInstance().toJson(result).getBytes())));
    }

    private Boolean verify(final RequestDTO requestDTO, final ServerWebExchange exchange) {
        if (Objects.isNull(requestDTO)
                || StringUtils.isBlank(requestDTO.getModule())
                || StringUtils.isBlank(requestDTO.getMethod())) {
            return false;
        }
        final RpcTypeEnum rpcTypeEnum = RpcTypeEnum.acquireByName(requestDTO.getRpcType());
        if (Objects.isNull(rpcTypeEnum)) {
            return false;
        }
        //if rpcType is dubbo
        if (Objects.equals(rpcTypeEnum.getName(), RpcTypeEnum.DUBBO.getName())) {
            final Object param = exchange.getAttributes().get(Constants.DUBBO_PARAMS);
            return !Objects.isNull(param);
        } else {
            return !StringUtils.isBlank(requestDTO.getHttpMethod())
                    && !Objects.isNull(HttpMethodEnum.acquireByName(requestDTO.getHttpMethod()));
        }
    }

}
