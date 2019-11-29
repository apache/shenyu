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
import org.dromara.soul.common.dto.MetaData;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.web.cache.AbstractLocalCacheManager;
import org.dromara.soul.web.request.RequestDTO;
import org.dromara.soul.web.result.SoulResultEnum;
import org.dromara.soul.web.result.SoulResultUtils;
import org.dromara.soul.web.result.SoulResultWarp;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebFilterChain;
import reactor.core.publisher.Mono;

import java.time.LocalDateTime;
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
            String path = request.getURI().getPath();
            MetaData metaData = AbstractLocalCacheManager.findPath(path);
            if (Objects.isNull(metaData) || !metaData.getEnabled()) {
                return Mono.just(false);
            }
            RequestDTO requestDTO = transform(request, metaData);
            if (!verify(requestDTO)) {
                return Mono.just(false);
            }
            exchange.getAttributes().put(Constants.REQUESTDTO, requestDTO);
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

    /**
     * ServerHttpRequest transform RequestDTO .
     *
     * @param request {@linkplain ServerHttpRequest}
     * @return RequestDTO request dto
     */
    private RequestDTO transform(final ServerHttpRequest request, final MetaData metaData) {
        final String appKey = request.getHeaders().getFirst(Constants.APP_KEY);
        final String sign = request.getHeaders().getFirst(Constants.SIGN);
        final String timestamp = request.getHeaders().getFirst(Constants.TIMESTAMP);
        RequestDTO requestDTO = new RequestDTO();
        String path = request.getURI().getPath();
        String[] splitList = StringUtils.split(path, "/");
        String contextPath = "/" + splitList[0];
        String realPath = path.substring(contextPath.length());
        requestDTO.setPath(path);
        requestDTO.setRealUrl(realPath);
        requestDTO.setContextPath(contextPath);
        requestDTO.setModule(metaData.getAppName());
        requestDTO.setMethod(metaData.getServiceName());
        requestDTO.setAppKey(appKey);
        requestDTO.setRpcType(metaData.getRpcType());
        requestDTO.setSign(sign);
        requestDTO.setTimestamp(timestamp);
        requestDTO.setMetaData(metaData);
        requestDTO.setStartDateTime(LocalDateTime.now());
        HttpMethod method = request.getMethod();
        if (Objects.nonNull(method)) {
            requestDTO.setHttpMethod(method.name());
        }
        return requestDTO;
    }

    private Boolean verify(final RequestDTO requestDTO) {
        if (Objects.isNull(requestDTO)
                || StringUtils.isBlank(requestDTO.getModule())
                || StringUtils.isBlank(requestDTO.getMethod())) {
            return false;
        }
        final RpcTypeEnum rpcTypeEnum = RpcTypeEnum.acquireByName(requestDTO.getRpcType());
        return !Objects.isNull(rpcTypeEnum);
    }

}
