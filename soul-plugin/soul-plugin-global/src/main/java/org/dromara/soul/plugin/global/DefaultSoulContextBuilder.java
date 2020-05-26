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

package org.dromara.soul.plugin.global;

import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.dto.MetaData;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.plugin.api.context.SoulContext;
import org.dromara.soul.plugin.api.context.SoulContextBuilder;
import org.dromara.soul.plugin.global.cache.MetaDataCache;
import org.springframework.http.HttpMethod;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.web.server.ServerWebExchange;

import java.time.LocalDateTime;
import java.util.Objects;

public class DefaultSoulContextBuilder implements SoulContextBuilder {
    
    @Override
    public SoulContext build(ServerWebExchange exchange) {
        final ServerHttpRequest request = exchange.getRequest();
        String path = request.getURI().getPath();
        MetaData metaData = MetaDataCache.getInstance().obtain(path);
        if (Objects.nonNull(metaData)) {
            exchange.getAttributes().put(Constants.META_DATA, metaData);
        }
        return transform(request, metaData);
    }
    
    /**
     * ServerHttpRequest transform RequestDTO .
     *
     * @param request {@linkplain ServerHttpRequest}
     * @return RequestDTO request dto
     */
    private SoulContext transform(final ServerHttpRequest request, final MetaData metaData) {
        final String appKey = request.getHeaders().getFirst(Constants.APP_KEY);
        final String sign = request.getHeaders().getFirst(Constants.SIGN);
        final String timestamp = request.getHeaders().getFirst(Constants.TIMESTAMP);
        SoulContext soulContext = new SoulContext();
        String path = request.getURI().getPath();
        soulContext.setPath(path);
        if (Objects.nonNull(metaData)) {
            soulContext.setModule(metaData.getAppName());
            soulContext.setMethod(metaData.getServiceName());
            soulContext.setRpcType(metaData.getRpcType());
            soulContext.setPath(metaData.getPath());
            soulContext.setContextPath(metaData.getContextPath());
        } else {
            String[] splitList = StringUtils.split(path, "/");
            if (null != splitList && splitList.length > 0) {
                String contextPath = "/" + splitList[0];
                String realUrl = path.substring(contextPath.length());
                soulContext.setContextPath(contextPath);
                soulContext.setModule(contextPath);
                soulContext.setRealUrl(realUrl);
            } else {
                soulContext.setModule(path);
                soulContext.setMethod(path);
            }
            soulContext.setRpcType(RpcTypeEnum.HTTP.getName());
        }
        soulContext.setAppKey(appKey);
        soulContext.setSign(sign);
        soulContext.setTimestamp(timestamp);
        soulContext.setStartDateTime(LocalDateTime.now());
        HttpMethod method = request.getMethod();
        if (Objects.nonNull(method)) {
            soulContext.setHttpMethod(method.name());
        }
        return soulContext;
    }
}
