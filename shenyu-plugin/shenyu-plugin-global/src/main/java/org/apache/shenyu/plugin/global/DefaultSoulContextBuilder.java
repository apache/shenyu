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

package org.apache.shenyu.plugin.global;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.api.context.SoulContext;
import org.apache.shenyu.plugin.api.context.SoulContextBuilder;
import org.apache.shenyu.plugin.api.context.SoulContextDecorator;
import org.apache.shenyu.plugin.global.cache.MetaDataCache;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.web.server.ServerWebExchange;

import java.time.LocalDateTime;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

/**
 * The type Default soul context builder.
 *
 * @author xiaoyu
 */
public class DefaultSoulContextBuilder implements SoulContextBuilder {
    
    private final Map<String, SoulContextDecorator> decoratorMap;
    
    /**
     * Instantiates a new Default soul context builder.
     *
     * @param decoratorMap the decorator map
     */
    public DefaultSoulContextBuilder(final Map<String, SoulContextDecorator> decoratorMap) {
        this.decoratorMap = decoratorMap;
    }

    @Override
    public SoulContext build(final ServerWebExchange exchange) {
        ServerHttpRequest request = exchange.getRequest();
        String path = request.getURI().getPath();
        MetaData metaData = MetaDataCache.getInstance().obtain(path);
        if (Objects.nonNull(metaData) && metaData.getEnabled()) {
            exchange.getAttributes().put(Constants.META_DATA, metaData);
        }
        return Optional.ofNullable(metaData).map(e -> decoratorMap.get(e.getRpcType()))
                .orElse(decoratorMap.get(RpcTypeEnum.HTTP.getName()))
                .decorator(buildDefault(request), metaData);
    }
    
    private SoulContext buildDefault(final ServerHttpRequest request) {
        String appKey = request.getHeaders().getFirst(Constants.APP_KEY);
        String sign = request.getHeaders().getFirst(Constants.SIGN);
        String timestamp = request.getHeaders().getFirst(Constants.TIMESTAMP);
        SoulContext soulContext = new SoulContext();
        String path = request.getURI().getPath();
        soulContext.setPath(path);
        soulContext.setAppKey(appKey);
        soulContext.setSign(sign);
        soulContext.setTimestamp(timestamp);
        soulContext.setStartDateTime(LocalDateTime.now());
        Optional.ofNullable(request.getMethod()).ifPresent(httpMethod -> soulContext.setHttpMethod(httpMethod.name()));
        return soulContext;
    }
}
