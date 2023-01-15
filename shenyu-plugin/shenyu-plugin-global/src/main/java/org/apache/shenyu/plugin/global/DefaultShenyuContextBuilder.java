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

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.context.ShenyuContextBuilder;
import org.apache.shenyu.plugin.api.context.ShenyuContextDecorator;
import org.apache.shenyu.plugin.base.cache.MetaDataCache;
import org.springframework.http.HttpHeaders;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.web.server.ServerWebExchange;

import java.time.LocalDateTime;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

/**
 * The type Default Shenyu context builder.
 */
public class DefaultShenyuContextBuilder implements ShenyuContextBuilder {

    private static final String RPC_TYPE = "rpc_type";

    private static final String UPGRADE = "Upgrade";

    private final Map<String, ShenyuContextDecorator> decoratorMap;

    /**
     * Instantiates a new Default shenyu context builder.
     *
     * @param decoratorMap the decorator map
     */
    public DefaultShenyuContextBuilder(final Map<String, ShenyuContextDecorator> decoratorMap) {
        this.decoratorMap = decoratorMap;
    }

    @Override
    public ShenyuContext build(final ServerWebExchange exchange) {
        Pair<String, MetaData> buildData = buildData(exchange);
        return decoratorMap.get(buildData.getLeft()).decorator(buildDefaultContext(exchange.getRequest()), buildData.getRight());
    }
    
    private Pair<String, MetaData> buildData(final ServerWebExchange exchange) {
        ServerHttpRequest request = exchange.getRequest();
        HttpHeaders headers = request.getHeaders();
        String rpcType = headers.getFirst(RPC_TYPE);
        if (StringUtils.isNotEmpty(rpcType)) {
            return Pair.of(rpcType, new MetaData());
        }
        String upgrade = headers.getFirst(UPGRADE);
        if (StringUtils.isNotEmpty(upgrade) && RpcTypeEnum.WEB_SOCKET.getName().equals(upgrade)) {
            return Pair.of(RpcTypeEnum.WEB_SOCKET.getName(), new MetaData());
        }
        MetaData metaData = MetaDataCache.getInstance().obtain(request.getURI().getPath());
        if (Objects.nonNull(metaData) && Boolean.TRUE.equals(metaData.getEnabled())) {
            exchange.getAttributes().put(Constants.META_DATA, metaData);
            return Pair.of(metaData.getRpcType(), metaData);
        } else {
            return Pair.of(RpcTypeEnum.HTTP.getName(), new MetaData());
        }
    }

    private ShenyuContext buildDefaultContext(final ServerHttpRequest request) {
        ShenyuContext shenyuContext = new ShenyuContext();
        String path = request.getURI().getPath();
        shenyuContext.setPath(path);
        shenyuContext.setStartDateTime(LocalDateTime.now());
        Optional.ofNullable(request.getMethod()).ifPresent(httpMethod -> shenyuContext.setHttpMethod(httpMethod.name()));
        return shenyuContext;
    }
}
