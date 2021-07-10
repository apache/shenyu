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

package org.apache.shenyu.plugin.grpc;

import io.grpc.CallOptions;
import io.grpc.MethodDescriptor;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.ResultEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.grpc.cache.GrpcClientCache;
import org.apache.shenyu.plugin.grpc.client.ShenyuGrpcClient;
import org.apache.shenyu.plugin.grpc.proto.ShenyuGrpcResponse;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;

/**
 * The type grpc plugin.
 */
@Slf4j
public class GrpcPlugin extends AbstractShenyuPlugin {

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
        String param = exchange.getAttribute(Constants.PARAM_TRANSFORM);
        ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        assert shenyuContext != null;
        MetaData metaData = exchange.getAttribute(Constants.META_DATA);
        if (!checkMetaData(metaData)) {
            assert metaData != null;
            log.error(" path is :{}, meta data have error.... {}", shenyuContext.getPath(), metaData);
            exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
            Object error = ShenyuResultWrap.error(ShenyuResultEnum.META_DATA_ERROR.getCode(), ShenyuResultEnum.META_DATA_ERROR.getMsg(), null);
            return WebFluxResultUtils.result(exchange, error);
        }
        if (StringUtils.isNoneBlank(metaData.getParameterTypes()) && StringUtils.isBlank(param)) {
            exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
            Object error = ShenyuResultWrap.error(ShenyuResultEnum.GRPC_HAVE_BODY_PARAM.getCode(), ShenyuResultEnum.GRPC_HAVE_BODY_PARAM.getMsg(), null);
            return WebFluxResultUtils.result(exchange, error);
        }
        final ShenyuGrpcClient client = GrpcClientCache.getGrpcClient(selector.getName());
        if (Objects.isNull(client)) {
            exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
            Object error = ShenyuResultWrap.error(ShenyuResultEnum.GRPC_CLIENT_NULL.getCode(), ShenyuResultEnum.GRPC_CLIENT_NULL.getMsg(), null);
            return WebFluxResultUtils.result(exchange, error);
        }
        GrpcExtInfo extInfo = GsonUtils.getGson().fromJson(metaData.getRpcExt(), GrpcExtInfo.class);
        CallOptions callOptions = CallOptions.DEFAULT.withDeadlineAfter(extInfo.timeout, TimeUnit.MILLISECONDS);
        CompletableFuture<ShenyuGrpcResponse> result = client.call(metaData, callOptions, param, extInfo.methodType);
        return Mono.fromFuture(result.thenApply(ret -> {
            exchange.getAttributes().put(Constants.RPC_RESULT, ret.getResults());
            exchange.getAttributes().put(Constants.CLIENT_RESPONSE_RESULT_TYPE, ResultEnum.SUCCESS.getName());
            return ret;
        })).onErrorMap(ShenyuException::new).then(chain.execute(exchange));
    }

    /**
     * acquire plugin name.
     *
     * @return plugin name.
     */
    @Override
    public String named() {
        return PluginEnum.GRPC.getName();
    }

    /**
     * plugin is execute.
     *
     * @param exchange the current server exchange
     * @return default false.
     */
    @Override
    public Boolean skip(final ServerWebExchange exchange) {
        final ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        assert shenyuContext != null;
        return !Objects.equals(shenyuContext.getRpcType(), RpcTypeEnum.GRPC.getName());
    }

    @Override
    public int getOrder() {
        return PluginEnum.GRPC.getCode();
    }

    private boolean checkMetaData(final MetaData metaData) {
        return null != metaData && !StringUtils.isBlank(metaData.getMethodName()) && !StringUtils.isBlank(metaData.getServiceName());
    }

    /**
     * The GrpcExt.
     */
    @Data
    static class GrpcExtInfo {

        private Integer timeout = 5000;

        private MethodDescriptor.MethodType methodType;

    }
}
