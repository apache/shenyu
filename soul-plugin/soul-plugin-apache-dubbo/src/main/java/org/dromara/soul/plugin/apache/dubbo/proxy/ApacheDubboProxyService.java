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

package org.dromara.soul.plugin.apache.dubbo.proxy;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.dubbo.common.Version;
import org.apache.dubbo.common.constants.CommonConstants;
import org.apache.dubbo.config.ReferenceConfig;
import org.apache.dubbo.rpc.Invoker;
import org.apache.dubbo.rpc.RpcContext;
import org.apache.dubbo.rpc.service.GenericException;
import org.apache.dubbo.rpc.service.GenericService;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.dto.MetaData;
import org.dromara.soul.common.enums.ResultEnum;
import org.dromara.soul.common.exception.SoulException;
import org.dromara.soul.common.utils.ParamCheckUtils;
import org.dromara.soul.common.utils.ReflectUtils;
import org.dromara.soul.plugin.apache.dubbo.cache.ApplicationConfigCache;
import org.dromara.soul.plugin.api.param.BodyParamResolveService;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Objects;
import java.util.concurrent.CompletableFuture;

/**
 * dubbo proxy service is  use GenericService.
 *
 * @author xiaoyu(Myth)
 */
@Slf4j
public class ApacheDubboProxyService {

    private final BodyParamResolveService bodyParamResolveService;

    /**
     * Instantiates a new Dubbo proxy service.
     *
     * @param bodyParamResolveService the generic param resolve service
     */
    public ApacheDubboProxyService(final BodyParamResolveService bodyParamResolveService) {
        this.bodyParamResolveService = bodyParamResolveService;
    }

    /**
     * Generic invoker object.
     *
     * @param body     the body
     * @param metaData the meta data
     * @param exchange the exchange
     * @return the object
     * @throws SoulException the soul exception
     */
    public Mono<Object> genericInvoker(final String body, final MetaData metaData, final ServerWebExchange exchange) throws SoulException {
        // issue(https://github.com/dromara/soul/issues/471), add dubbo tag route
        String dubboTagRouteFromHttpHeaders = exchange.getRequest().getHeaders().getFirst(Constants.DUBBO_TAG_ROUTE);
        if (StringUtils.isNotBlank(dubboTagRouteFromHttpHeaders)) {
            RpcContext.getContext().setAttachment(CommonConstants.TAG_KEY, dubboTagRouteFromHttpHeaders);
        }
        ReferenceConfig<GenericService> reference = ApplicationConfigCache.getInstance().get(metaData.getPath());
        if (Objects.isNull(reference) || StringUtils.isEmpty(reference.getInterface())) {
            ApplicationConfigCache.getInstance().invalidate(metaData.getPath());
            reference = ApplicationConfigCache.getInstance().initRef(metaData);
        }
        GenericService genericService = reference.get();
        Pair<String[], Object[]> pair;
        if (ParamCheckUtils.dubboBodyIsEmpty(body)) {
            pair = new ImmutablePair<>(new String[]{}, new Object[]{});
        } else {
            pair = bodyParamResolveService.buildParameter(body, metaData.getParameterTypes());
        }
        CompletableFuture<Object> future;
        if (isProviderSupportAsync(reference)) {
            future = genericService.$invokeAsync(metaData.getMethodName(), pair.getLeft(), pair.getRight());
        } else {
            Object data = genericService.$invoke(metaData.getMethodName(), pair.getLeft(), pair.getRight());
            if (data instanceof CompletableFuture) {
                future = (CompletableFuture<Object>) data;
            } else {
                future = CompletableFuture.completedFuture(data);
            }
        }
        return Mono.fromFuture(future.thenApply(ret -> {
            if (Objects.isNull(ret)) {
                ret = Constants.DUBBO_RPC_RESULT_EMPTY;
            }
            exchange.getAttributes().put(Constants.DUBBO_RPC_RESULT, ret);
            exchange.getAttributes().put(Constants.CLIENT_RESPONSE_RESULT_TYPE, ResultEnum.SUCCESS.getName());
            return ret;
        })).onErrorMap(exception -> exception instanceof GenericException ? new SoulException(((GenericException) exception).getExceptionMessage()) : new SoulException(exception));
    }

    private boolean isProviderSupportAsync(final ReferenceConfig<GenericService> reference) {
        boolean support = false;
        try {
            Invoker invoker = (Invoker) ReflectUtils.getFieldValue(reference, Constants.DUBBO_REFRENCE_INVOKER);
            int sdkVersion = Version.getIntVersion(invoker.getUrl().getParameter(Constants.DUBBO_PROVIDER_VERSION));
            //dubbo sdk only supports $invokeAsync after version 2.7.3
            if (sdkVersion >= Constants.DUBBO_SUPPORT_ASYNC_VERSION) {
                support = true;
            }
        } catch (Exception e) {
            if (log.isDebugEnabled()) {
                log.debug("check dubbo provider version ex:{}", e.getMessage());
            }
        }
        return support;
    }
}
