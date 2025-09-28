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

package org.apache.shenyu.plugin.sofa.proxy;

import com.alipay.hessian.generic.model.GenericObject;
import com.alipay.sofa.rpc.api.GenericService;
import com.alipay.sofa.rpc.config.ConsumerConfig;
import com.alipay.sofa.rpc.context.RpcInvokeContext;
import com.alipay.sofa.rpc.core.exception.SofaRpcException;
import com.alipay.sofa.rpc.core.invoke.SofaResponseCallback;
import com.alipay.sofa.rpc.core.request.RequestBase;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.selector.SofaUpstream;
import org.apache.shenyu.common.enums.ResultEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.ParamCheckUtils;
import org.apache.shenyu.plugin.sofa.cache.ApplicationConfigCache;
import org.apache.shenyu.plugin.sofa.param.SofaParamResolveService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Objects;
import java.util.concurrent.CompletableFuture;

/**
 * sofa proxy service is use GenericService.
 */
public class SofaProxyService {
    
    private static final Logger LOG = LoggerFactory.getLogger(SofaProxyService.class);
    
    private final SofaParamResolveService sofaParamResolveService;
    
    /**
     * Instantiates a new Sofa proxy service.
     *
     * @param sofaParamResolveService the sofa param resolve service
     */
    public SofaProxyService(final SofaParamResolveService sofaParamResolveService) {
        this.sofaParamResolveService = sofaParamResolveService;
    }
    
    /**
     * Generic invoker object.
     *
     * @param body          the body
     * @param metaData      the meta data
     * @param selectorData  the selector data
     * @param exchange      the webExchange
     * @return the object
     * @throws ShenyuException the shenyu exception
     */
    public Mono<Object> genericInvoker(final String body, final MetaData metaData, final SelectorData selectorData, final ServerWebExchange exchange) throws ShenyuException {
        ConsumerConfig<GenericService> reference = this.getConsumerConfig(selectorData, metaData, exchange);
        Pair<String[], Object[]> pair;
        if (StringUtils.isBlank(metaData.getParameterTypes()) || ParamCheckUtils.bodyIsEmpty(body)) {
            pair = new ImmutablePair<>(new String[]{}, new Object[]{});
        } else {
            pair = sofaParamResolveService.buildParameter(body, metaData.getParameterTypes());
        }
        CompletableFuture<Object> future = new CompletableFuture<>();
        RpcInvokeContext.getContext().setResponseCallback(new SofaResponseCallback<>() {
            @Override
            public void onAppResponse(final Object o, final String s, final RequestBase requestBase) {
                future.complete(o);
            }
            
            @Override
            public void onAppException(final Throwable throwable, final String s, final RequestBase requestBase) {
                future.completeExceptionally(throwable);
            }
            
            @Override
            public void onSofaException(final SofaRpcException e, final String s, final RequestBase requestBase) {
                future.completeExceptionally(e);
            }
        });
        GenericService genericService = reference.refer();
        genericService.$genericInvoke(metaData.getMethodName(), pair.getLeft(), pair.getRight());
        return Mono.fromFuture(future.thenApply(ret -> {
            Object result = ret;
            if (Objects.isNull(result)) {
                result = Constants.SOFA_RPC_RESULT_EMPTY;
            }
            
            GenericObject genericObject = (GenericObject) result;
            exchange.getAttributes().put(Constants.RPC_RESULT, genericObject.getFields());
            exchange.getAttributes().put(Constants.CLIENT_RESPONSE_RESULT_TYPE, ResultEnum.SUCCESS.getName());
            return result;
        })).onErrorMap(ShenyuException::new);
    }

    private ConsumerConfig<GenericService> getConsumerConfig(final SelectorData selectorData, final MetaData metaData, final ServerWebExchange exchange) {
        String referenceKey = metaData.getPath();
        SofaUpstream sofaUpstream = GsonUtils.getInstance().fromJson(selectorData.getHandle(), SofaUpstream.class);
        // if sofaUpstreams is empty, use default plugin config
        if (Objects.isNull(sofaUpstream)) {
            ConsumerConfig<GenericService> reference = ApplicationConfigCache.getInstance().get(referenceKey);
            if (StringUtils.isEmpty(reference.getInterfaceId())) {
                ApplicationConfigCache.getInstance().invalidate(referenceKey);
                reference = ApplicationConfigCache.getInstance().initRef(metaData);
            }
            return reference;
        }
        referenceKey = ApplicationConfigCache.getInstance().generateUpstreamCacheKey(selectorData.getId(), metaData.getPath(), sofaUpstream);
        ConsumerConfig<GenericService> reference = ApplicationConfigCache.getInstance().get(referenceKey);
        if (StringUtils.isEmpty(reference.getInterfaceId())) {
            ApplicationConfigCache.getInstance().invalidate(referenceKey);
            reference = ApplicationConfigCache.getInstance().initRef(selectorData.getId(), metaData, sofaUpstream);
            ApplicationConfigCache.getInstance().putUpstream(referenceKey, sofaUpstream);
        }
        return reference;
    }

}
