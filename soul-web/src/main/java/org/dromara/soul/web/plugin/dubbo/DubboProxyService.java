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

package org.dromara.soul.web.plugin.dubbo;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.dubbo.config.ReferenceConfig;
import org.apache.dubbo.rpc.service.GenericException;
import org.apache.dubbo.rpc.service.GenericService;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.dto.MetaData;
import org.dromara.soul.common.enums.ResultEnum;
import org.dromara.soul.common.exception.SoulException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Objects;
import java.util.concurrent.CompletableFuture;

/**
 * dubbo proxy service is  use GenericService.
 *
 * @author xiaoyu(Myth)
 */
public class DubboProxyService {

    /**
     * logger.
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(DubboProxyService.class);

    private final GenericParamResolveService genericParamResolveService;

    /**
     * Instantiates a new Dubbo proxy service.
     *
     * @param genericParamResolveService the generic param resolve service
     */
    public DubboProxyService(final GenericParamResolveService genericParamResolveService) {
        this.genericParamResolveService = genericParamResolveService;
    }

    /**
     * Generic invoker object.
     *
     * @param body            the body
     * @param metaData        the meta data
     * @return the object
     * @throws SoulException the soul exception
     */
    public Mono<Object> genericInvoker(final String body, final MetaData metaData, ServerWebExchange exchange) throws SoulException {
        ReferenceConfig<GenericService> reference;
        GenericService genericService;
        try {
            reference = ApplicationConfigCache.getInstance().get(metaData.getServiceName());
            if (Objects.isNull(reference) || StringUtils.isEmpty(reference.getInterface())) {
                ApplicationConfigCache.getInstance().invalidate(metaData.getServiceName());
                reference = ApplicationConfigCache.getInstance().initRef(metaData);
            }
            genericService = reference.get();
        } catch (Exception ex) {
            LOGGER.error("dubbo 泛化初始化异常:", ex);
            ApplicationConfigCache.getInstance().invalidate(metaData.getServiceName());
            reference = ApplicationConfigCache.getInstance().initRef(metaData);
            genericService = reference.get();
        }
        Pair<String[], Object[]> pair;
        try {
            if ("".equals(body) || "{}".equals(body) || "null".equals(body)) {
                pair = new ImmutablePair<>(new String[]{}, new Object[]{});
            } else {
                pair = genericParamResolveService.buildParameter(body, metaData.getParameterTypes());
            }
            CompletableFuture<Object> future= genericService.$invokeAsync(metaData.getMethodName(), pair.getLeft(), pair.getRight());
            return  Mono.fromFuture(future.thenApply(ret -> {
                if (Objects.nonNull(ret)) {
                    exchange.getAttributes().put(Constants.DUBBO_RPC_RESULT,ret);
                } else {
                    exchange.getAttributes().put(Constants.DUBBO_RPC_RESULT, Constants.DUBBO_RPC_RESULT_EMPTY);
                }
                exchange.getAttributes().put(Constants.CLIENT_RESPONSE_RESULT_TYPE, ResultEnum.SUCCESS.getName());
                return ret;
            }));
        } catch (GenericException e) {
            LOGGER.error("dubbo 泛化调用异常", e);
            throw new SoulException(e.getMessage());
        }
    }

}
