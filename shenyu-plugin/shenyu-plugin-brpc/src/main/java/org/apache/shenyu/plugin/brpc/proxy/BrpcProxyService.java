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

package org.apache.shenyu.plugin.brpc.proxy;

import com.baidu.cloud.starlight.api.rpc.config.ServiceConfig;
import com.baidu.cloud.starlight.core.rpc.generic.AsyncGenericService;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.concurrent.ShenyuThreadPoolExecutor;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.convert.plugin.BrpcRegisterConfig;
import org.apache.shenyu.common.enums.ResultEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.brpc.cache.ApplicationConfigCache;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.NoSuchBeanDefinitionException;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.SynchronousQueue;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * Brpc proxy service.
 */
public class BrpcProxyService {

    private static final Logger LOG = LoggerFactory.getLogger(BrpcProxyService.class);

    private final ThreadFactory factory = ShenyuThreadFactory.create("shenyu-brpc", true);

    private ExecutorService threadPool;

    /**
     * Generic invoker object.
     *
     * @param body     the body
     * @param metaData the meta data
     * @param exchange the exchange
     * @return the object
     * @throws ShenyuException the shenyu exception
     */
    @SuppressWarnings("all")
    public Mono<Object> genericInvoker(final String body, final MetaData metaData, final ServerWebExchange exchange) throws ShenyuException {
        ApplicationConfigCache.BrpcParamInfo brpcParamInfo = ApplicationConfigCache.PARAM_MAP.get(metaData.getMethodName());
        Object[] params;
        if (Objects.isNull(brpcParamInfo)) {
            params = new Object[0];
        } else {
            int num = brpcParamInfo.getParamTypes().length;
            params = new Object[num];
            Map<String, Object> bodyMap = GsonUtils.getInstance().convertToMap(body);
            for (int i = 0; i < num; i++) {
                params[i] = bodyMap.get(brpcParamInfo.getParamNames()[i]).toString();
            }
        }
        initThreadPool();
        CompletableFuture<Object> future = CompletableFuture.supplyAsync(() -> getValue(metaData, params), threadPool);
        return Mono.fromFuture(future.thenApply(ret -> {
            if (Objects.isNull(ret)) {
                ret = Constants.BRPC_RPC_RESULT_EMPTY;
            }
            exchange.getAttributes().put(Constants.RPC_RESULT, ret);
            exchange.getAttributes().put(Constants.CLIENT_RESPONSE_RESULT_TYPE, ResultEnum.SUCCESS.getName());
            return ret;
        })).onErrorMap(ShenyuException::new);
    }

    private Object getValue(final MetaData metaData, final Object[] params) {
        try {
            ServiceConfig serviceConfig = ApplicationConfigCache.getInstance().get(metaData.getPath());
            if (StringUtils.isBlank(serviceConfig.getServiceId())) {
                ApplicationConfigCache.getInstance().invalidate(metaData.getPath());
                serviceConfig = ApplicationConfigCache.getInstance().initRef(metaData);
            }
            AsyncGenericService service = ApplicationConfigCache.getInstance().buildService(serviceConfig, metaData);
            return service.$invokeFuture(metaData.getMethodName(), params).get();
        } catch (Exception e) {
            LOG.error("Exception caught in BrpcProxyService#genericInvoker.", e);
            return null;
        }
    }

    private void initThreadPool() {
        if (Objects.nonNull(threadPool)) {
            return;
        }
        BrpcRegisterConfig config = Singleton.INST.get(BrpcRegisterConfig.class);
        if (Objects.isNull(config)) {
            // should not execute to here
            threadPool = new ThreadPoolExecutor(0, Integer.MAX_VALUE,
                    60L, TimeUnit.SECONDS,
                    new SynchronousQueue<Runnable>(),
                    factory);
            return;
        }
        final String threadpool = Optional.ofNullable(config.getThreadpool()).orElse(Constants.CACHED);
        switch (threadpool) {
            case Constants.SHARED:
                try {
                    threadPool = SpringBeanUtils.getInstance().getBean(ShenyuThreadPoolExecutor.class);
                    return;
                } catch (NoSuchBeanDefinitionException t) {
                    throw new ShenyuException("shared thread pool is not enable, config ${shenyu.sharedPool.enable} in your xml/yml !", t);
                }
            case Constants.FIXED:
            case Constants.EAGER:
            case Constants.LIMITED:
                throw new UnsupportedOperationException();
            case Constants.CACHED:
            default:
                int corePoolSize = Optional.ofNullable(config.getCorethreads()).orElse(0);
                int maximumPoolSize = Optional.ofNullable(config.getThreads()).orElse(Integer.MAX_VALUE);
                int queueSize = Optional.ofNullable(config.getQueues()).orElse(0);
                threadPool = new ThreadPoolExecutor(corePoolSize, maximumPoolSize, 60L, TimeUnit.SECONDS,
                        queueSize > 0 ? new LinkedBlockingQueue<>(queueSize) : new SynchronousQueue<>(), factory);
        }
    }

}
