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

package org.apache.shenyu.plugin.tars;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.concurrent.ShenyuThreadPoolExecutor;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.plugin.TarsRegisterConfig;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.ResultEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.tars.cache.ApplicationConfigCache;
import org.apache.shenyu.plugin.tars.proxy.TarsInvokePrxList;
import org.apache.shenyu.plugin.tars.util.PrxInfoUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.NoSuchBeanDefinitionException;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.lang.reflect.Method;
import java.util.Objects;
import java.util.Optional;
import java.util.Random;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.SynchronousQueue;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * The tars plugin.
 */
public class TarsPlugin extends AbstractShenyuPlugin {

    private static final Logger LOG = LoggerFactory.getLogger(TarsPlugin.class);

    private static final Random RANDOM = new Random();

    private final ThreadFactory factory = ShenyuThreadFactory.create("shenyu-tars", true);

    private ExecutorService threadPool;

    @Override
    @SuppressWarnings({"unchecked", "rawtypes"})
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
        String body = exchange.getAttribute(Constants.PARAM_TRANSFORM);
        ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        assert shenyuContext != null;
        MetaData metaData = exchange.getAttribute(Constants.META_DATA);
        if (!checkMetaData(metaData)) {
            assert metaData != null;
            LOG.error(" path is :{}, meta data have error.... {}", shenyuContext.getPath(), metaData);
            exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
            Object error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.META_DATA_ERROR);
            return WebFluxResultUtils.result(exchange, error);
        }
        if (StringUtils.isNoneBlank(metaData.getParameterTypes()) && StringUtils.isBlank(body)) {
            exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
            Object error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.TARS_HAVE_BODY_PARAM);
            return WebFluxResultUtils.result(exchange, error);
        }
        TarsInvokePrxList tarsInvokePrxList = ApplicationConfigCache.getInstance().get(metaData.getPath());
        int index = RANDOM.nextInt(tarsInvokePrxList.getTarsInvokePrxList().size());
        Object prx = tarsInvokePrxList.getTarsInvokePrxList().get(index).getInvokePrx();
        Method method = tarsInvokePrxList.getMethod();
        CompletableFuture future;

        initThreadPool();
        try {
            future = CompletableFuture.supplyAsync(() -> {
                try {
                    return method.invoke(prx, PrxInfoUtil.getParamArray(tarsInvokePrxList.getParamTypes(), tarsInvokePrxList.getParamNames(), body));
                } catch (Exception e) {
                    LOG.error("Invoke tars error", e);
                    exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
                    Object error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.TARS_INVOKE);
                    return WebFluxResultUtils.result(exchange, error);
                }
            }, threadPool);
        } catch (Exception e) {
            LOG.error("Invoke tars error", e);
            exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
            Object error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.TARS_INVOKE);
            return WebFluxResultUtils.result(exchange, error);
        }
        return Mono.fromFuture(future.thenApply(ret -> {
            if (Objects.isNull(ret)) {
                ret = Constants.TARS_RPC_RESULT_EMPTY;
            }
            exchange.getAttributes().put(Constants.RPC_RESULT, ret);
            exchange.getAttributes().put(Constants.CLIENT_RESPONSE_RESULT_TYPE, ResultEnum.SUCCESS.getName());
            return ret;
        })).onErrorMap(m -> new ShenyuException("failed to invoke tars")).then(chain.execute(exchange));
    }

    @Override
    public int getOrder() {
        return PluginEnum.TARS.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.TARS.getName();
    }

    @Override
    public boolean skip(final ServerWebExchange exchange) {
        return skipExcept(exchange, RpcTypeEnum.TARS);
    }

    private boolean checkMetaData(final MetaData metaData) {
        return Objects.nonNull(metaData) && !StringUtils.isBlank(metaData.getMethodName()) && !StringUtils.isBlank(metaData.getServiceName());
    }

    private void initThreadPool() {
        if (Objects.nonNull(threadPool)) {
            return;
        }
        TarsRegisterConfig config = Singleton.INST.get(TarsRegisterConfig.class);
        if (Objects.isNull(config)) {
            // should not execute to here
            threadPool = Executors.newCachedThreadPool(factory);
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
