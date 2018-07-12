/*
 *
 *  * Licensed to the Apache Software Foundation (ASF) under one or more
 *  * contributor license agreements.  See the NOTICE file distributed with
 *  * this work for additional information regarding copyright ownership.
 *  * The ASF licenses this file to You under the Apache License, Version 2.0
 *  * (the "License"); you may not use this file except in compliance with
 *  * the License.  You may obtain a copy of the License at
 *  *
 *  *     http://www.apache.org/licenses/LICENSE-2.0
 *  *
 *  * Unless required by applicable law or agreed to in writing, software
 *  * distributed under the License is distributed on an "AS IS" BASIS,
 *  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  * See the License for the specific language governing permissions and
 *  * limitations under the License.
 *
 */

package org.dromara.soul.web.plugin.function;

import com.hqyg.skyway.api.convert.DubboHandle;
import com.hqyg.skyway.api.dto.zk.RuleZkDTO;
import com.hqyg.skyway.common.constant.Constants;
import com.hqyg.skyway.common.enums.PluginEnum;
import com.hqyg.skyway.common.enums.PluginTypeEnum;
import com.hqyg.skyway.common.enums.ResultEnum;
import com.hqyg.skyway.common.enums.RpcTypeEnum;
import com.hqyg.skyway.common.utils.GSONUtils;
import com.hqyg.skyway.common.utils.LogUtils;
import com.hqyg.skyway.core.request.RequestDTO;
import com.hqyg.skyway.web.cache.DataCacheManager;
import com.hqyg.skyway.web.plugin.AbstractSkywayPlugin;
import com.hqyg.skyway.web.plugin.SkywayPluginChain;
import com.hqyg.skyway.web.plugin.dubbo.DubboServiceProxy;
import com.hqyg.skyway.web.plugin.hystrix.DubboHystrixCommand;
import com.netflix.hystrix.HystrixCommandGroupKey;
import com.netflix.hystrix.HystrixCommandKey;
import com.netflix.hystrix.HystrixCommandProperties;
import com.netflix.hystrix.HystrixObservableCommand;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.core.publisher.MonoSink;
import rx.Subscription;

import java.util.Map;
import java.util.Objects;

/**
 * dubbo proxy.
 *
 * @author xiaoyu(Myth)
 */
public class DubboSkywayPlugin extends AbstractSkywayPlugin {

    /**
     * logger.
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(DubboSkywayPlugin.class);

    private final DubboServiceProxy dubboServiceProxy;

    public DubboSkywayPlugin(final DataCacheManager dataCacheManager, final DubboServiceProxy dubboServiceProxy) {
        super(dataCacheManager);
        this.dubboServiceProxy = dubboServiceProxy;
    }


    /**
     * this is Template Method child has Implement your own logic.
     *
     * @param exchange exchange the current server exchange {@linkplain ServerWebExchange}
     * @param chain    chain the current chain  {@linkplain ServerWebExchange}
     * @param rule     rule    {@linkplain RuleZkDTO}
     * @return {@code Mono<Void>} to indicate when request handling is complete
     */
    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final SkywayPluginChain chain, final RuleZkDTO rule) {

        final String handle = rule.getHandle();

        final DubboHandle dubboHandle = GSONUtils.getInstance().fromJson(handle, DubboHandle.class);

        if (!checkData(dubboHandle)) {
            return chain.execute(exchange);
        }

        buildDefaultHystrixAttr(dubboHandle);

        HystrixCommandGroupKey groupKey = HystrixCommandGroupKey.Factory.asKey(dubboHandle.getGroupKey());

        HystrixCommandKey commandKey = HystrixCommandKey.Factory.asKey(dubboHandle.getCommandKey());

        final Map<String, Object> paramMap = exchange.getAttribute(Constants.DUBBO_RPC_PARAMS);

        final HystrixCommandProperties.Setter propertiesSetter =
                HystrixCommandProperties.Setter()
                        // set time out
                        .withExecutionTimeoutInMilliseconds(dubboHandle.getTimeout())
                        .withCircuitBreakerEnabled(true)
                        // set IsolationStrategy.SEMAPHORE
                        .withExecutionIsolationStrategy(HystrixCommandProperties.ExecutionIsolationStrategy.SEMAPHORE)
                        .withExecutionIsolationSemaphoreMaxConcurrentRequests(dubboHandle.getMaxConcurrentRequests())
                        .withCircuitBreakerErrorThresholdPercentage(dubboHandle.getErrorThresholdPercentage())
                        .withCircuitBreakerRequestVolumeThreshold(dubboHandle.getRequestVolumeThreshold())
                        .withCircuitBreakerSleepWindowInMilliseconds(dubboHandle.getSleepWindowInMilliseconds());

        final HystrixObservableCommand.Setter setter =
                HystrixObservableCommand.Setter
                        .withGroupKey(groupKey)
                .andCommandKey(commandKey)
                .andCommandPropertiesDefaults(propertiesSetter);

        DubboHystrixCommand command = new DubboHystrixCommand(setter, paramMap, exchange, chain, dubboServiceProxy, dubboHandle);

        return Mono.create((MonoSink<Object> s) -> {
            Subscription sub = command.toObservable().subscribe(s::success,
                    s::error, s::success);
            s.onCancel(sub::unsubscribe);
            if (command.isCircuitBreakerOpen()) {
                LogUtils.error(LOGGER, () -> groupKey + ":dubbo circuitBreaker is Open !");
            }
        }).doOnError(throwable -> {
            throwable.printStackTrace();
            exchange.getAttributes().put(Constants.CLIENT_RESPONSE_RESULT_TYPE, ResultEnum.ERROR.getName());
            chain.execute(exchange);
        }).then();
    }

    /**
     * return plugin type.
     *
     * @return {@linkplain PluginTypeEnum}
     */
    @Override
    public PluginTypeEnum pluginType() {
        return PluginTypeEnum.FUNCTION;
    }

    /**
     * acquire plugin name.
     *
     * @return plugin name.
     */
    @Override
    public String named() {
        return PluginEnum.DUBBO.getName();
    }

    /**
     * plugin is execute.
     *
     * @param exchange the current server exchange
     * @return default false.
     */
    @Override
    public Boolean skip(final ServerWebExchange exchange) {
        final RequestDTO body = exchange.getAttribute(Constants.REQUESTDTO);
        return Objects.equals(body.getRpcType(), RpcTypeEnum.HTTP.getName());
    }

    @Override
    public int getOrder() {
        return PluginEnum.DUBBO.getCode();
    }

    private void buildDefaultHystrixAttr(final DubboHandle dubboHandle) {
        if (dubboHandle.getMaxConcurrentRequests() == 0) {
            dubboHandle.setMaxConcurrentRequests(Constants.MAX_CONCURRENT_REQUESTS);
        }
        if (dubboHandle.getErrorThresholdPercentage() == 0) {
            dubboHandle.setErrorThresholdPercentage(Constants.ERROR_THRESHOLD_PERCENTAGE);
        }
        if (dubboHandle.getRequestVolumeThreshold() == 0) {
            dubboHandle.setRequestVolumeThreshold(Constants.REQUEST_VOLUME_THRESHOLD);
        }
        if (dubboHandle.getSleepWindowInMilliseconds() == 0) {
            dubboHandle.setSleepWindowInMilliseconds(Constants.SLEEP_WINDOW_INMILLISECONDS);
        }
    }

    private boolean checkData(final DubboHandle dubboHandle) {
        if (StringUtils.isBlank(dubboHandle.getGroupKey())
                || StringUtils.isBlank(dubboHandle.getCommandKey())
                || StringUtils.isBlank(dubboHandle.getRegistry())
                || StringUtils.isBlank(dubboHandle.getAppName())) {
            LogUtils.error(LOGGER, () -> "dubbo handle require param not config!");
            return false;

        }
        return true;
    }
}
