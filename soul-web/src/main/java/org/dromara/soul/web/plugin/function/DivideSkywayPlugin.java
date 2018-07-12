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

import com.hqyg.skyway.api.convert.DivideHandle;
import com.hqyg.skyway.api.convert.DivideUpstream;
import com.hqyg.skyway.api.dto.zk.RuleZkDTO;
import com.hqyg.skyway.common.constant.Constants;
import com.hqyg.skyway.common.enums.PluginEnum;
import com.hqyg.skyway.common.enums.PluginTypeEnum;
import com.hqyg.skyway.common.enums.ResultEnum;
import com.hqyg.skyway.common.enums.RpcTypeEnum;
import com.hqyg.skyway.common.utils.GSONUtils;
import com.hqyg.skyway.common.utils.LogUtils;
import com.hqyg.skyway.core.balance.LoadBalance;
import com.hqyg.skyway.core.balance.factory.LoadBalanceFactory;
import com.hqyg.skyway.core.request.RequestDTO;
import com.hqyg.skyway.web.cache.DataCacheManager;
import com.hqyg.skyway.web.plugin.AbstractSkywayPlugin;
import com.hqyg.skyway.web.plugin.SkywayPluginChain;
import com.hqyg.skyway.web.plugin.hystrix.HttpHystrixCommand;
import com.netflix.hystrix.HystrixCommandGroupKey;
import com.netflix.hystrix.HystrixCommandKey;
import com.netflix.hystrix.HystrixCommandProperties;
import com.netflix.hystrix.HystrixObservableCommand;
import com.netflix.hystrix.exception.HystrixRuntimeException;
import org.apache.commons.collections4.CollectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.core.publisher.MonoSink;
import rx.Subscription;

import java.util.Objects;
import java.util.function.Function;

/**
 * Divide Plugin.
 * @author xiaoyu(Myth)
 */
@SuppressWarnings("unchecked")
public class DivideSkywayPlugin extends AbstractSkywayPlugin {

    /**
     * logger.
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(DivideSkywayPlugin.class);

    public DivideSkywayPlugin(final DataCacheManager dataCacheManager) {
        super(dataCacheManager);
    }

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final SkywayPluginChain chain, final RuleZkDTO rule) {

        final RequestDTO body = exchange.getAttribute(Constants.REQUESTDTO);

        final String handle = rule.getHandle();

        final DivideHandle divideHandle = GSONUtils.getInstance().fromJson(handle, DivideHandle.class);

        if (Objects.isNull(divideHandle)
                || CollectionUtils.isEmpty(divideHandle.getUpstreamList())) {
            LogUtils.error(LOGGER, "divide rule config error：{}", () -> handle);
            return chain.execute(exchange);
        }

        final LoadBalance loadBalance = LoadBalanceFactory.of(divideHandle.getLoadBalance());

        final DivideUpstream divideUpstream = loadBalance.select(divideHandle.getUpstreamList());

        if (Objects.isNull(divideUpstream)) {
            LogUtils.error(LOGGER, () -> "LoadBalance has error！");
            return chain.execute(exchange);
        }
        buildDefaultHystrixAttr(divideHandle);

        HystrixCommandGroupKey groupKey = HystrixCommandGroupKey.Factory.asKey(divideHandle.getGroupKey());
        HystrixCommandKey commandKey = HystrixCommandKey.Factory.asKey(divideHandle.getCommandKey());

        final HystrixCommandProperties.Setter propertiesSetter =
                HystrixCommandProperties.Setter()
                        // set time out
                        .withExecutionTimeoutInMilliseconds(Integer.parseInt(divideUpstream.getTimeout() + ""))
                        .withCircuitBreakerEnabled(true)
                        // set IsolationStrategy.SEMAPHORE
                        .withExecutionIsolationStrategy(HystrixCommandProperties.ExecutionIsolationStrategy.SEMAPHORE)
                        .withExecutionIsolationSemaphoreMaxConcurrentRequests(divideHandle.getMaxConcurrentRequests())
                        .withCircuitBreakerErrorThresholdPercentage(divideHandle.getErrorThresholdPercentage())
                        .withCircuitBreakerRequestVolumeThreshold(divideHandle.getRequestVolumeThreshold())
                        .withCircuitBreakerSleepWindowInMilliseconds(divideHandle.getSleepWindowInMilliseconds());

        final HystrixObservableCommand.Setter setter = HystrixObservableCommand.Setter.withGroupKey(groupKey)
                .andCommandKey(commandKey)
                .andCommandPropertiesDefaults(propertiesSetter);
        HttpHystrixCommand command = new HttpHystrixCommand(setter, divideUpstream, body, exchange, chain);
        return Mono.create((MonoSink<Object> s) -> {
            Subscription sub = command.toObservable().subscribe(s::success,
                    s::error, s::success);
            s.onCancel(sub::unsubscribe);
            if (command.isCircuitBreakerOpen()) {
                LogUtils.error(LOGGER, () -> groupKey + "....http:circuitBreaker is Open !");
            }
        }).onErrorResume((Function<Throwable, Mono<Void>>) throwable -> {
            if (throwable instanceof HystrixRuntimeException) {
                HystrixRuntimeException e = (HystrixRuntimeException) throwable;
                LOGGER.error(e.getCause().getMessage());
                if (e.getFailureType() == HystrixRuntimeException.FailureType.TIMEOUT) {
                    exchange.getAttributes().put(Constants.CLIENT_RESPONSE_RESULT_TYPE, ResultEnum.TIME_OUT.getName());
                    exchange.getResponse().setStatusCode(HttpStatus.GATEWAY_TIMEOUT);
                } else {
                    exchange.getAttributes().put(Constants.CLIENT_RESPONSE_RESULT_TYPE, ResultEnum.ERROR.getName());
                    exchange.getResponse().setStatusCode(HttpStatus.BAD_GATEWAY);
                }
                return chain.execute(exchange);
            }
            return Mono.empty();
        }).then();
    }

    @Override
    public String named() {
        return PluginEnum.DIVIDE.getName();
    }

    /**
     * plugin is execute.
     *
     * @return default false.
     */
    @Override
    public Boolean skip(final ServerWebExchange exchange) {
        final RequestDTO body = exchange.getAttribute(Constants.REQUESTDTO);
        return Objects.equals(body.getRpcType(), RpcTypeEnum.DUBBO.getName());
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

    @Override
    public int getOrder() {
        return PluginEnum.DIVIDE.getCode();
    }

    private void buildDefaultHystrixAttr(final DivideHandle divideHandle) {
        if (divideHandle.getMaxConcurrentRequests() == 0) {
            divideHandle.setMaxConcurrentRequests(Constants.MAX_CONCURRENT_REQUESTS);
        }
        if (divideHandle.getErrorThresholdPercentage() == 0) {
            divideHandle.setErrorThresholdPercentage(Constants.ERROR_THRESHOLD_PERCENTAGE);
        }
        if (divideHandle.getRequestVolumeThreshold() == 0) {
            divideHandle.setRequestVolumeThreshold(Constants.REQUEST_VOLUME_THRESHOLD);
        }
        if (divideHandle.getSleepWindowInMilliseconds() == 0) {
            divideHandle.setSleepWindowInMilliseconds(Constants.SLEEP_WINDOW_INMILLISECONDS);
        }
    }
}
