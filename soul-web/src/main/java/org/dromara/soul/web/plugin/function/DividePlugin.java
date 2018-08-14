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

import com.netflix.hystrix.exception.HystrixRuntimeException;
import org.apache.commons.collections4.CollectionUtils;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.dto.convert.DivideHandle;
import org.dromara.soul.common.dto.convert.DivideUpstream;
import org.dromara.soul.common.dto.zk.RuleZkDTO;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.enums.PluginTypeEnum;
import org.dromara.soul.common.enums.ResultEnum;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.common.utils.GSONUtils;
import org.dromara.soul.common.utils.LogUtils;
import org.dromara.soul.web.balance.LoadBalance;
import org.dromara.soul.web.balance.factory.LoadBalanceFactory;
import org.dromara.soul.web.cache.ZookeeperCacheManager;
import org.dromara.soul.web.plugin.AbstractSoulPlugin;
import org.dromara.soul.web.plugin.SoulPluginChain;
import org.dromara.soul.web.plugin.hystrix.HttpCommand;
import org.dromara.soul.web.plugin.hystrix.HystrixBuilder;
import org.dromara.soul.web.request.RequestDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.core.publisher.MonoSink;
import rx.Subscription;

import java.util.List;
import java.util.Objects;
import java.util.function.Function;

/**
 * Divide Plugin.
 *
 * @author xiaoyu(Myth)
 */
@SuppressWarnings("all")
public class DividePlugin extends AbstractSoulPlugin {

    /**
     * logger.
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(DividePlugin.class);

    public DividePlugin(final ZookeeperCacheManager zookeeperCacheManager) {
        super(zookeeperCacheManager);
    }

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final SoulPluginChain chain, final RuleZkDTO rule) {

        final RequestDTO body = exchange.getAttribute(Constants.REQUESTDTO);

        final String handle = rule.getHandle();

        final DivideHandle divideHandle = GSONUtils.getInstance().fromJson(handle, DivideHandle.class);

        if (Objects.isNull(divideHandle)
                || CollectionUtils.isEmpty(divideHandle.getUpstreamList())) {
            LogUtils.error(LOGGER, "divide config error：{}", () -> handle);
            return chain.execute(exchange);
        }

        DivideUpstream divideUpstream;
        final List<DivideUpstream> upstreamList = divideHandle.getUpstreamList();
        if (upstreamList.size() == 1) {
            divideUpstream = upstreamList.get(0);
        } else {
            final LoadBalance loadBalance = LoadBalanceFactory.of(divideHandle.getLoadBalance());
            final String ip = exchange.getRequest().getRemoteAddress().getAddress().getHostAddress();
            divideUpstream = loadBalance.select(divideHandle.getUpstreamList(), ip);
        }
        if (Objects.isNull(divideUpstream)) {
            LogUtils.error(LOGGER, () -> "LoadBalance has error！");
            return chain.execute(exchange);
        }

        HttpCommand command = new HttpCommand(HystrixBuilder.build(divideHandle),
                divideUpstream, body, exchange, chain);
        return Mono.create((MonoSink<Object> s) -> {
            Subscription sub = command.toObservable().subscribe(s::success,
                    s::error, s::success);
            s.onCancel(sub::unsubscribe);
            if (command.isCircuitBreakerOpen()) {
                LogUtils.error(LOGGER, () -> divideHandle.getGroupKey() + "....http:circuitBreaker is Open.... !");
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
        return Objects.equals(Objects.requireNonNull(body).getRpcType(), RpcTypeEnum.DUBBO.getName());
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

}
