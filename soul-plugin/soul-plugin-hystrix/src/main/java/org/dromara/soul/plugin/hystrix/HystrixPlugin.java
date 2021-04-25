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

package org.dromara.soul.plugin.hystrix;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.dto.convert.HystrixHandle;
import org.dromara.soul.common.enums.HystrixIsolationModeEnum;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.enums.ResultEnum;
import org.dromara.soul.plugin.api.SoulPluginChain;
import org.dromara.soul.plugin.api.context.SoulContext;
import org.dromara.soul.plugin.base.AbstractSoulPlugin;
import org.dromara.soul.plugin.hystrix.builder.HystrixBuilder;
import org.dromara.soul.plugin.hystrix.cache.HystrixRuleHandleCache;
import org.dromara.soul.plugin.hystrix.command.Command;
import org.dromara.soul.plugin.hystrix.command.HystrixCommand;
import org.dromara.soul.plugin.hystrix.command.HystrixCommandOnThread;
import org.dromara.soul.plugin.hystrix.handler.HystrixPluginDataHandler;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import rx.Subscription;

import java.util.Objects;

/**
 * Hystrix Plugin.
 *
 * @author xiaoyu(Myth)
 */
@Slf4j
public class HystrixPlugin extends AbstractSoulPlugin {

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final SoulPluginChain chain, final SelectorData selector, final RuleData rule) {
        final SoulContext soulContext = exchange.getAttribute(Constants.CONTEXT);
        assert soulContext != null;
        final HystrixHandle hystrixHandle = HystrixRuleHandleCache.getInstance()
                .obtainHandle(HystrixPluginDataHandler.getCacheKeyName(rule));
        if (StringUtils.isBlank(hystrixHandle.getGroupKey())) {
            hystrixHandle.setGroupKey(Objects.requireNonNull(soulContext).getModule());
        }
        if (StringUtils.isBlank(hystrixHandle.getCommandKey())) {
            hystrixHandle.setCommandKey(Objects.requireNonNull(soulContext).getMethod());
        }
        Command command = fetchCommand(hystrixHandle, exchange, chain);
        return Mono.create(s -> {
            Subscription sub = command.fetchObservable().subscribe(s::success,
                    s::error, s::success);
            s.onCancel(sub::unsubscribe);
            if (command.isCircuitBreakerOpen()) {
                log.error("hystrix execute have circuitBreaker is Open! groupKey:{},commandKey:{}", hystrixHandle.getGroupKey(), hystrixHandle.getCommandKey());
            }
        }).doOnError(throwable -> {
            log.error("hystrix execute exception:", throwable);
            exchange.getAttributes().put(Constants.CLIENT_RESPONSE_RESULT_TYPE, ResultEnum.ERROR.getName());
            chain.execute(exchange);
        }).then();
    }

    private Command fetchCommand(final HystrixHandle hystrixHandle, final ServerWebExchange exchange, final SoulPluginChain chain) {
        if (hystrixHandle.getExecutionIsolationStrategy() == HystrixIsolationModeEnum.SEMAPHORE.getCode()) {
            return new HystrixCommand(HystrixBuilder.build(hystrixHandle),
                    exchange, chain, hystrixHandle.getCallBackUri());
        }
        return new HystrixCommandOnThread(HystrixBuilder.buildForHystrixCommand(hystrixHandle),
                exchange, chain, hystrixHandle.getCallBackUri());
    }

    @Override
    public String named() {
        return PluginEnum.HYSTRIX.getName();
    }

    @Override
    public int getOrder() {
        return PluginEnum.HYSTRIX.getCode();
    }

}
