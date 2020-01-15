/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * Contributor license agreements.See the NOTICE file distributed with
 * This work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * he License.You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.dromara.soul.web.plugin.hystrix;

import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.dto.convert.HystrixHandle;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.enums.PluginTypeEnum;
import org.dromara.soul.common.enums.ResultEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.web.cache.LocalCacheManager;
import org.dromara.soul.web.plugin.AbstractSoulPlugin;
import org.dromara.soul.web.plugin.SoulPluginChain;
import org.dromara.soul.web.request.RequestDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import rx.Subscription;

import java.util.Objects;

/**
 * Hystrix Plugin.
 *
 * @author xiaoyu(Myth)
 */
public class HystrixPlugin extends AbstractSoulPlugin {

    /**
     * logger.
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(HystrixPlugin.class);

    /**
     * Instantiates a new Hystrix plugin.
     *
     * @param localCacheManager the local cache manager
     */
    public HystrixPlugin(final LocalCacheManager localCacheManager) {
        super(localCacheManager);
    }

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final SoulPluginChain chain, final SelectorData selector, final RuleData rule) {
        final RequestDTO requestDTO = exchange.getAttribute(Constants.REQUESTDTO);
        assert requestDTO != null;
        final HystrixHandle hystrixHandle = GsonUtils.getInstance().fromJson(rule.getHandle(), HystrixHandle.class);
        if (StringUtils.isBlank(hystrixHandle.getGroupKey())) {
            hystrixHandle.setGroupKey(Objects.requireNonNull(requestDTO).getModule());
        }
        if (StringUtils.isBlank(hystrixHandle.getCommandKey())) {
            hystrixHandle.setCommandKey(Objects.requireNonNull(requestDTO).getMethod());
        }

        HystrixCommand command = new HystrixCommand(HystrixBuilder.build(hystrixHandle), exchange, chain);

        return Mono.create(s -> {
            Subscription sub = command.toObservable().subscribe(s::success,
                    s::error, s::success);
            s.onCancel(sub::unsubscribe);
            if (command.isCircuitBreakerOpen()) {
                LOGGER.error("hystrix execute have circuitBreaker is Open! groupKey:{},commandKey:{}", hystrixHandle.getGroupKey(), hystrixHandle.getCommandKey());
            }
        }).doOnError(throwable -> {
            LOGGER.error("hystrix execute exception:", throwable);
            exchange.getAttributes().put(Constants.CLIENT_RESPONSE_RESULT_TYPE, ResultEnum.ERROR.getName());
            chain.execute(exchange);
        }).then();
    }

    @Override
    public String named() {
        return PluginEnum.HYSTRIX.getName();
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
        return PluginEnum.HYSTRIX.getCode();
    }

}
