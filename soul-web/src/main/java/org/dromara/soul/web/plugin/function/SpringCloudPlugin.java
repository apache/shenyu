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

package org.dromara.soul.web.plugin.function;

import org.apache.commons.lang.StringUtils;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.dto.convert.rule.SpringCloudRuleHandle;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.enums.PluginTypeEnum;
import org.dromara.soul.common.enums.ResultEnum;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.common.utils.LogUtils;
import org.dromara.soul.web.cache.LocalCacheManager;
import org.dromara.soul.web.plugin.AbstractSoulPlugin;
import org.dromara.soul.web.plugin.SoulPluginChain;
import org.dromara.soul.web.plugin.hystrix.HttpCommand;
import org.dromara.soul.web.plugin.hystrix.HystrixBuilder;
import org.dromara.soul.web.request.RequestDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.cloud.client.ServiceInstance;
import org.springframework.cloud.client.loadbalancer.LoadBalancerClient;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.core.publisher.MonoSink;
import rx.Subscription;

import java.net.URI;
import java.util.Objects;

/**
 * this is springCloud proxy impl.
 *
 * @author xiaoyu(myth)
 */
public class SpringCloudPlugin extends AbstractSoulPlugin {

    private static final Logger LOGGER = LoggerFactory.getLogger(SpringCloudPlugin.class);

    private final LoadBalancerClient loadBalancer;

    /**
     * Instantiates a new Spring cloud plugin.
     *
     * @param localCacheManager the local cache manager
     * @param loadBalancer      the load balancer
     */
    public SpringCloudPlugin(final LocalCacheManager localCacheManager, final LoadBalancerClient loadBalancer) {
        super(localCacheManager);
        this.loadBalancer = loadBalancer;
    }

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final SoulPluginChain chain, final SelectorData selector, final RuleData rule) {
        if (Objects.isNull(rule)) {
            return Mono.empty();
        }

        final RequestDTO requestDTO = exchange.getAttribute(Constants.REQUESTDTO);

        assert requestDTO != null;

        final SpringCloudRuleHandle ruleHandle = GsonUtils.getInstance().fromJson(rule.getHandle(), SpringCloudRuleHandle.class);

        final String serviceId = selector.getHandle();

        if (StringUtils.isBlank(ruleHandle.getGroupKey())) {
            ruleHandle.setGroupKey(requestDTO.getModule());
        }

        if (StringUtils.isBlank(ruleHandle.getCommandKey())) {
            ruleHandle.setCommandKey(requestDTO.getMethod());
        }

        if (StringUtils.isBlank(serviceId) || StringUtils.isBlank(ruleHandle.getPath())) {
            LogUtils.error(LOGGER, () -> "can not configuration spring cloud handle....");
            return Mono.empty();
        }

        final ServiceInstance serviceInstance = loadBalancer.choose(serviceId);

        if (Objects.isNull(serviceInstance)) {
            LogUtils.error(LOGGER, () -> "eureka never register this serviceId " + serviceId);
            return Mono.empty();
        }

        final URI uri = loadBalancer.reconstructURI(serviceInstance, URI.create(ruleHandle.getPath()));

        HttpCommand command =
                new HttpCommand(HystrixBuilder.build(ruleHandle),
                        exchange, chain, requestDTO, uri.toString(), ruleHandle.getTimeout());

        return Mono.create((MonoSink<Object> s) -> {
            Subscription sub = command.toObservable().subscribe(s::success,
                    s::error, s::success);
            s.onCancel(sub::unsubscribe);
            if (command.isCircuitBreakerOpen()) {
                LogUtils.error(LOGGER, () -> ruleHandle.getGroupKey() + ":spring cloud execute circuitBreaker is Open !");
            }
        }).doOnError(throwable -> {
            throwable.printStackTrace();
            exchange.getAttributes().put(Constants.CLIENT_RESPONSE_RESULT_TYPE,
                    ResultEnum.ERROR.getName());
            chain.execute(exchange);
        }).then();
    }

    @Override
    public PluginTypeEnum pluginType() {
        return PluginTypeEnum.FUNCTION;
    }

    @Override
    public int getOrder() {
        return PluginEnum.SPRING_CLOUD.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.SPRING_CLOUD.getName();
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
        return !Objects.equals(Objects.requireNonNull(body).getRpcType(), RpcTypeEnum.SPRING_CLOUD.getName());
    }
}
