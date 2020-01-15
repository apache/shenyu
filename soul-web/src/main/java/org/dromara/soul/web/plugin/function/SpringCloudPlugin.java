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
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.web.cache.LocalCacheManager;
import org.dromara.soul.web.plugin.AbstractSoulPlugin;
import org.dromara.soul.web.plugin.SoulPluginChain;
import org.dromara.soul.web.request.RequestDTO;
import org.dromara.soul.web.result.SoulResultEnum;
import org.dromara.soul.web.result.SoulResultUtils;
import org.dromara.soul.web.result.SoulResultWarp;
import org.springframework.cloud.client.ServiceInstance;
import org.springframework.cloud.client.loadbalancer.LoadBalancerClient;
import org.springframework.http.HttpMethod;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.net.URI;
import java.util.Objects;

/**
 * this is springCloud proxy impl.
 *
 * @author xiaoyu(myth)
 */
public class SpringCloudPlugin extends AbstractSoulPlugin {

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
        if (StringUtils.isBlank(serviceId) || StringUtils.isBlank(ruleHandle.getPath())) {
            Object error = SoulResultWarp.error(SoulResultEnum.CANNOT_CONFIG_SPRINGCLOUD_SERVICEID.getCode(), SoulResultEnum.CANNOT_CONFIG_SPRINGCLOUD_SERVICEID.getMsg(), null);
            return SoulResultUtils.result(exchange, error);
        }

        final ServiceInstance serviceInstance = loadBalancer.choose(serviceId);
        if (Objects.isNull(serviceInstance)) {
            Object error = SoulResultWarp.error(SoulResultEnum.SPRINGCLOUD_SERVICEID_IS_ERROR.getCode(), SoulResultEnum.SPRINGCLOUD_SERVICEID_IS_ERROR.getMsg(), null);
            return SoulResultUtils.result(exchange, error);
        }
        final URI uri = loadBalancer.reconstructURI(serviceInstance, URI.create(requestDTO.getRealUrl()));

        String realURL = buildRealURL(uri.toASCIIString(), requestDTO.getHttpMethod(), exchange.getRequest().getURI().getQuery());

        exchange.getAttributes().put(Constants.HTTP_URL, realURL);
        //设置下超时时间
        exchange.getAttributes().put(Constants.HTTP_TIME_OUT, ruleHandle.getTimeout());
        return chain.execute(exchange);
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

    private String buildRealURL(final String url, final String httpMethod, final String query) {
        if (httpMethod.equals(HttpMethod.GET.name())) {
            if (StringUtils.isNotBlank(query)) {
                return url + "?" + query;
            }
        }
        return url;
    }
}
