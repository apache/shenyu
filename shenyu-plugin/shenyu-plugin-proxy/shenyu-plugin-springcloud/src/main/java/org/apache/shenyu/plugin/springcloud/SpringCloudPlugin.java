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

package org.apache.shenyu.plugin.springcloud;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.SpringCloudRuleHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.loadbalancer.factory.LoadBalancerFactory;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.apache.shenyu.plugin.api.utils.RequestUrlUtils;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.springcloud.handler.SpringCloudPluginDataHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.net.URI;
import java.util.List;
import java.util.Objects;

/**
 * this is springCloud proxy impl.
 */
public class SpringCloudPlugin extends AbstractShenyuPlugin {

    private static final Logger LOG = LoggerFactory.getLogger(SpringCloudPlugin.class);
    
    /**
     * Instantiates a new Spring cloud plugin.
     */
    public SpringCloudPlugin() {
    }
    
    @Override
    protected String getRawPath(final ServerWebExchange exchange) {
        return RequestUrlUtils.getRewrittenRawPath(exchange);
    }

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain,
                                   final SelectorData selector, final RuleData rule) {
        if (Objects.isNull(rule)) {
            return Mono.empty();
        }
        final ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        final SpringCloudRuleHandle ruleHandle = buildRuleHandle(rule);
        List<Upstream> upstreamList = UpstreamCacheManager.getInstance().findUpstreamListBySelectorId(selector.getId());
        if (CollectionUtils.isEmpty(upstreamList)) {
            Object error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.CANNOT_FIND_HEALTHY_UPSTREAM_URL);
            return WebFluxResultUtils.result(exchange, error);
        }
        String ip = Objects.requireNonNull(exchange.getRequest().getRemoteAddress()).getAddress().getHostAddress();
        Upstream upstream = LoadBalancerFactory.selector(upstreamList, ruleHandle.getLoadBalance(), ip);
        if (Objects.isNull(upstream)) {
            Object error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.SPRINGCLOUD_SERVICEID_IS_ERROR);
            return WebFluxResultUtils.result(exchange, error);
        }
        // set the http url
        if (CollectionUtils.isNotEmpty(exchange.getRequest().getHeaders().get(Constants.SPECIFY_DOMAIN))) {
            upstream.setUrl(exchange.getRequest().getHeaders().get(Constants.SPECIFY_DOMAIN).get(0));
        }
        // set domain
        final String domain = upstream.buildDomain();
        setDomain(URI.create(domain + shenyuContext.getRealUrl()), exchange);
        //set time out.
        exchange.getAttributes().put(Constants.HTTP_TIME_OUT, ruleHandle.getTimeout());
        return chain.execute(exchange);
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
    public boolean skip(final ServerWebExchange exchange) {
        return skipExcept(exchange, RpcTypeEnum.SPRING_CLOUD);
    }

    @Override
    protected Mono<Void> handleSelectorIfNull(final String pluginName, final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        return WebFluxResultUtils.noSelectorResult(pluginName, exchange);
    }

    @Override
    protected Mono<Void> handleRuleIfNull(final String pluginName, final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        return WebFluxResultUtils.noRuleResult(pluginName, exchange);
    }
    
    private SpringCloudRuleHandle buildRuleHandle(final RuleData rule) {
        return SpringCloudPluginDataHandler.RULE_CACHED.get().obtainHandle(CacheKeyUtils.INST.getKey(rule));
    }

    private void setDomain(final URI uri, final ServerWebExchange exchange) {
        String domain = uri.getScheme() + "://" + uri.getAuthority();
        exchange.getAttributes().put(Constants.HTTP_DOMAIN, domain);
    }
}
