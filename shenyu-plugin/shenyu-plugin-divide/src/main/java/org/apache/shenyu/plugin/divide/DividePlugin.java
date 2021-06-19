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

package org.apache.shenyu.plugin.divide;

import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.DivideUpstream;
import org.apache.shenyu.common.dto.convert.rule.impl.DivideRuleHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.divide.balance.utils.LoadBalanceUtils;
import org.apache.shenyu.plugin.divide.cache.UpstreamCacheManager;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Objects;

/**
 * Divide Plugin.
 */
@Slf4j
public class DividePlugin extends AbstractShenyuPlugin {

    @SneakyThrows
    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
        ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        assert shenyuContext != null;
        DivideRuleHandle ruleHandle = UpstreamCacheManager.getInstance().obtainHandle(CacheKeyUtils.INST.getKey(rule));
        long headerSize = 0;
        for (List<String> multiHeader : exchange.getRequest().getHeaders().values()) {
            for (String value : multiHeader) {
                headerSize += value.getBytes(StandardCharsets.UTF_8).length;
            }
        }
        if (headerSize > ruleHandle.getHeaderMaxSize()) {
            log.error("request header is too large");
            Object error = ShenyuResultWrap.error(ShenyuResultEnum.REQUEST_HEADER_TOO_LARGE.getCode(), ShenyuResultEnum.REQUEST_HEADER_TOO_LARGE.getMsg(), null);
            return WebFluxResultUtils.result(exchange, error);
        }
        if (exchange.getRequest().getHeaders().getContentLength() > ruleHandle.getRequestMaxSize()) {
            log.error("request entity is too large");
            Object error = ShenyuResultWrap.error(ShenyuResultEnum.REQUEST_ENTITY_TOO_LARGE.getCode(), ShenyuResultEnum.REQUEST_ENTITY_TOO_LARGE.getMsg(), null);
            return WebFluxResultUtils.result(exchange, error);
        }
        List<DivideUpstream> upstreamList = UpstreamCacheManager.getInstance().findUpstreamListBySelectorId(selector.getId());
        if (CollectionUtils.isEmpty(upstreamList)) {
            log.error("divide upstream configuration error： {}", rule);
            Object error = ShenyuResultWrap.error(ShenyuResultEnum.CANNOT_FIND_URL.getCode(), ShenyuResultEnum.CANNOT_FIND_URL.getMsg(), null);
            return WebFluxResultUtils.result(exchange, error);
        }
        String ip = Objects.requireNonNull(exchange.getRequest().getRemoteAddress()).getAddress().getHostAddress();
        DivideUpstream divideUpstream = LoadBalanceUtils.selector(upstreamList, ruleHandle.getLoadBalance(), ip);
        if (Objects.isNull(divideUpstream)) {
            log.error("divide has no upstream");
            Object error = ShenyuResultWrap.error(ShenyuResultEnum.CANNOT_FIND_URL.getCode(), ShenyuResultEnum.CANNOT_FIND_URL.getMsg(), null);
            return WebFluxResultUtils.result(exchange, error);
        }
        // set the http url
        String domain = buildDomain(divideUpstream);
        String realURL = buildRealURL(domain, shenyuContext, exchange);
        exchange.getAttributes().put(Constants.HTTP_URL, realURL);
        // set the http timeout
        exchange.getAttributes().put(Constants.HTTP_TIME_OUT, ruleHandle.getTimeout());
        exchange.getAttributes().put(Constants.HTTP_RETRY, ruleHandle.getRetry());
        return chain.execute(exchange);
    }

    @Override
    public String named() {
        return PluginEnum.DIVIDE.getName();
    }

    @Override
    public Boolean skip(final ServerWebExchange exchange) {
        final ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        return !Objects.equals(Objects.requireNonNull(shenyuContext).getRpcType(), RpcTypeEnum.HTTP.getName());
    }

    @Override
    public int getOrder() {
        return PluginEnum.DIVIDE.getCode();
    }

    @Override
    protected Mono<Void> handleSelectorIfNull(final String pluginName, final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        return WebFluxResultUtils.noSelectorResult(pluginName, exchange);
    }

    @Override
    protected Mono<Void> handleRuleIfNull(final String pluginName, final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        return WebFluxResultUtils.noRuleResult(pluginName, exchange);
    }

    private String buildDomain(final DivideUpstream divideUpstream) {
        String protocol = divideUpstream.getProtocol();
        if (StringUtils.isBlank(protocol)) {
            protocol = "http://";
        }
        return protocol + divideUpstream.getUpstreamUrl().trim();
    }

    private String buildRealURL(final String domain, final ShenyuContext shenyuContext, final ServerWebExchange exchange) {
        String path = domain;
        final String rewriteURI = (String) exchange.getAttributes().get(Constants.REWRITE_URI);
        if (StringUtils.isNoneBlank(rewriteURI)) {
            path = path + rewriteURI;
        } else {
            final String realUrl = shenyuContext.getRealUrl();
            if (StringUtils.isNoneBlank(realUrl)) {
                path = path + realUrl;
            }
        }
        String query = exchange.getRequest().getURI().getQuery();
        if (StringUtils.isNoneBlank(query)) {
            return path + "?" + query;
        }
        return path;
    }
}
