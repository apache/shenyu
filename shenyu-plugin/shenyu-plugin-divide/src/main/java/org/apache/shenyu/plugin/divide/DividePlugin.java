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
import org.apache.shenyu.plugin.divide.cache.UpstreamCacheManager;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.DivideUpstream;
import org.apache.shenyu.common.dto.convert.rule.impl.DivideRuleHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.api.SoulPluginChain;
import org.apache.shenyu.plugin.api.context.SoulContext;
import org.apache.shenyu.plugin.api.result.SoulResultEnum;
import org.apache.shenyu.plugin.base.AbstractSoulPlugin;
import org.apache.shenyu.plugin.base.utils.FallbackUtils;
import org.apache.shenyu.plugin.api.result.SoulResultWrap;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.divide.balance.utils.LoadBalanceUtils;
import org.apache.shenyu.plugin.divide.handler.DividePluginDataHandler;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Objects;

/**
 * Divide Plugin.
 *
 * @author xiaoyu(Myth)
 */
@Slf4j
public class DividePlugin extends AbstractSoulPlugin {

    @SneakyThrows
    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final SoulPluginChain chain, final SelectorData selector, final RuleData rule) {
        final SoulContext soulContext = exchange.getAttribute(Constants.CONTEXT);
        assert soulContext != null;
        final DivideRuleHandle ruleHandle = UpstreamCacheManager.getInstance().obtainHandle(DividePluginDataHandler.getCacheKeyName(rule));
        long headerSize = 0;
        for (List<String> multiHeader : exchange.getRequest().getHeaders().values()) {
            for (String value : multiHeader) {
                headerSize += value.getBytes(StandardCharsets.UTF_8).length;
            }
        }
        if (headerSize > ruleHandle.getHeaderMaxSize()) {
            log.error("request header is too large");
            Object error = SoulResultWrap.error(SoulResultEnum.REQUEST_HEADER_TOO_LARGE.getCode(), SoulResultEnum.REQUEST_HEADER_TOO_LARGE.getMsg(), null);
            return WebFluxResultUtils.result(exchange, error);
        }
        if (exchange.getRequest().getHeaders().getContentLength() > ruleHandle.getRequestMaxSize()) {
            log.error("request entity is too large");
            Object error = SoulResultWrap.error(SoulResultEnum.REQUEST_ENTITY_TOO_LARGE.getCode(), SoulResultEnum.REQUEST_ENTITY_TOO_LARGE.getMsg(), null);
            return WebFluxResultUtils.result(exchange, error);
        }
        final List<DivideUpstream> upstreamList = UpstreamCacheManager.getInstance().findUpstreamListBySelectorId(selector.getId());
        if (CollectionUtils.isEmpty(upstreamList)) {
            log.error("divide upstream configuration error： {}", rule.toString());
            Object error = SoulResultWrap.error(SoulResultEnum.CANNOT_FIND_URL.getCode(), SoulResultEnum.CANNOT_FIND_URL.getMsg(), null);
            return WebFluxResultUtils.result(exchange, error);
        }
        final String ip = Objects.requireNonNull(exchange.getRequest().getRemoteAddress()).getAddress().getHostAddress();
        DivideUpstream divideUpstream = LoadBalanceUtils.selector(upstreamList, ruleHandle.getLoadBalance(), ip);
        if (Objects.isNull(divideUpstream)) {
            log.error("divide has no upstream");
            Object error = SoulResultWrap.error(SoulResultEnum.CANNOT_FIND_URL.getCode(), SoulResultEnum.CANNOT_FIND_URL.getMsg(), null);
            return WebFluxResultUtils.result(exchange, error);
        }
        // set the http url
        String domain = buildDomain(divideUpstream);
        String realURL = buildRealURL(domain, soulContext, exchange);
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
        final SoulContext soulContext = exchange.getAttribute(Constants.CONTEXT);
        return !Objects.equals(Objects.requireNonNull(soulContext).getRpcType(), RpcTypeEnum.HTTP.getName());
    }

    @Override
    public int getOrder() {
        return PluginEnum.DIVIDE.getCode();
    }

    @Override
    protected Mono<Void> handleSelectorIsNull(final String pluginName, final ServerWebExchange exchange, final SoulPluginChain chain) {
        return FallbackUtils.getNoSelectorResult(pluginName, exchange);
    }

    @Override
    protected Mono<Void> handleRuleIsNull(final String pluginName, final ServerWebExchange exchange, final SoulPluginChain chain) {
        return FallbackUtils.getNoRuleResult(pluginName, exchange);
    }

    private String buildDomain(final DivideUpstream divideUpstream) {
        String protocol = divideUpstream.getProtocol();
        if (StringUtils.isBlank(protocol)) {
            protocol = "http://";
        }
        return protocol + divideUpstream.getUpstreamUrl().trim();
    }

    private String buildRealURL(final String domain, final SoulContext soulContext, final ServerWebExchange exchange) {
        String path = domain;
        final String rewriteURI = (String) exchange.getAttributes().get(Constants.REWRITE_URI);
        if (StringUtils.isNoneBlank(rewriteURI)) {
            path = path + rewriteURI;
        } else {
            final String realUrl = soulContext.getRealUrl();
            if (StringUtils.isNoneBlank(realUrl)) {
                path = path + realUrl;
            }
        }
        String query = exchange.getRequest().getURI().getRawQuery();
        if (StringUtils.isNoneBlank(query)) {
            return path + "?" + query;
        }
        return path;
    }
}
