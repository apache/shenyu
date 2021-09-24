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

package org.apache.shenyu.plugin.request;

import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.RequestHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.CollectionUtils;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.request.handler.RequestPluginHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpCookie;
import org.springframework.http.HttpHeaders;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.util.UriComponentsBuilder;

import reactor.core.publisher.Mono;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * The RequestPlugin.
 */
public class RequestPlugin extends AbstractShenyuPlugin {

    private static final Logger LOG = LoggerFactory.getLogger(RequestPlugin.class);

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector,
            final RuleData rule) {
        RequestHandle requestHandle = RequestPluginHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(rule));
        if (Objects.isNull(requestHandle) || requestHandle.isEmptyConfig()) {
            LOG.error("request handler can not configuration：{}", requestHandle);
            return chain.execute(exchange);
        }
        ServerHttpRequest request = exchange.getRequest();
        ServerWebExchange modifiedExchange = exchange.mutate()
                .request(originalRequest -> originalRequest.uri(
                        UriComponentsBuilder.fromUri(exchange.getRequest()
                                .getURI())
                                .replaceQueryParams(getQueryParams(request, requestHandle))
                                .build()
                                .toUri()
                        ).headers(httpHeaders -> setHeaders(httpHeaders, request, requestHandle))
                ).build();
        return chain.execute(modifiedExchange);
    }

    @Override
    public int getOrder() {
        return PluginEnum.REQUEST.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.REQUEST.getName();
    }

    /**
     * getHeaders.
     *
     * @param request serverHttpRequest
     * @param requestHandle requestHandle
     * @return new headers
     */
    private void setHeaders(final HttpHeaders headers, final ServerHttpRequest request, final RequestHandle requestHandle) {
        List<HttpCookie> cookies = getCookies(request, requestHandle).values().stream()
                .flatMap(Collection::stream).collect(Collectors.toList());
        if (CollectionUtils.isNotEmpty(cookies)) {
            headers.remove(HttpHeaders.COOKIE);
            headers.set(HttpHeaders.COOKIE, StringUtils.join(cookies, "; "));
        }

        RequestHandle.ShenyuRequestHeader shenyuReqHeader = requestHandle.getHeader();
        if (Objects.isNull(shenyuReqHeader)) {
            return;
        }
        if (MapUtils.isNotEmpty(shenyuReqHeader.getAddHeaders())) {
            shenyuReqHeader.getAddHeaders().entrySet().forEach(s -> this.fillHeader(s, headers));
        }
        if (MapUtils.isNotEmpty(shenyuReqHeader.getSetHeaders())) {
            shenyuReqHeader.getSetHeaders().entrySet().forEach(s -> this.fillHeader(s, headers));
        }
        if (MapUtils.isNotEmpty(shenyuReqHeader.getReplaceHeaderKeys())) {
            shenyuReqHeader.getReplaceHeaderKeys().entrySet().forEach(s -> this.replaceHeaderKey(s, headers));
        }
        if (CollectionUtils.isNotEmpty(shenyuReqHeader.getRemoveHeaderKeys())) {
            shenyuReqHeader.getRemoveHeaderKeys().forEach(headers::remove);
        }
    }

    /**
     * get cookies.
     *
     * @param request serverHttpRequest
     * @param requestHandle requestHandle
     * @return new cookies
     */
    private MultiValueMap<String, HttpCookie> getCookies(final ServerHttpRequest request, final RequestHandle requestHandle) {
        RequestHandle.ShenyuCookie shenyuCookie = requestHandle.getCookie();
        MultiValueMap<String, HttpCookie> cookies = new LinkedMultiValueMap<>(request.getCookies());
        if (Objects.isNull(shenyuCookie)) {
            return cookies;
        }
        if (MapUtils.isNotEmpty(shenyuCookie.getAddCookies())) {
            shenyuCookie.getAddCookies().entrySet().forEach(s -> this.fillCookie(s, cookies));
        }
        if (MapUtils.isNotEmpty(shenyuCookie.getSetCookies())) {
            shenyuCookie.getSetCookies().entrySet().forEach(s -> this.fillCookie(s, cookies));
        }
        if (MapUtils.isNotEmpty(shenyuCookie.getReplaceCookieKeys())) {
            shenyuCookie.getReplaceCookieKeys().entrySet().forEach(s -> this.replaceCookieKey(s, cookies));
        }
        if (CollectionUtils.isNotEmpty(shenyuCookie.getRemoveCookieKeys())) {
            shenyuCookie.getRemoveCookieKeys().forEach(cookies::remove);
        }
        return cookies;
    }

    /**
     * get queryParams.
     *
     * @param request serverHttpRequest
     * @param requestHandle requestHandle
     * @return new queryParams
     */
    private MultiValueMap<String, String> getQueryParams(final ServerHttpRequest request, final RequestHandle requestHandle) {
        RequestHandle.ShenyuRequestParameter shenyuReqParameter = requestHandle.getParameter();
        MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>(request.getQueryParams());
        if (Objects.isNull(shenyuReqParameter)) {
            return queryParams;
        }
        if (MapUtils.isNotEmpty(shenyuReqParameter.getAddParameters())) {
            shenyuReqParameter.getAddParameters().entrySet().forEach(s -> this.fillParameter(s, queryParams));
        }
        if (MapUtils.isNotEmpty(shenyuReqParameter.getSetParameters())) {
            shenyuReqParameter.getSetParameters().entrySet().forEach(s -> this.fillParameter(s, queryParams));
        }
        if (MapUtils.isNotEmpty(shenyuReqParameter.getReplaceParameterKeys())) {
            shenyuReqParameter.getReplaceParameterKeys().entrySet().forEach(s -> this.replaceParameterKey(s, queryParams));
        }
        if (CollectionUtils.isNotEmpty(shenyuReqParameter.getRemoveParameterKeys())) {
            shenyuReqParameter.getRemoveParameterKeys().forEach(queryParams::remove);
        }
        return queryParams;
    }

    private void replaceParameterKey(final Map.Entry<String, String> shenyuParam, final MultiValueMap<String, String> queryParams) {
        List<String> values = queryParams.get(shenyuParam.getKey());
        if (Objects.nonNull(values)) {
            queryParams.addAll(shenyuParam.getValue(), values);
            queryParams.remove(shenyuParam.getKey());
        }
    }

    private void fillParameter(final Map.Entry<String, String> shenyuParam, final MultiValueMap<String, String> queryParams) {
        queryParams.set(shenyuParam.getKey(), shenyuParam.getValue());
    }

    private void replaceCookieKey(final Map.Entry<String, String> shenyuCookie, final MultiValueMap<String, HttpCookie> cookies) {
        List<HttpCookie> httpCookies = cookies.get(shenyuCookie.getKey());
        if (Objects.nonNull(httpCookies)) {
            cookies.addAll(shenyuCookie.getValue(), httpCookies);
            cookies.remove(shenyuCookie.getKey());
        }
    }

    private void fillCookie(final Map.Entry<String, String> shenyuCookie, final MultiValueMap<String, HttpCookie> cookies) {
        cookies.set(shenyuCookie.getKey(), new HttpCookie(shenyuCookie.getKey(), shenyuCookie.getValue()));
    }

    private void replaceHeaderKey(final Map.Entry<String, String> shenyuHeader, final HttpHeaders headers) {
        List<String> values = headers.get(shenyuHeader.getKey());
        if (Objects.nonNull(values)) {
            headers.addAll(shenyuHeader.getValue(), values);
            headers.remove(shenyuHeader.getKey());
        }
    }

    private void fillHeader(final Map.Entry<String, String> shenyuHeader, final HttpHeaders headers) {
        headers.set(shenyuHeader.getKey(), shenyuHeader.getValue());
    }
}
