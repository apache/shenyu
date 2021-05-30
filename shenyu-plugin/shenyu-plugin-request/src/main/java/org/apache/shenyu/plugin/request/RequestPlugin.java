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

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.MapUtils;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.RequestHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.CollectionUtils;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.request.cache.RequestRuleHandleCache;
import org.springframework.http.HttpCookie;
import org.springframework.http.HttpHeaders;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpRequestDecorator;
import org.springframework.util.MultiValueMap;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.util.annotation.NonNull;

import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * The RequestPlugin.
 */
@Slf4j
public class RequestPlugin extends AbstractShenyuPlugin {

    @Override
    protected Mono<Void> doExecute(ServerWebExchange exchange, ShenyuPluginChain chain, SelectorData selector, RuleData rule) {
        RequestHandle requestHandle = RequestRuleHandleCache.getInstance().obtainHandle(CacheKeyUtils.INST.getKey(rule));
        if (Objects.isNull(requestHandle)) {
            log.error("uri request rule can not configuration.");
            return chain.execute(exchange);
        }
        ServerHttpRequest request = exchange.getRequest();
        return chain.execute(exchange.mutate().request(new RequestPluginServerHttpRequest(request, requestHandle)).build());
    }

    @Override
    public int getOrder() {
        return PluginEnum.REQUEST.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.REQUEST.getName();
    }

    static class RequestPluginServerHttpRequest extends ServerHttpRequestDecorator {
        private final RequestHandle requestHandle;

        public RequestPluginServerHttpRequest(ServerHttpRequest request, RequestHandle requestHandle) {
            super(request);
            this.requestHandle = requestHandle;
        }

        @NonNull
        @Override
        public HttpHeaders getHeaders() {
            RequestHandle.ShenyuRequestHeader shenyuReqHeader = requestHandle.getHeader();
            HttpHeaders headers = super.getHeaders();
            if (Objects.isNull(shenyuReqHeader)) {
                return headers;
            }
            if (Objects.nonNull(shenyuReqHeader.getAddHeaders())
                    && MapUtils.isNotEmpty(shenyuReqHeader.getAddHeaders())) {
                shenyuReqHeader.getAddHeaders().entrySet().forEach(s -> this.fillHeader(s, headers));
            }
            if (Objects.nonNull(shenyuReqHeader.getSetHeaders())
                    && MapUtils.isNotEmpty(shenyuReqHeader.getSetHeaders())) {
                shenyuReqHeader.getSetHeaders().entrySet().forEach(s -> this.fillHeader(s, headers));
            }
            if (Objects.nonNull(shenyuReqHeader.getReplaceHeaderKeys())
                    && MapUtils.isNotEmpty(shenyuReqHeader.getReplaceHeaderKeys())) {
                shenyuReqHeader.getReplaceHeaderKeys().entrySet().forEach(s -> this.replaceHeaderKey(s, headers));
            }
            if (Objects.nonNull(shenyuReqHeader.getRemoveHeaderKeys())
                    && CollectionUtils.isNotEmpty(shenyuReqHeader.getRemoveHeaderKeys())) {
                shenyuReqHeader.getRemoveHeaderKeys().forEach(headers::remove);
            }
            return headers;
        }

        @NonNull
        @Override
        public MultiValueMap<String, HttpCookie> getCookies() {
            RequestHandle.ShenyuCookie shenyuCookie = requestHandle.getCookie();
            MultiValueMap<String, HttpCookie> cookies = super.getCookies();
            if (Objects.isNull(shenyuCookie)) {
                return cookies;
            }
            if (Objects.nonNull(shenyuCookie.getAddCookies()) && MapUtils.isNotEmpty(shenyuCookie.getAddCookies())) {
                shenyuCookie.getAddCookies().entrySet().forEach(s -> this.fillCookie(s, cookies));
            }
            if (Objects.nonNull(shenyuCookie.getSetCookies()) && MapUtils.isNotEmpty(shenyuCookie.getSetCookies())) {
                shenyuCookie.getSetCookies().entrySet().forEach(s -> this.fillCookie(s, cookies));
            }
            if (Objects.nonNull(shenyuCookie.getReplaceCookieKeys())
                    && MapUtils.isNotEmpty(shenyuCookie.getReplaceCookieKeys())) {
                shenyuCookie.getReplaceCookieKeys().entrySet().forEach(s -> this.replaceCookieKey(s, cookies));
            }
            if (Objects.nonNull(shenyuCookie.getRemoveCookieKeys())
                    && CollectionUtils.isNotEmpty(shenyuCookie.getRemoveCookieKeys())) {
                shenyuCookie.getRemoveCookieKeys().forEach(cookies::remove);
            }
            return cookies;
        }

        @NonNull
        @Override
        public MultiValueMap<String, String> getQueryParams() {
            RequestHandle.ShenyuRequestParameter shenyuReqParameter = requestHandle.getParameter();
            MultiValueMap<String, String> queryParams = super.getQueryParams();
            if (Objects.isNull(shenyuReqParameter)) {
                return queryParams;
            }
            if (Objects.nonNull(shenyuReqParameter.getAddParameters())
                    && MapUtils.isNotEmpty(shenyuReqParameter.getAddParameters())) {
                shenyuReqParameter.getAddParameters().entrySet().forEach(s -> this.fillParameter(s, queryParams));
            }
            if (Objects.nonNull(shenyuReqParameter.getSetParameters())
                    && MapUtils.isNotEmpty(shenyuReqParameter.getSetParameters())) {
                shenyuReqParameter.getSetParameters().entrySet().forEach(s -> this.fillParameter(s, queryParams));
            }
            if (Objects.nonNull(shenyuReqParameter.getReplaceParameterKeys())
                    && MapUtils.isNotEmpty(shenyuReqParameter.getReplaceParameterKeys())) {
                shenyuReqParameter.getReplaceParameterKeys().entrySet().forEach(s -> this.replaceParameterKey(s, queryParams));
            }
            if (Objects.nonNull(shenyuReqParameter.getRemoveParameterKeys())
                    && CollectionUtils.isNotEmpty(shenyuReqParameter.getRemoveParameterKeys())) {
                shenyuReqParameter.getRemoveParameterKeys().forEach(queryParams::remove);
            }
            return queryParams;
        }

        private void replaceParameterKey(Map.Entry<String, String> entry, MultiValueMap<String, String> queryParams) {
            List<String> values = queryParams.get(entry.getKey());
            if (Objects.nonNull(values)) {
                queryParams.addAll(entry.getValue(), values);
                queryParams.remove(entry.getKey());
            }
        }

        private void fillParameter(Map.Entry<String, String> entry, MultiValueMap<String, String> queryParams) {
            queryParams.set(entry.getKey(), entry.getValue());
        }

        private void replaceCookieKey(Map.Entry<String, String> entry, MultiValueMap<String, HttpCookie> cookies) {
            List<HttpCookie> httpCookies = cookies.get(entry.getKey());
            if (Objects.nonNull(httpCookies)) {
                cookies.addAll(entry.getValue(), httpCookies);
                cookies.remove(entry.getKey());
            }
        }

        private void fillCookie(Map.Entry<String, String> entry, MultiValueMap<String, HttpCookie> cookies) {
            cookies.set(entry.getKey(), new HttpCookie(entry.getKey(), entry.getValue()));
        }

        private void replaceHeaderKey(Map.Entry<String, String> entry, HttpHeaders headers) {
            List<String> values = headers.get(entry.getKey());
            if (Objects.nonNull(values)) {
                headers.addAll(entry.getValue(), values);
                headers.remove(entry.getKey());
            }
        }

        private void fillHeader(Map.Entry<String, String> s, HttpHeaders headers) {
            headers.set(s.getKey(), s.getValue());
        }
    }
}
