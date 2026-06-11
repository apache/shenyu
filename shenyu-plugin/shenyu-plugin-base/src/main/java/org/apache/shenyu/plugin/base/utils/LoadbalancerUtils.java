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

package org.apache.shenyu.plugin.base.utils;

import org.apache.commons.collections4.MapUtils;
import org.apache.shenyu.loadbalancer.entity.LoadBalanceData;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.loadbalancer.factory.LoadBalancerFactory;
import org.springframework.http.HttpCookie;
import org.springframework.http.HttpHeaders;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.util.MultiValueMap;
import org.springframework.web.server.ServerWebExchange;

import java.net.URI;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * The type Loadbalancer utils.
 */
public final class LoadbalancerUtils {
    
    private LoadbalancerUtils() {
    }
    
    /**
     * Gets for exchange.
     *
     * @param upstreamList the upstream list
     * @param algorithm the algorithm
     * @param exchange the exchange
     * @return the for exchange
     */
    public static Upstream getForExchange(final List<Upstream> upstreamList, final String algorithm, final ServerWebExchange exchange) {
        LoadBalanceData loadBalanceData = buildLoadBalanceData(exchange);
        return LoadBalancerFactory.selector(upstreamList, algorithm, loadBalanceData);
    }
    
    /**
     * Gets for no exchange.
     *
     * @param upstreamList the upstream list
     * @param algorithm the algorithm
     * @return the for no exchange
     */
    public static Upstream getForNoExchange(final List<Upstream> upstreamList, final String algorithm) {
        return LoadBalancerFactory.selector(upstreamList, algorithm, new LoadBalanceData());
    }
    
    private static LoadBalanceData buildLoadBalanceData(final ServerWebExchange exchange) {
        ServerHttpRequest request = exchange.getRequest();
        String ip = Objects.requireNonNull(request.getRemoteAddress()).getAddress().getHostAddress();
        String httpMethod = request.getMethod().name();
        URI uri = exchange.getRequest().getURI();
        HttpHeaders headers = request.getHeaders();
        MultiValueMap<String, HttpCookie> cookies = request.getCookies();
        Map<String, Object> attributes = exchange.getAttributes();
        MultiValueMap<String, String> queryParams = request.getQueryParams();
        return new LoadBalanceData(httpMethod, ip, uri,
                buildMultiValueMap(headers),
                buildCookies(cookies),
                attributes,
                buildMultiValueMap(queryParams));
    }
    
    private static Map<String, Collection<String>> buildMultiValueMap(final MultiValueMap<String, String> queryParams) {
        Map<String, Collection<String>> resultMap = new HashMap<>();
        if (MapUtils.isNotEmpty(queryParams)) {
            resultMap.putAll(queryParams);
        }
        return resultMap;
    }
    
    private static Map<String, String> buildCookies(final MultiValueMap<String, HttpCookie> cookies) {
        Map<String, String> resultMap = new HashMap<>();
        if (MapUtils.isNotEmpty(cookies)) {
            cookies.forEach((key, value) -> value.forEach((cookie) -> resultMap.put(cookie.getName(), cookie.getValue())));
        }
        return resultMap;
    }
}
