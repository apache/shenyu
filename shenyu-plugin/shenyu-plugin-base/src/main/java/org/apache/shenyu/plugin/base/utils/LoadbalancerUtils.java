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

import org.apache.shenyu.loadbalancer.entity.LoadBalanceData;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.loadbalancer.factory.LoadBalancerFactory;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.web.server.ServerWebExchange;

import java.net.URI;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;

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
        String ip = Optional.ofNullable(request.getRemoteAddress())
                .map(address -> address.getAddress())
                .map(address -> address.getHostAddress())
                .orElse("127.0.0.1");
        String httpMethod = Optional.ofNullable(request.getMethod()).map(method -> method.name()).orElse("GET");
        URI uri = request.getURI();
        Map<String, Object> attributes = exchange.getAttributes();
        // Only the ip is read by a load balancer (hash); copying headers, cookies and query
        // params here would allocate maps per request without any consumer.
        return new LoadBalanceData(httpMethod, ip, uri,
                Collections.emptyMap(),
                Collections.emptyMap(),
                attributes,
                Collections.emptyMap());
    }
}
