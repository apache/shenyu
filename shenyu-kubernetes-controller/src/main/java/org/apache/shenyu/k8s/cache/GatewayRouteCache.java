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

package org.apache.shenyu.k8s.cache;

import com.google.common.collect.Maps;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicLong;

public final class GatewayRouteCache {

    private static final GatewayRouteCache INSTANCE = new GatewayRouteCache();

    private static final Map<String, List<String>> ROUTE_SELECTOR_MAP = Maps.newConcurrentMap();

    private static final Map<String, List<String>> GATEWAY_ROUTE_MAP = Maps.newConcurrentMap();

    private static final Map<String, String> ROUTE_GATEWAY_MAP = Maps.newConcurrentMap();

    private static final AtomicLong GROWING_ID = new AtomicLong(10000);

    private GatewayRouteCache() {
    }

    public static GatewayRouteCache getInstance() {
        return INSTANCE;
    }

    public void putRouteSelectors(final String namespace, final String routeName,
                                  final String pluginName, final List<String> selectorIds) {
        ROUTE_SELECTOR_MAP.put(routeKey(namespace, routeName, pluginName), selectorIds);
    }

    public void addRouteSelector(final String namespace, final String routeName,
                                 final String pluginName, final String selectorId) {
        ROUTE_SELECTOR_MAP.computeIfAbsent(routeKey(namespace, routeName, pluginName),
                k -> new CopyOnWriteArrayList<>()).add(selectorId);
    }

    public List<String> getRouteSelectors(final String namespace, final String routeName,
                                          final String pluginName) {
        return ROUTE_SELECTOR_MAP.get(routeKey(namespace, routeName, pluginName));
    }

    public List<String> removeRouteSelectors(final String namespace, final String routeName,
                                             final String pluginName) {
        return ROUTE_SELECTOR_MAP.remove(routeKey(namespace, routeName, pluginName));
    }

    public void bindRouteToGateway(final String gatewayNamespace, final String gatewayName,
                                   final String routeNamespace, final String routeName) {
        String gwKey = gatewayKey(gatewayNamespace, gatewayName);
        GATEWAY_ROUTE_MAP.computeIfAbsent(gwKey, k -> new CopyOnWriteArrayList<>()).add(routeKey(routeNamespace, routeName));
        ROUTE_GATEWAY_MAP.put(routeKey(routeNamespace, routeName), gwKey);
    }

    public List<String> getRoutesByGateway(final String gatewayNamespace, final String gatewayName) {
        return GATEWAY_ROUTE_MAP.get(gatewayKey(gatewayNamespace, gatewayName));
    }

    public List<String> removeRoutesByGateway(final String gatewayNamespace, final String gatewayName) {
        String gwKey = gatewayKey(gatewayNamespace, gatewayName);
        List<String> routes = GATEWAY_ROUTE_MAP.remove(gwKey);
        if (Objects.nonNull(routes)) {
            routes.forEach(ROUTE_GATEWAY_MAP::remove);
        }
        return routes;
    }

    public String getGatewayForRoute(final String routeNamespace, final String routeName) {
        return ROUTE_GATEWAY_MAP.get(routeKey(routeNamespace, routeName));
    }

    public void removeRouteGatewayBinding(final String routeNamespace, final String routeName) {
        String routeKey = routeKey(routeNamespace, routeName);
        String gwKey = ROUTE_GATEWAY_MAP.remove(routeKey);
        if (Objects.nonNull(gwKey)) {
            List<String> routes = GATEWAY_ROUTE_MAP.get(gwKey);
            if (Objects.nonNull(routes)) {
                routes.remove(routeKey);
            }
        }
    }

    public String generateSelectorId() {
        return String.valueOf(GROWING_ID.getAndIncrement());
    }

    public String generateRuleId() {
        return String.valueOf(GROWING_ID.getAndIncrement());
    }

    /**
     * Clear all cached data. Used for testing.
     */
    public void clear() {
        ROUTE_SELECTOR_MAP.clear();
        GATEWAY_ROUTE_MAP.clear();
        ROUTE_GATEWAY_MAP.clear();
    }

    private String routeKey(final String namespace, final String name) {
        return namespace + "/" + name;
    }

    private String routeKey(final String namespace, final String name, final String pluginName) {
        return String.format("%s/%s-%s", namespace, name, pluginName);
    }

    private String gatewayKey(final String namespace, final String name) {
        return namespace + "/" + name;
    }
}
