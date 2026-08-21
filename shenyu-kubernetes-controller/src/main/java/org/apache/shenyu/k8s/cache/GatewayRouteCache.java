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
import com.google.common.collect.Sets;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

public final class GatewayRouteCache {

    private static final GatewayRouteCache INSTANCE = new GatewayRouteCache();

    private static final Map<String, List<String>> ROUTE_SELECTOR_MAP = Maps.newConcurrentMap();

    private static final Map<String, Set<String>> GATEWAY_ROUTE_MAP = Maps.newConcurrentMap();

    private static final Map<String, Set<String>> ROUTE_GATEWAY_MAP = Maps.newConcurrentMap();

    private GatewayRouteCache() {
    }

    public static GatewayRouteCache getInstance() {
        return INSTANCE;
    }

    public void putRouteSelectors(final String namespace, final String routeName,
                                  final String pluginName, final List<String> selectorIds) {
        ROUTE_SELECTOR_MAP.put(routeKey(namespace, routeName, pluginName), selectorIds);
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
        String rKey = routeKey(routeNamespace, routeName);
        GATEWAY_ROUTE_MAP.computeIfAbsent(gwKey, k -> Sets.newConcurrentHashSet()).add(rKey);
        ROUTE_GATEWAY_MAP.computeIfAbsent(rKey, k -> Sets.newConcurrentHashSet()).add(gwKey);
    }

    public Set<String> getRoutesByGateway(final String gatewayNamespace, final String gatewayName) {
        Set<String> routes = GATEWAY_ROUTE_MAP.get(gatewayKey(gatewayNamespace, gatewayName));
        return Objects.isNull(routes) ? null : Set.copyOf(routes);
    }

    /**
     * ShenYu Gateways a route is currently bound to; a route may attach to several
     * ShenYu Gateways via multiple parentRefs.
     *
     * @param routeNamespace namespace of the route
     * @param routeName name of the route
     * @return gateway keys ("namespace/name") the route is bound to, null if none
     */
    public Set<String> getGatewaysForRoute(final String routeNamespace, final String routeName) {
        Set<String> gateways = ROUTE_GATEWAY_MAP.get(routeKey(routeNamespace, routeName));
        return Objects.isNull(gateways) ? null : Set.copyOf(gateways);
    }

    public Set<String> removeRoutesByGateway(final String gatewayNamespace, final String gatewayName) {
        String gwKey = gatewayKey(gatewayNamespace, gatewayName);
        Set<String> routes = GATEWAY_ROUTE_MAP.remove(gwKey);
        if (Objects.nonNull(routes)) {
            // Drop this Gateway from each route's binding set and remove emptied entries,
            // so a route still bound to another ShenYu Gateway keeps a non-empty set.
            routes.forEach(rKey -> ROUTE_GATEWAY_MAP.computeIfPresent(rKey, (k, gateways) -> {
                gateways.remove(gwKey);
                return gateways.isEmpty() ? null : gateways;
            }));
        }
        return routes;
    }

    public void removeRouteGatewayBinding(final String routeNamespace, final String routeName) {
        String rKey = routeKey(routeNamespace, routeName);
        Set<String> gwKeys = ROUTE_GATEWAY_MAP.remove(rKey);
        if (Objects.nonNull(gwKeys)) {
            for (String gwKey : gwKeys) {
                Set<String> routes = GATEWAY_ROUTE_MAP.get(gwKey);
                if (Objects.nonNull(routes)) {
                    routes.remove(rKey);
                }
            }
        }
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
