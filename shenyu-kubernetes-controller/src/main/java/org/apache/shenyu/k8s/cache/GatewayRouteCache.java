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

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * In-memory bindings between ShenYu-managed Gateways, their listeners and the HTTPRoutes
 * attached to them, maintained by the HTTPRoute reconciler and read by the Gateway
 * reconciler (listener-level attachedRoutes) and the deletion paths (cascade cleanup).
 *
 * <p>Bindings are tracked per listener ({@code attachedRoutes} is defined per listener in
 * the Gateway API spec): a gateway entry maps each attached route to the set of listener
 * names that accepted it, so a route targeting only {@code sectionName: http} is counted
 * on that listener alone.
 */
public final class GatewayRouteCache {

    private static final GatewayRouteCache INSTANCE = new GatewayRouteCache();

    private static final Map<String, List<String>> ROUTE_SELECTOR_MAP = Maps.newConcurrentMap();

    /** gatewayKey → (routeKey → listener names that accepted the route). */
    private static final Map<String, Map<String, Set<String>>> GATEWAY_ROUTE_MAP = Maps.newConcurrentMap();

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

    /**
     * Bind a route to a Gateway on the given listener names, replacing the route's previous
     * binding to that Gateway.
     *
     * @param gatewayNamespace namespace of the Gateway
     * @param gatewayName name of the Gateway
     * @param listenerNames listeners of the Gateway that accepted the route
     * @param routeNamespace namespace of the route
     * @param routeName name of the route
     */
    public void bindRouteToGateway(final String gatewayNamespace, final String gatewayName,
                                   final Set<String> listenerNames,
                                   final String routeNamespace, final String routeName) {
        String gwKey = gatewayKey(gatewayNamespace, gatewayName);
        String rKey = routeKey(routeNamespace, routeName);
        GATEWAY_ROUTE_MAP.computeIfAbsent(gwKey, k -> Maps.newConcurrentMap())
                .compute(rKey, (k, listeners) -> {
                    Set<String> merged = Objects.isNull(listeners) ? Sets.newConcurrentHashSet() : listeners;
                    merged.addAll(listenerNames);
                    return merged;
                });
    }

    /**
     * Routes attached to a Gateway through any of its listeners.
     *
     * @param gatewayNamespace namespace of the Gateway
     * @param gatewayName name of the Gateway
     * @return route keys ("namespace/name") attached to the Gateway, null if none
     */
    public Set<String> getRoutesByGateway(final String gatewayNamespace, final String gatewayName) {
        Map<String, Set<String>> routes = GATEWAY_ROUTE_MAP.get(gatewayKey(gatewayNamespace, gatewayName));
        return Objects.isNull(routes) || routes.isEmpty() ? null : Set.copyOf(routes.keySet());
    }

    /**
     * Routes attached to one specific listener of a Gateway; the count of this set is the
     * listener's {@code attachedRoutes} status value.
     *
     * @param gatewayNamespace namespace of the Gateway
     * @param gatewayName name of the Gateway
     * @param listenerName name of the listener
     * @return route keys ("namespace/name") attached through that listener, empty if none
     */
    public Set<String> getRoutesByListener(final String gatewayNamespace, final String gatewayName,
                                           final String listenerName) {
        Map<String, Set<String>> routes = GATEWAY_ROUTE_MAP.get(gatewayKey(gatewayNamespace, gatewayName));
        if (Objects.isNull(routes)) {
            return Set.of();
        }
        Set<String> attached = new HashSet<>();
        routes.forEach((routeKey, listeners) -> {
            if (listeners.contains(listenerName)) {
                attached.add(routeKey);
            }
        });
        return attached;
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
        String rKey = routeKey(routeNamespace, routeName);
        Set<String> gateways = new HashSet<>();
        GATEWAY_ROUTE_MAP.forEach((gwKey, routes) -> {
            if (routes.containsKey(rKey)) {
                gateways.add(gwKey);
            }
        });
        return gateways.isEmpty() ? null : gateways;
    }

    public Set<String> removeRoutesByGateway(final String gatewayNamespace, final String gatewayName) {
        String gwKey = gatewayKey(gatewayNamespace, gatewayName);
        Map<String, Set<String>> routes = GATEWAY_ROUTE_MAP.remove(gwKey);
        return Objects.isNull(routes) || routes.isEmpty() ? null : Set.copyOf(routes.keySet());
    }

    public void removeRouteGatewayBinding(final String routeNamespace, final String routeName) {
        String rKey = routeKey(routeNamespace, routeName);
        GATEWAY_ROUTE_MAP.forEach((gwKey, routes) -> routes.remove(rKey));
    }

    /**
     * Clear all cached data. Used for testing.
     */
    public void clear() {
        ROUTE_SELECTOR_MAP.clear();
        GATEWAY_ROUTE_MAP.clear();
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
