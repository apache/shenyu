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
 * In-memory route↔gateway/listener bindings written by the HTTPRoute reconciler, read for
 * attachedRoutes counts and cascade cleanup. Tracked per listener because the spec defines
 * attachedRoutes per listener.
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

    /** Bind a route to a Gateway on the given listeners, replacing the previous binding. */
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

    /** Route keys attached through any listener, null if none. */
    public Set<String> getRoutesByGateway(final String gatewayNamespace, final String gatewayName) {
        Map<String, Set<String>> routes = GATEWAY_ROUTE_MAP.get(gatewayKey(gatewayNamespace, gatewayName));
        return Objects.isNull(routes) || routes.isEmpty() ? null : Set.copyOf(routes.keySet());
    }

    /** Route keys attached through one listener; its size is the listener's attachedRoutes. */
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

    /** Gateway keys the route is bound to via multiple parentRefs, null if none. */
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
