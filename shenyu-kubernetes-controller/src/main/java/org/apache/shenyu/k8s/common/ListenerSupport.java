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

package org.apache.shenyu.k8s.common;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;

/**
 * Read-only helpers for evaluating Gateway {@code spec.listeners}: which listeners an
 * HTTPRoute may attach to (namespace policy, kind policy, protocol support) and the
 * route/listener hostname intersection mandated by the Gateway API attachment rules.
 */
public final class ListenerSupport {

    private ListenerSupport() {
    }

    /**
     * Listeners selected by a parentRef: the one named by sectionName, or all listeners
     * when sectionName is absent.
     *
     * @param gatewayRaw raw Gateway json
     * @param sectionName optional sectionName of the parentRef
     * @return matching listeners; empty when sectionName matches no listener
     */
    public static List<JsonObject> selectListeners(final JsonObject gatewayRaw, final String sectionName) {
        JsonObject spec = JsonFields.getJsonObject(gatewayRaw, "spec");
        JsonArray listeners = JsonFields.getJsonArray(spec, "listeners");
        List<JsonObject> result = new ArrayList<>();
        if (Objects.isNull(listeners)) {
            return result;
        }
        for (JsonElement element : listeners) {
            if (!element.isJsonObject()) {
                continue;
            }
            JsonObject listener = element.getAsJsonObject();
            if (Objects.isNull(sectionName) || sectionName.equals(nameOf(listener))) {
                result.add(listener);
            }
        }
        return result;
    }

    public static String nameOf(final JsonObject listener) {
        return JsonFields.getString(listener, "name");
    }

    /**
     * Listener protocol; defaults to HTTP per the Gateway API spec.
     *
     * @param listener the listener object
     * @return the protocol, never null
     */
    public static String protocolOf(final JsonObject listener) {
        String protocol = JsonFields.getString(listener, "protocol");
        return Objects.isNull(protocol) ? GatewayApiConstants.PROTOCOL_HTTP : protocol;
    }

    public static String hostnameOf(final JsonObject listener) {
        return JsonFields.getString(listener, "hostname");
    }

    /**
     * Listener port; null when absent (the field is required by the CRD, but status written
     * by other controllers is not schema-guaranteed).
     *
     * @param listener the listener object
     * @return the port, or null
     */
    public static Long portOf(final JsonObject listener) {
        return JsonFields.getLong(listener, "port");
    }

    /**
     * Whether the listener's protocol can be served: only HTTP is supported.
     *
     * @param listener the listener object
     * @return true if the listener speaks plain HTTP
     */
    public static boolean isSupportedProtocol(final JsonObject listener) {
        return GatewayApiConstants.PROTOCOL_HTTP.equals(protocolOf(listener));
    }

    /**
     * Whether the listener's {@code allowedRoutes.namespaces} policy permits a route from
     * {@code routeNamespace} to attach to a Gateway in {@code gatewayNamespace}. The spec
     * default is Same. Selector-based policies are not implemented and deny the attachment:
     * silently widening a label-restricted policy would punch a hole in namespace isolation.
     *
     * @param listener the listener object
     * @param routeNamespace namespace of the attaching HTTPRoute
     * @param gatewayNamespace namespace of the Gateway
     * @return true if the namespace policy permits the attachment
     */
    public static boolean allowsNamespace(final JsonObject listener, final String routeNamespace, final String gatewayNamespace) {
        JsonObject allowedRoutes = JsonFields.getJsonObject(listener, "allowedRoutes");
        JsonObject namespaces = JsonFields.getJsonObject(allowedRoutes, "namespaces");
        String from = JsonFields.getString(namespaces, "from");
        if (Objects.isNull(from) || "Same".equals(from)) {
            return Objects.equals(routeNamespace, gatewayNamespace);
        }
        return "All".equals(from);
    }

    /**
     * Whether the listener's {@code allowedRoutes.kinds} policy permits HTTPRoute. Absent
     * kinds means "all kinds matching the protocol", which is HTTPRoute for HTTP.
     *
     * @param listener the listener object
     * @return true if HTTPRoute is permitted
     */
    public static boolean allowsKind(final JsonObject listener) {
        JsonObject allowedRoutes = JsonFields.getJsonObject(listener, "allowedRoutes");
        JsonArray kinds = JsonFields.getJsonArray(allowedRoutes, "kinds");
        if (Objects.isNull(kinds) || kinds.size() == 0) {
            return true;
        }
        for (JsonElement element : kinds) {
            if (!element.isJsonObject()) {
                continue;
            }
            JsonObject kind = element.getAsJsonObject();
            String group = JsonFields.getString(kind, "group");
            boolean groupMatches = Objects.isNull(group) || GatewayApiConstants.GATEWAY_API_GROUP.equals(group);
            if (groupMatches && GatewayApiConstants.HTTP_ROUTE_KIND.equals(JsonFields.getString(kind, "kind"))) {
                return true;
            }
        }
        return false;
    }

    /**
     * Intersect the route hostnames with a listener hostname per the spec: a listener
     * hostname restricts the route's effective hostnames to their overlap. A null listener
     * hostname imposes no restriction.
     *
     * @param listenerHostname listener hostname, may be null
     * @param routeHostnames hostnames from the HTTPRoute spec, empty means "any host"
     * @return effective hostnames, or null when the two sides have no overlap
     */
    public static List<String> intersectHostnames(final String listenerHostname, final List<String> routeHostnames) {
        if (Objects.isNull(listenerHostname)) {
            return new ArrayList<>(routeHostnames);
        }
        if (routeHostnames.isEmpty()) {
            return List.of(listenerHostname);
        }
        Set<String> overlaps = new LinkedHashSet<>();
        for (String routeHostname : routeHostnames) {
            String overlap = overlap(routeHostname, listenerHostname);
            if (Objects.nonNull(overlap)) {
                overlaps.add(overlap);
            }
        }
        return overlaps.isEmpty() ? null : new ArrayList<>(overlaps);
    }

    /**
     * The more specific of two hostnames when they overlap, null when they do not.
     * Wildcard semantics per spec: {@code *.example.com} matches one or more labels
     * followed by {@code .example.com}, but not {@code example.com} itself.
     */
    private static String overlap(final String routeHostname, final String listenerHostname) {
        if (routeHostname.equals(listenerHostname)) {
            return routeHostname;
        }
        boolean routeWildcard = routeHostname.startsWith("*.");
        boolean listenerWildcard = listenerHostname.startsWith("*.");
        if (!routeWildcard && !listenerWildcard) {
            return null;
        }
        if (routeWildcard && listenerWildcard) {
            String routeSuffix = routeHostname.substring(2);
            String listenerSuffix = listenerHostname.substring(2);
            if (listenerSuffix.endsWith("." + routeSuffix)) {
                return listenerHostname;
            }
            if (routeSuffix.endsWith("." + listenerSuffix)) {
                return routeHostname;
            }
            return null;
        }
        if (routeWildcard) {
            return wildcardCovers(routeHostname, listenerHostname) ? listenerHostname : null;
        }
        return wildcardCovers(listenerHostname, routeHostname) ? routeHostname : null;
    }

    private static boolean wildcardCovers(final String wildcard, final String hostname) {
        return hostname.endsWith("." + wildcard.substring(2));
    }
}
