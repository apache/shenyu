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

    /** Listeners matching the optional sectionName and parentRef port; both selectors must match. */
    public static List<JsonObject> selectListeners(final JsonObject gatewayRaw, final String sectionName,
                                                   final Long parentPort) {
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
            boolean nameMatches = Objects.isNull(sectionName) || sectionName.equals(nameOf(listener));
            boolean portMatches = Objects.isNull(parentPort) || parentPort.equals(portOf(listener));
            if (nameMatches && portMatches) {
                result.add(listener);
            }
        }
        return result;
    }

    /** Listeners selected by sectionName only. */
    public static List<JsonObject> selectListeners(final JsonObject gatewayRaw, final String sectionName) {
        return selectListeners(gatewayRaw, sectionName, null);
    }

    /** True when the listener's port equals the served data-plane port; a portless listener cannot be confirmed served. */
    public static boolean servesPort(final JsonObject listener, final long servedPort) {
        Long port = portOf(listener);
        return Objects.nonNull(port) && port == servedPort;
    }

    public static String nameOf(final JsonObject listener) {
        return JsonFields.getString(listener, "name");
    }

    /** Listener protocol; defaults to HTTP per the spec. */
    public static String protocolOf(final JsonObject listener) {
        String protocol = JsonFields.getString(listener, "protocol");
        return Objects.isNull(protocol) ? GatewayApiConstants.PROTOCOL_HTTP : protocol;
    }

    public static String hostnameOf(final JsonObject listener) {
        return JsonFields.getString(listener, "hostname");
    }

    /** Listener port; null when absent (foreign status is not schema-guaranteed). */
    public static Long portOf(final JsonObject listener) {
        return JsonFields.getLong(listener, "port");
    }

    /** Only plain HTTP is supported. */
    public static boolean isSupportedProtocol(final JsonObject listener) {
        return GatewayApiConstants.PROTOCOL_HTTP.equals(protocolOf(listener));
    }

    /** Spec default is Same, from=All allows all; from=Selector is unimplemented and denies (widening would break isolation). */
    public static boolean allowsNamespace(final JsonObject listener, final String routeNamespace, final String gatewayNamespace) {
        String from = fromOf(listener);
        if (Objects.isNull(from) || "Same".equals(from)) {
            return Objects.equals(routeNamespace, gatewayNamespace);
        }
        return "All".equals(from);
    }

    /** from=Selector is unsupported; distinguishable so status reports UnsupportedValue, not a permission denial. */
    public static boolean usesUnsupportedFrom(final JsonObject listener) {
        return "Selector".equals(fromOf(listener));
    }

    private static String fromOf(final JsonObject listener) {
        JsonObject allowedRoutes = JsonFields.getJsonObject(listener, "allowedRoutes");
        JsonObject namespaces = JsonFields.getJsonObject(allowedRoutes, "namespaces");
        return JsonFields.getString(namespaces, "from");
    }

    /** Absent kinds means all protocol-matching kinds, i.e. HTTPRoute for HTTP. */
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

    /** Route × listener hostname intersection; a null listener hostname imposes no restriction. */
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

    /** The more specific of two overlapping hostnames; {@code *.example.com} matches one or more labels, never the bare domain. */
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
