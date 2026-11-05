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

package org.apache.shenyu.plugin.agent.gateway.remote;

import java.net.URI;
import java.util.Collection;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

/** Immutable deployment-owned routes; hostname targets fail closed until approved. */
final class RemoteTransportRouteSet {

    private final Map<URI, RemoteTransportRoute> routes;

    private RemoteTransportRouteSet(final Map<URI, RemoteTransportRoute> routes) {
        this.routes = Map.copyOf(routes);
    }

    /**
     * Parse endpoint=address route declarations.
     * @param declarations deployment-owned declarations, never Admin plugin data
     * @return immutable routes
     */
    static RemoteTransportRouteSet parse(final Collection<String> declarations) {
        if (Objects.isNull(declarations)) {
            throw new IllegalArgumentException("Missing controlled MCP routes");
        }
        Map<URI, RemoteTransportRoute> parsed = declarations.stream()
                .map(RemoteTransportRouteSet::parseRoute)
                .collect(Collectors.toUnmodifiableMap(RemoteTransportRoute::endpoint, route -> route));
        return new RemoteTransportRouteSet(parsed);
    }

    private static RemoteTransportRoute parseRoute(final String declaration) {
        if (Objects.isNull(declaration) || declaration.length() > 2048) {
            throw new IllegalArgumentException("Invalid controlled MCP route");
        }
        int separator = declaration.lastIndexOf('=');
        if (separator < 1) {
            throw new IllegalArgumentException("Invalid controlled MCP route");
        }
        URI endpoint = URI.create(declaration.substring(0, separator));
        String address = declaration.substring(separator + 1);
        if (isLiteralIpv4(endpoint.getHost())) {
            if (!address.isEmpty()) {
                throw new IllegalArgumentException("Fixed-IP route cannot override its endpoint address");
            }
            return RemoteTransportRoute.fixed(endpoint);
        }
        if (address.isEmpty()) {
            throw new IllegalArgumentException("Controlled hostname route requires an address");
        }
        return RemoteTransportRoute.controlledDns(endpoint, address);
    }

    private static boolean isLiteralIpv4(final String value) {
        return Objects.nonNull(value) && value.matches("(?:0|[1-9][0-9]{0,2})(?:\\.(?:0|[1-9][0-9]{0,2})){3}");
    }

    Set<URI> endpoints() {
        return routes.keySet();
    }

    RemoteTransportRoute route(final URI endpoint) {
        RemoteTransportRoute route = routes.get(endpoint);
        if (Objects.isNull(route) || !route.endpoint().equals(endpoint)) {
            throw new SecurityException("Target has no approved transport route");
        }
        return route;
    }

    @Override
    public String toString() {
        return "RemoteTransportRouteSet[endpoints=" + routes.size() + "]";
    }
}
