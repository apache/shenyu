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

import java.net.InetAddress;
import java.net.URI;
import java.net.UnknownHostException;
import java.util.Objects;

/**
 * Immutable route from an approved target URI to its single connection address.
 * DNS targets must be accompanied by an operator-approved address, so the JDK
 * never performs an implicit second resolution after policy validation.
 */
final class RemoteTransportRoute {

    private final URI endpoint;

    private final InetAddress address;

    private RemoteTransportRoute(final URI endpoint, final InetAddress address) {
        this.endpoint = endpoint;
        this.address = address;
    }

    /**
     * Create a route whose connection address is the endpoint's fixed IPv4 address.
     * @param endpoint approved fixed-IP endpoint
     * @return immutable route
     */
    static RemoteTransportRoute fixed(final URI endpoint) {
        RemoteTransportPolicy.fixedEndpoint(endpoint);
        try {
            return new RemoteTransportRoute(endpoint, InetAddress.getByName(endpoint.getHost()));
        } catch (UnknownHostException error) {
            throw new IllegalArgumentException("Invalid fixed MCP endpoint address");
        }
    }

    /**
     * Create a controlled DNS route without querying DNS.
     * @param endpoint approved HTTPS hostname endpoint
     * @param approvedAddress operator-approved literal IPv4 address
     * @return immutable route
     */
    static RemoteTransportRoute controlledDns(final URI endpoint, final String approvedAddress) {
        RemoteTransportPolicy.httpsHostname(endpoint);
        if (Objects.isNull(approvedAddress) || !approvedAddress.matches("(?:0|[1-9][0-9]{0,2})(?:\\.(?:0|[1-9][0-9]{0,2})){3}")) {
            throw new IllegalArgumentException("Invalid approved MCP address");
        }
        try {
            InetAddress address = InetAddress.getByName(approvedAddress);
            if (address.isAnyLocalAddress() || address.isLinkLocalAddress()
                    || address.isMulticastAddress() || address.getHostAddress().startsWith("224.")
                    || address.getHostAddress().startsWith("240.") || "255.255.255.255".equals(address.getHostAddress())) {
                throw new IllegalArgumentException("Unapproved MCP address class");
            }
            return new RemoteTransportRoute(endpoint, address);
        } catch (UnknownHostException error) {
            throw new IllegalArgumentException("Invalid approved MCP address");
        }
    }

    URI endpoint() {
        return endpoint;
    }

    InetAddress address() {
        return address;
    }

    @Override
    public boolean equals(final Object other) {
        return other instanceof RemoteTransportRoute route && endpoint.equals(route.endpoint) && address.equals(route.address);
    }

    @Override
    public int hashCode() {
        return Objects.hash(endpoint, address);
    }

    @Override
    public String toString() {
        return "RemoteTransportRoute[endpoint=" + endpoint.getHost() + ", address=" + address.getHostAddress() + "]";
    }
}
