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
import com.google.gson.JsonObject;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Listener helpers the Gateway status and HTTPRoute attachment both depend on: which protocols can
 * carry an HTTPRoute, and when {@code allowedRoutes.kinds} permits one.
 */
public final class ListenerSupportTest {

    @Test
    public void testSupportsHttpRouteByProtocol() {
        Assertions.assertTrue(ListenerSupport.supportsHttpRoute(listener("HTTP", 9195)));
        Assertions.assertTrue(ListenerSupport.supportsHttpRoute(listener("HTTPS", 443)));
        Assertions.assertFalse(ListenerSupport.supportsHttpRoute(listener("TLS", 443)));
        Assertions.assertFalse(ListenerSupport.supportsHttpRoute(listener("TCP", 9000)));
        Assertions.assertFalse(ListenerSupport.supportsHttpRoute(listener("UDP", 9001)));
        // a schema-invalid listener with no protocol is read as plain HTTP; the comparison is exact-case
        Assertions.assertTrue(ListenerSupport.supportsHttpRoute(new JsonObject()));
        Assertions.assertFalse(ListenerSupport.supportsHttpRoute(listener("http", 9195)));
    }

    @Test
    public void testAllowsKindWithoutDeclaredKinds() {
        Assertions.assertTrue(ListenerSupport.allowsKind(listener("HTTP", 9195)));
        Assertions.assertTrue(ListenerSupport.allowsKind(listener("HTTPS", 443)));
        // kinds are protocol-scoped: these listeners can never carry an HTTPRoute
        Assertions.assertFalse(ListenerSupport.allowsKind(listener("TLS", 443)));
        Assertions.assertFalse(ListenerSupport.allowsKind(listener("TCP", 9000)));
        Assertions.assertFalse(ListenerSupport.allowsKind(listener("UDP", 9001)));
        // a declared but empty kinds list means the same as an absent one
        Assertions.assertTrue(ListenerSupport.allowsKind(withKinds(listener("HTTP", 9195), new JsonArray())));
    }

    @Test
    public void testAllowsKindWithDeclaredKinds() {
        JsonObject bareHttpRouteKind = new JsonObject();
        bareHttpRouteKind.addProperty("kind", "HTTPRoute");
        Assertions.assertTrue(ListenerSupport.allowsKind(withKinds(listener("HTTP", 9195), kinds(bareHttpRouteKind))));
        Assertions.assertTrue(ListenerSupport.allowsKind(withKinds(listener("HTTPS", 443),
                kinds(routeKind(GatewayApiConstants.GATEWAY_API_GROUP, "HTTPRoute")))));
        // a foreign group names a different resource, not an HTTPRoute
        Assertions.assertFalse(ListenerSupport.allowsKind(withKinds(listener("HTTP", 9195),
                kinds(routeKind("example.com", "HTTPRoute")))));
        Assertions.assertFalse(ListenerSupport.allowsKind(withKinds(listener("HTTP", 9195),
                kinds(routeKind(GatewayApiConstants.GATEWAY_API_GROUP, "TLSRoute")))));
        Assertions.assertFalse(ListenerSupport.allowsKind(withKinds(listener("HTTPS", 443),
                kinds(routeKind(GatewayApiConstants.GATEWAY_API_GROUP, "GRPCRoute")))));
        // the protocol gate wins over a declared kind the listener can never carry
        Assertions.assertFalse(ListenerSupport.allowsKind(withKinds(listener("TCP", 9000),
                kinds(routeKind(GatewayApiConstants.GATEWAY_API_GROUP, "HTTPRoute")))));
    }

    @Test
    public void testSelectiveFromIsIndependentOfKindSupport() {
        JsonObject tcpListener = listener("TCP", 9000);
        JsonObject namespaces = new JsonObject();
        namespaces.addProperty("from", "Selector");
        JsonObject allowedRoutes = new JsonObject();
        allowedRoutes.add("namespaces", namespaces);
        tcpListener.add("allowedRoutes", allowedRoutes);
        Assertions.assertTrue(ListenerSupport.usesUnsupportedFrom(tcpListener),
                "the predicate itself carries no protocol gate, so callers must check attachability first");
        Assertions.assertFalse(ListenerSupport.allowsKind(tcpListener));
    }

    @Test
    public void testServableRequiresHttpOnTheServedPort() {
        Assertions.assertTrue(ListenerSupport.isServable(listener("HTTP", 9195), 9195));
        Assertions.assertFalse(ListenerSupport.isServable(listener("HTTP", 80), 9195));
        Assertions.assertFalse(ListenerSupport.isServable(listener("HTTPS", 9195), 9195));
        Assertions.assertTrue(ListenerSupport.supportsHttpRoute(listener("HTTPS", 9195)));
    }

    private JsonObject listener(final String protocol, final int port) {
        JsonObject listener = new JsonObject();
        listener.addProperty("name", "http");
        listener.addProperty("protocol", protocol);
        listener.addProperty("port", port);
        return listener;
    }

    private JsonObject withKinds(final JsonObject listener, final JsonArray kinds) {
        JsonObject allowedRoutes = new JsonObject();
        allowedRoutes.add("kinds", kinds);
        listener.add("allowedRoutes", allowedRoutes);
        return listener;
    }

    private JsonArray kinds(final JsonObject... entries) {
        JsonArray kinds = new JsonArray();
        for (JsonObject entry : entries) {
            kinds.add(entry);
        }
        return kinds;
    }

    private JsonObject routeKind(final String group, final String kind) {
        JsonObject routeKind = new JsonObject();
        routeKind.addProperty("group", group);
        routeKind.addProperty("kind", kind);
        return routeKind;
    }
}
