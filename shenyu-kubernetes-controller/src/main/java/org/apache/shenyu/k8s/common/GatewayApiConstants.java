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
import io.kubernetes.client.util.generic.dynamic.DynamicKubernetesObject;

import java.util.Objects;

public final class GatewayApiConstants {

    public static final String GATEWAY_API_GROUP = "gateway.networking.k8s.io";

    public static final String GATEWAY_API_VERSION = "v1";

    public static final String GATEWAY_KIND = "Gateway";

    public static final String HTTP_ROUTE_KIND = "HTTPRoute";

    public static final String SERVICE_KIND = "Service";

    /** The core (legacy) API group, named by the empty string; group of Service. */
    public static final String CORE_API_GROUP = "";

    /** ResolvedRefs=False reason: a backendRef's Service has no ready endpoints. */
    public static final String REASON_BACKEND_NOT_FOUND = "BackendNotFound";

    /** ResolvedRefs=False / Accepted=False reason: a cross-namespace reference (parentRef or backendRef) is not permitted by a ReferenceGrant or listener policy. */
    public static final String REASON_REF_NOT_PERMITTED = "RefNotPermitted";

    /** ResolvedRefs=False reason: a backendRef's kind is not Service, the only supported kind. */
    public static final String REASON_INVALID_KIND = "InvalidKind";

    /** Condition type Accepted. */
    public static final String CONDITION_ACCEPTED = "Accepted";

    /** Condition type ResolvedRefs. */
    public static final String CONDITION_RESOLVED_REFS = "ResolvedRefs";

    /** Condition type Programmed. */
    public static final String CONDITION_PROGRAMMED = "Programmed";

    /** Accepted=False reason: parentRef does not resolve to an existing Gateway. */
    public static final String REASON_NO_MATCHING_PARENT = "NoMatchingParent";

    /** Accepted=False reason: no listener hostname intersects the route hostnames. */
    public static final String REASON_NO_MATCHING_LISTENER_HOSTNAME = "NoMatchingListenerHostname";

    /** Condition reason: a spec value (filter, sectionName, ...) is unsupported. */
    public static final String REASON_UNSUPPORTED_VALUE = "UnsupportedValue";

    /** Listener Accepted=False reason: protocol is not supported (only HTTP is served). */
    public static final String REASON_UNSUPPORTED_PROTOCOL = "UnsupportedProtocol";

    /** Listener Accepted=False reason: port is not served by this gateway. */
    public static final String REASON_PORT_UNAVAILABLE = "PortUnavailable";

    /** Condition reason: programmed into the data plane. */
    public static final String REASON_PROGRAMMED = "Programmed";

    /** Gateway Programmed=False reason: no listener with a supported protocol and port. */
    public static final String REASON_LISTENERS_NOT_VALID = "ListenersNotValid";

    /** The only listener protocol this controller serves. */
    public static final String PROTOCOL_HTTP = "HTTP";

    public static final String SHENYU_CONTROLLER_NAME = "gateway.shenyu.apache.org/shenyu-controller";

    private GatewayApiConstants() {
    }

    /**
     * Whether a backendRef references a Service: {@code kind} defaults to Service when
     * absent, the only kind ShenYu can resolve to upstream addresses.
     *
     * @param backendRef the backendRef object
     * @return true if the reference targets a Service
     */
    public static boolean isServiceRef(final JsonObject backendRef) {
        String kind = JsonFields.getString(backendRef, "kind");
        return Objects.isNull(kind) || SERVICE_KIND.equals(kind);
    }

    /**
     * Check if a Gateway API resource has a specific condition with status "True".
     *
     * @param resource the dynamic kubernetes object
     * @param conditionType the condition type to check
     * @return true if the condition exists and is "True"
     */
    public static boolean isConditionTrue(final DynamicKubernetesObject resource, final String conditionType) {
        JsonObject raw = resource.getRaw();
        if (!raw.has("status") || raw.get("status").isJsonNull()) {
            return false;
        }
        JsonObject status = raw.getAsJsonObject("status");
        if (!status.has("conditions") || status.get("conditions").isJsonNull()) {
            return false;
        }
        JsonArray conditions = status.getAsJsonArray("conditions");
        for (JsonElement element : conditions) {
            JsonObject condition = element.getAsJsonObject();
            String type = condition.has("type") ? condition.get("type").getAsString() : null;
            String value = condition.has("status") ? condition.get("status").getAsString() : null;
            if (conditionType.equals(type) && "True".equals(value)) {
                return true;
            }
        }
        return false;
    }
}
