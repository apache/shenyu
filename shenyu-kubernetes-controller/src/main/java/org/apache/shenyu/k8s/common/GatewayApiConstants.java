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

public final class GatewayApiConstants {

    public static final String GATEWAY_API_GROUP = "gateway.networking.k8s.io";

    public static final String GATEWAY_API_VERSION = "v1";

    public static final String GATEWAY_KIND = "Gateway";

    public static final String HTTP_ROUTE_KIND = "HTTPRoute";

    public static final String GRPC_ROUTE_KIND = "GRPCRoute";

    public static final String SHENYU_GATEWAY_CLASS_NAME = "shenyu";

    public static final String SHENYU_CONTROLLER_NAME = "gateway.shenyu.apache.org/shenyu-controller";

    private GatewayApiConstants() {
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
