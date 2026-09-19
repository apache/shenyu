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
import io.kubernetes.client.openapi.ApiClient;
import io.kubernetes.client.openapi.ApiException;
import io.kubernetes.client.openapi.ApiResponse;
import okhttp3.Call;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * Startup probe for the Gateway API CRDs. All four watched resources must be served at
 * {@code gateway.networking.k8s.io/v1}; note ReferenceGrant only serves v1 since Gateway
 * API v1.5.0. Missing CRDs make the dynamic informers fail with 404 forever and degrade
 * the controller silently (e.g. every cross-namespace reference denied), so the controller
 * fails fast with an actionable message instead.
 */
public final class GatewayApiCrdVerifier {

    private static final Logger LOG = LoggerFactory.getLogger(GatewayApiCrdVerifier.class);

    private static final Set<String> REQUIRED_RESOURCES = Set.of(
            "gatewayclasses", "gateways", "httproutes", "referencegrants");

    private GatewayApiCrdVerifier() {
    }

    /**
     * Verify that every required resource is served at gateway.networking.k8s.io/v1.
     *
     * @param apiClient the Kubernetes API client
     * @throws IllegalStateException when any required resource is missing
     */
    public static void verify(final ApiClient apiClient) {
        String path = "/apis/" + GatewayApiConstants.GATEWAY_API_GROUP + "/" + GatewayApiConstants.GATEWAY_API_VERSION;
        final JsonObject resourceList;
        try {
            Map<String, String> headerParams = new HashMap<>();
            headerParams.put("Accept", "application/json");
            String[] authNames = apiClient.getAuthentications().keySet().toArray(new String[0]);
            // buildCall dereferences cookieParams unconditionally, so it must be non-null
            Call call = apiClient.buildCall(path, "GET", null, null, null, headerParams,
                    new HashMap<>(), null, authNames, null);
            ApiResponse<JsonObject> response = apiClient.execute(call, JsonObject.class);
            resourceList = response.getData();
        } catch (ApiException e) {
            throw new IllegalStateException("Gateway API is not available at " + path + " (HTTP " + e.getCode()
                    + "). Install the Gateway API standard channel CRDs (>= v1.5.0) or disable shenyu.k8s.mode=gateway-api.", e);
        }
        Set<String> served = new HashSet<>();
        JsonArray resources = JsonFields.getJsonArray(resourceList, "resources");
        if (Objects.nonNull(resources)) {
            for (JsonElement element : resources) {
                if (element.isJsonObject()) {
                    String name = JsonFields.getString(element.getAsJsonObject(), "name");
                    if (Objects.nonNull(name) && !name.contains("/")) {
                        served.add(name);
                    }
                }
            }
        }
        Set<String> missing = new HashSet<>(REQUIRED_RESOURCES);
        missing.removeAll(served);
        if (!missing.isEmpty()) {
            throw new IllegalStateException("Gateway API CRDs " + missing + " are not served at "
                    + GatewayApiConstants.GATEWAY_API_GROUP + "/" + GatewayApiConstants.GATEWAY_API_VERSION
                    + ". Install the standard channel CRDs (>= v1.5.0; referencegrants serve v1 only since then)"
                    + " or disable shenyu.k8s.mode=gateway-api.");
        }
        LOG.info("Gateway API CRDs verified: all required resources are served at {}/{}",
                GatewayApiConstants.GATEWAY_API_GROUP, GatewayApiConstants.GATEWAY_API_VERSION);
    }
}
