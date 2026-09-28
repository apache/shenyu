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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.TreeSet;

/**
 * Startup probe for the Gateway API CRDs: all four resources must serve
 * {@code gateway.networking.k8s.io/v1} (ReferenceGrant only since v1.5.0) or the controller
 * fails fast — missing CRDs make informers 404 forever and silently degrade. Also reads
 * each CRD's bundle-version annotation for the SupportedVersion condition; an unreadable,
 * unannotated or unrecognized version only degrades that condition, never startup.
 */
public final class GatewayApiCrdVerifier {

    private static final Logger LOG = LoggerFactory.getLogger(GatewayApiCrdVerifier.class);

    private static final Set<String> REQUIRED_RESOURCES = Set.of(
            "gatewayclasses", "gateways", "httproutes", "referencegrants");

    private static final String CRD_API_PATH = "/apis/apiextensions.k8s.io/v1/customresourcedefinitions/";

    /** Annotation stamped on every Gateway API CRD by the official release bundles. */
    public static final String BUNDLE_VERSION_ANNOTATION = "gateway.networking.k8s.io/bundle-version";

    private GatewayApiCrdVerifier() {
    }

    /** Fails fast on missing resources; returns the detected bundle versions. */
    public static GatewayApiCrdVersions verify(final ApiClient apiClient) {
        String path = "/apis/" + GatewayApiConstants.GATEWAY_API_GROUP + "/" + GatewayApiConstants.GATEWAY_API_VERSION;
        final JsonObject resourceList;
        try {
            resourceList = fetchJson(apiClient, path);
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
        return detectBundleVersions(apiClient);
    }

    private static JsonObject fetchJson(final ApiClient apiClient, final String path) throws ApiException {
        Map<String, String> headerParams = new HashMap<>();
        headerParams.put("Accept", "application/json");
        String[] authNames = apiClient.getAuthentications().keySet().toArray(new String[0]);
        // buildCall dereferences cookieParams unconditionally, so it must be non-null
        Call call = apiClient.buildCall(path, "GET", null, null, null, headerParams,
                new HashMap<>(), null, authNames, null);
        ApiResponse<JsonObject> response = apiClient.execute(call, JsonObject.class);
        return response.getData();
    }

    /**
     * Read the bundle-version annotation off every required CRD. Fetch failures degrade to
     * an unsupported verdict instead of failing startup.
     */
    private static GatewayApiCrdVersions detectBundleVersions(final ApiClient apiClient) {
        Set<String> detected = new TreeSet<>();
        boolean supported = true;
        for (String resource : new TreeSet<>(REQUIRED_RESOURCES)) {
            String crdName = resource + "." + GatewayApiConstants.GATEWAY_API_GROUP;
            String version = null;
            try {
                JsonObject crd = fetchJson(apiClient, CRD_API_PATH + crdName);
                JsonObject metadata = JsonFields.getJsonObject(crd, "metadata");
                JsonObject annotations = JsonFields.getJsonObject(metadata, "annotations");
                version = JsonFields.getString(annotations, BUNDLE_VERSION_ANNOTATION);
            } catch (ApiException e) {
                LOG.warn("Could not read CRD {} to detect its bundle version (HTTP {}); marking CRD versions unsupported",
                        crdName, e.getCode());
            }
            int[] majorMinor = GatewayApiCrdVersions.majorMinor(version);
            if (Objects.isNull(version) || Objects.isNull(majorMinor) || !GatewayApiCrdVersions.atLeastMinSupported(majorMinor)) {
                supported = false;
            }
            if (Objects.nonNull(version)) {
                detected.add(version);
            }
        }
        LOG.info("Gateway API CRD bundle versions detected: {} (supported: {})", detected, supported);
        return new GatewayApiCrdVersions(new ArrayList<>(detected), supported);
    }
}
