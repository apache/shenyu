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

import java.util.Objects;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import io.kubernetes.client.openapi.ApiClient;
import io.kubernetes.client.openapi.ApiResponse;
import okhttp3.Call;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * CRD verification: fail-fast on missing discovery entries, and bundle-version detection
 * feeding the SupportedVersion condition (missing annotation means unsupported).
 */
public final class GatewayApiCrdVerifierTest {

    @Test
    public void testSupportedBundleVersionsDetected() throws Exception {
        ApiClient apiClient = mockApiClient(discovery(true), crd("v1.5.1"));

        GatewayApiCrdVersions versions = GatewayApiCrdVerifier.verify(apiClient);

        Assertions.assertTrue(versions.isSupported());
        Assertions.assertEquals(java.util.List.of("v1.5.1"), versions.detectedVersions());
    }

    @Test
    public void testMissingBundleVersionAnnotationIsUnsupported() throws Exception {
        ApiClient apiClient = mockApiClient(discovery(true), crd(null));

        GatewayApiCrdVersions versions = GatewayApiCrdVerifier.verify(apiClient);

        Assertions.assertFalse(versions.isSupported());
        Assertions.assertTrue(versions.detectedVersions().isEmpty());
    }

    @Test
    public void testMissingRequiredResourceFailsFast() throws Exception {
        ApiClient apiClient = mockApiClient(discovery(false), crd("v1.5.1"));

        Assertions.assertThrows(IllegalStateException.class, () -> GatewayApiCrdVerifier.verify(apiClient));
    }

    private JsonObject discovery(final boolean complete) {
        JsonArray resources = new JsonArray();
        resources.add(resource("gatewayclasses"));
        resources.add(resource("gateways"));
        resources.add(resource("httproutes"));
        if (complete) {
            resources.add(resource("referencegrants"));
        }
        JsonObject discovery = new JsonObject();
        discovery.add("resources", resources);
        return discovery;
    }

    private JsonObject resource(final String name) {
        JsonObject resource = new JsonObject();
        resource.addProperty("name", name);
        resource.addProperty("verbs", "get");
        return resource;
    }

    /** A CRD object carrying (or missing) the bundle-version annotation. */
    private JsonObject crd(final String bundleVersion) {
        JsonObject metadata = new JsonObject();
        if (Objects.nonNull(bundleVersion)) {
            JsonObject annotations = new JsonObject();
            annotations.addProperty(GatewayApiCrdVerifier.BUNDLE_VERSION_ANNOTATION, bundleVersion);
            metadata.add("annotations", annotations);
        }
        JsonObject crd = new JsonObject();
        crd.add("metadata", metadata);
        return crd;
    }

    /**
     * The first API call is the discovery document; every later call returns the same CRD
     * object for all four required CRDs.
     */
    @SuppressWarnings("unchecked")
    private ApiClient mockApiClient(final JsonObject discovery, final JsonObject crd) throws Exception {
        ApiClient apiClient = mock(ApiClient.class);
        when(apiClient.getAuthentications()).thenReturn(Map.of());
        when(apiClient.buildCall(any(), any(), any(), any(), any(), any(), any(), any(), any(), any()))
                .thenReturn(mock(Call.class));

        ApiResponse<JsonObject> discoveryResponse = mock(ApiResponse.class);
        when(discoveryResponse.getData()).thenReturn(discovery);
        ApiResponse<JsonObject> crdResponse = mock(ApiResponse.class);
        when(crdResponse.getData()).thenReturn(crd);
        when(apiClient.<JsonObject>execute(any(Call.class), eq(JsonObject.class)))
                .thenReturn(discoveryResponse, crdResponse, crdResponse, crdResponse, crdResponse);
        return apiClient;
    }
}
