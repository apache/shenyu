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

import com.google.gson.JsonObject;
import io.kubernetes.client.openapi.ApiClient;
import io.kubernetes.client.openapi.ApiException;
import okhttp3.Call;

import java.util.HashMap;
import java.util.Map;

/**
 * Merge-patch through the {@link ApiClient}'s request pipeline: a raw okhttp call would
 * miss per-request header auth (static kubeconfig tokens) and only work for client-bound auth.
 */
public final class StatusMergePatch {

    private static final String MERGE_PATCH_CONTENT_TYPE = "application/merge-patch+json";

    private StatusMergePatch() {
    }

    /**
     * Merge-patch the resource at the given path.
     *
     * @param apiClient the Kubernetes API client providing transport and authentication
     * @param path the /status subresource path
     * @param body the patch body
     * @throws ApiException when the API server rejects the patch
     */
    public static void patch(final ApiClient apiClient, final String path, final JsonObject body) throws ApiException {
        Map<String, String> headerParams = new HashMap<>();
        headerParams.put("Accept", "application/json");
        headerParams.put("Content-Type", MERGE_PATCH_CONTENT_TYPE);
        String[] authNames = apiClient.getAuthentications().keySet().toArray(new String[0]);
        // buildCall dereferences cookieParams unconditionally, so it must be non-null
        Call call = apiClient.buildCall(path, "PATCH", null, null, body, headerParams,
                new HashMap<>(), null, authNames, null);
        apiClient.execute(call);
    }
}
