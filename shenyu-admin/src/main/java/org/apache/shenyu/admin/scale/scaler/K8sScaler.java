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

package org.apache.shenyu.admin.scale.scaler;

import io.kubernetes.client.openapi.ApiClient;
import io.kubernetes.client.openapi.apis.AppsV1Api;
import io.kubernetes.client.openapi.models.V1Scale;

import java.util.Objects;

public class K8sScaler {
    private final ApiClient apiClient;
    private final AppsV1Api appsV1Api;

    public K8sScaler(ApiClient apiClient) {
        this.apiClient = apiClient;
        this.appsV1Api = new AppsV1Api(apiClient);
    }

    public void scale(ScaleAction action) throws Exception {
        // 获取当前 Deployment 的 Scale 对象
        V1Scale scale = appsV1Api.readNamespacedDeploymentScale("shenyu-gateway", "default", null);

        // 修改副本数
        Objects.requireNonNull(scale.getSpec()).setReplicas(action.getReplicaCount());

        // 更新 Scale 对象
        appsV1Api.replaceNamespacedDeploymentScale("shenyu-gateway", "default", scale, null, null, null, null);
    }
}
