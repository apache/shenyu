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
import io.kubernetes.client.util.Config;
import org.apache.shenyu.admin.scale.config.DeploymentProperties;
import org.springframework.stereotype.Component;

import java.io.IOException;

@Component
public class K8sApiClientProvider {

    private final DeploymentProperties deploymentProperties;

    public K8sApiClientProvider(final DeploymentProperties deploymentProperties) {
        this.deploymentProperties = deploymentProperties;
    }

    /**
     * createApiClient.
     *
     * @return ApiClient
     */
    public ApiClient createApiClient() throws IOException {
        if (isLocalEnvironment()) {
            return Config.defaultClient();
        } else {
            ApiClient client = Config.fromToken(
                    deploymentProperties.getApiServer(),
                    deploymentProperties.getToken(),
                    false
            );
            client.setSslCaCert(new java.io.FileInputStream(deploymentProperties.getCaCertPath()));
            return client;
        }
    }

    /**
     * isLocalEnvironment.
     *
     * @return boolean
     */
    private boolean isLocalEnvironment() {
        return System.getenv("ENV") == null || "local".equalsIgnoreCase(System.getenv("ENV"));
    }
}