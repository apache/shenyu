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

package org.apache.shenyu.admin.config;

import io.kubernetes.client.openapi.ApiClient;
import io.kubernetes.client.openapi.apis.AppsV1Api;
import io.kubernetes.client.util.Config;
import org.apache.shenyu.admin.config.properties.DeploymentProperties;
import org.apache.shenyu.admin.scale.scaler.KubernetesScaler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.io.FileInputStream;
import java.io.IOException;
import java.util.Optional;

@Configuration
@EnableConfigurationProperties(DeploymentProperties.class)
public class KubernetesConfiguration {

    private final Logger logger = LoggerFactory.getLogger(KubernetesConfiguration.class);

    /**
     * kubernetes apiClient.
     * @param deploymentProperties deploymentProperties
     * @return AppsV1Api
     */
    @Bean
    @ConditionalOnProperty(value = "shenyu.k8s.scale.enabled", havingValue = "true")
    @ConditionalOnMissingBean(DeploymentProperties.class)
    public AppsV1Api apiClient(final DeploymentProperties deploymentProperties) {
        try {
            ApiClient client = Config.fromToken(
                    deploymentProperties.getApiServer(),
                    deploymentProperties.getToken(),
                    false
            );
            client.setSslCaCert(new FileInputStream(deploymentProperties.getCaCertPath()));
            return new AppsV1Api(client);

        } catch (IOException e) {
            logger.error("kubernetes apiClient create error", e);
        }
        return null;
    }

    /**
     * kubernetes scaler.
     * @param appsV1Api appsV1Api
     * @param deploymentProperties deploymentProperties
     * @return KubernetesScaler KubernetesScaler
     */
    @Bean
    @ConditionalOnMissingBean({AppsV1Api.class, DeploymentProperties.class})
    public KubernetesScaler kubernetesScaler(final Optional<AppsV1Api> appsV1Api, final DeploymentProperties deploymentProperties) {
        return new KubernetesScaler(appsV1Api, deploymentProperties);
    }

}
