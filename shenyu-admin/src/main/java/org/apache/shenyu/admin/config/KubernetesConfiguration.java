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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Paths;

@Configuration
public class KubernetesConfiguration {

    private final Logger logger = LoggerFactory.getLogger(KubernetesConfiguration.class);

    /**
     * kubernetes deployment properties. Registered as a conditional bean rather
     * than through {@code @EnableConfigurationProperties}, so a user-supplied
     * {@link DeploymentProperties} bean cleanly wins instead of competing with the
     * one registered here for the {@code apiClient} argument (#7912).
     *
     * @return the deployment properties bound from {@code shenyu.k8s.deployment}
     */
    @Bean
    @ConditionalOnMissingBean
    @ConfigurationProperties(prefix = "shenyu.k8s.deployment")
    public DeploymentProperties deploymentProperties() {
        return new DeploymentProperties();
    }

    /**
     * kubernetes apiClient.
     * @param deploymentProperties deploymentProperties
     * @return AppsV1Api
     */
    @Bean
    @ConditionalOnProperty(value = "shenyu.k8s.scale.enabled", havingValue = "true")
    @ConditionalOnMissingBean(AppsV1Api.class)
    public AppsV1Api apiClient(final DeploymentProperties deploymentProperties) {
        try {
            ApiClient client = Config.fromToken(
                    deploymentProperties.getApiServer(),
                    deploymentProperties.getToken(),
                    // verify the api server certificate against the configured CA
                    true
            );
            try (InputStream caCertStream = Files.newInputStream(Paths.get(deploymentProperties.getCaCertPath()))) {
                // the api client keeps the stream and reads it when it builds the
                // http client, so hand it a re-readable copy instead of a closed file
                client.setSslCaCert(new ByteArrayInputStream(caCertStream.readAllBytes()));
            }
            return new AppsV1Api(client);

        } catch (IOException | RuntimeException e) {
            // an unreachable cluster or an unusable CA must not stop Admin from
            // starting: the scaler degrades to a logged no-op without a client
            logger.error("kubernetes apiClient create error", e);
        }
        return null;
    }

}
