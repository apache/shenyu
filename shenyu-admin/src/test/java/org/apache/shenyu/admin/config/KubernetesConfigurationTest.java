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

import io.kubernetes.client.openapi.apis.AppsV1Api;
import org.apache.shenyu.admin.config.properties.DeploymentProperties;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.function.Supplier;

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

/**
 * Bean-condition tests for the kubernetes scaling configuration (#7912).
 * The client's back-off condition must key on the {@link AppsV1Api} bean
 * type, not on the always-present {@link DeploymentProperties} bean.
 */
public final class KubernetesConfigurationTest {

    @TempDir
    private Path tempDir;

    private final ApplicationContextRunner runner = new ApplicationContextRunner()
            .withUserConfiguration(KubernetesConfiguration.class);

    @Test
    public void shouldCreateClientWithStandardBindingWhenEnabled() {
        runner.withPropertyValues(enabledProps())
                .run(context -> assertTrue(context.containsBean("apiClient"),
                        "the api client bean must be created when scaling is enabled"));
    }

    @Test
    public void shouldCreateClientEvenWhenPropertiesBeanIsPreRegistered() {
        runner.withPropertyValues(enabledProps())
                .withBean(DeploymentProperties.class, (Supplier<DeploymentProperties>) this::deploymentProperties)
                .run(context -> assertTrue(context.containsBean("apiClient"),
                        "a pre-registered DeploymentProperties bean must not suppress the api client (#7912)"));
    }

    @Test
    public void shouldBackOffWhenACustomClientIsProvided() {
        final AppsV1Api custom = mock(AppsV1Api.class);
        runner.withPropertyValues(enabledProps())
                .withBean("customAppsV1Api", AppsV1Api.class, () -> custom)
                .run(context -> assertSame(custom, context.getBean(AppsV1Api.class),
                        "a user-supplied AppsV1Api must win the back-off"));
    }

    @Test
    public void shouldNotCreateClientWhenScalingIsDisabled() {
        runner.run(context -> assertTrue(context.getBeansOfType(AppsV1Api.class).isEmpty(),
                "no api client may be created when scaling is disabled"));
    }

    private String[] enabledProps() {
        return new String[]{
                "shenyu.k8s.scale.enabled=true",
                "shenyu.k8s.deployment.name=shenyu-bootstrap",
                "shenyu.k8s.deployment.namespace=shenyu",
                "shenyu.k8s.deployment.api-server=https://127.0.0.1:6443",
                "shenyu.k8s.deployment.token=token",
                "shenyu.k8s.deployment.ca-cert-path:" + certFile(),
        };
    }

    private String certFile() {
        try {
            final Path cert = tempDir.resolve("ca.crt");
            if (Files.notExists(cert)) {
                Files.write(cert, new byte[]{1});
            }
            return cert.toString();
        } catch (Exception e) {
            throw new IllegalStateException("cannot prepare the ca cert file", e);
        }
    }

    private DeploymentProperties deploymentProperties() {
        final DeploymentProperties properties = new DeploymentProperties();
        properties.setName("shenyu-bootstrap");
        properties.setNamespace("shenyu");
        properties.setApiServer("https://127.0.0.1:6443");
        properties.setToken("token");
        properties.setCaCertPath(certFile());
        return properties;
    }
}
