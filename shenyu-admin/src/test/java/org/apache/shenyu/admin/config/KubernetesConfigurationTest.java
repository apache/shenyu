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
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.autoconfigure.context.ConfigurationPropertiesAutoConfiguration;
import org.springframework.boot.test.context.assertj.AssertableApplicationContext;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.function.Supplier;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

/**
 * Bean-condition tests for the kubernetes scaling configuration (#7912).
 * The client's back-off condition must key on the {@link AppsV1Api} bean
 * type, not on the always-present {@link DeploymentProperties} bean.
 */
public final class KubernetesConfigurationTest {

    /**
     * A throwaway self-signed certificate: the client parses the CA when it is
     * created, so the tests need real PEM data rather than placeholder bytes.
     */
    private static final String SELF_SIGNED_CA = "-----BEGIN CERTIFICATE-----\n"
            + "MIIDEzCCAfugAwIBAgIUH4u8FdeymXrwh9z0yN6dp2sem34wDQYJKoZIhvcNAQEL\n"
            + "BQAwGTEXMBUGA1UEAwwOc2hlbnl1LXRlc3QtY2EwHhcNMjYxMDIxMTMyODM5WhcN\n"
            + "MzYxMDE4MTMyODM5WjAZMRcwFQYDVQQDDA5zaGVueXUtdGVzdC1jYTCCASIwDQYJ\n"
            + "KoZIhvcNAQEBBQADggEPADCCAQoCggEBALbaq3kRwM5qaRiksHzGiJcFXk8WZg13\n"
            + "wN0tZA4feGXfTXrnKfDGuCtDs6eVaboXB0JGUS/zuWSeL7bjmZQzGvF2NaO9rWj8\n"
            + "eI9NvxoRZERMqOae5m+2ZHtFUqThjzXNDVLSmbd71z3i1eXPmHvIk2BM+fGdiEuL\n"
            + "09Jhq2sX6/T2/lUeGLxKJHop2gB5L3SQoNS23keEeJbxYw7oH/IZNStT6zfkeMn8\n"
            + "YNNVjXPpIsWLkDKMDS7ltxsIvtNhE+uQkpm6EID/XNPvJpsppCSv8g8oiNXOUZMV\n"
            + "dN81WkAUAAteIktw1GSc4tm37QExCUy6d3KX8XWYhqWPwtRoCbcGP7sCAwEAAaNT\n"
            + "MFEwHQYDVR0OBBYEFPELa31848xyVMlVgjaLYqrLK+9EMB8GA1UdIwQYMBaAFPEL\n"
            + "a31848xyVMlVgjaLYqrLK+9EMA8GA1UdEwEB/wQFMAMBAf8wDQYJKoZIhvcNAQEL\n"
            + "BQADggEBAEb70NyFvDOHWUb+UMZdjF+AQ5MZpVIS4vqBDGMTpI56CfIt13J0vjUZ\n"
            + "SOs9zZ2bBwtbhfWNW6+c/i66CDONdXLDoYktLy5dFoHbjDUx+78QpWXh2vBPrr9i\n"
            + "lVp5TP2B654JMp0cwzS1TmpsBMU5lM1vUhwtxPmClYURJ2M6/nTOg2bHT2/PXUAe\n"
            + "QDS9WvENlLMTT+rPVrAP2q1ljKUVYQwnYYd9Z48dYPDvLzIFFKgXINWXoH35daWu\n"
            + "HTNnLozedrj3zlU8G3ubEOzmkdapDz7R3zd5ohqlF65c05NDUB4o6I22uOJMpRJ6\n"
            + "p07cnl7D8goTqlxI+j6CW0MZui9vmf8=\n"
            + "-----END CERTIFICATE-----\n";

    @TempDir
    private Path tempDir;

    private final ApplicationContextRunner runner = new ApplicationContextRunner()
            // @ConfigurationProperties binding needs the post-processor, which a plain
            // runner does not register (a real Boot app always has it)
            .withConfiguration(AutoConfigurations.of(ConfigurationPropertiesAutoConfiguration.class))
            .withUserConfiguration(KubernetesConfiguration.class);

    @Test
    public void shouldCreateClientWithStandardBindingWhenEnabled() {
        runner.withPropertyValues(enabledProps())
                .run(context -> assertHasSingleClient(context, "the api client must be created when scaling is enabled"));
    }

    /**
     * The prefix still binds with only the class-level annotation on the way: the
     * bound values are what the api client is built from, so an unbound bean would
     * surface as a null api server.
     */
    @Test
    public void shouldBindDeploymentPropertiesFromTheConfiguredPrefix() {
        runner.withPropertyValues(enabledProps())
                .run(context -> {
                    final DeploymentProperties properties = context.getBean(DeploymentProperties.class);
                    assertEquals("shenyu-bootstrap", properties.getName());
                    assertEquals("https://127.0.0.1:6443", properties.getApiServer());
                    assertEquals("token", properties.getToken());
                });
    }

    /**
     * The reported configuration: a deployment registers its own properties bean.
     * The bean name is deliberately not {@code deploymentProperties}, so the
     * argument can only be resolved by type — a name coincidence must not be what
     * keeps the context from failing (#7912).
     */
    @Test
    public void shouldCreateClientEvenWhenPropertiesBeanIsPreRegistered() {
        runner.withPropertyValues(enabledProps())
                .withBean("preRegisteredProperties", DeploymentProperties.class,
                        (Supplier<DeploymentProperties>) this::deploymentProperties)
                .run(context -> assertHasSingleClient(context,
                        "a pre-registered DeploymentProperties bean must not suppress the api client (#7912)"));
    }

    /**
     * A user-supplied properties bean overrides the bound one and is the one the
     * client is built from.
     */
    @Test
    public void shouldUseThePreRegisteredProperties() {
        final DeploymentProperties custom = deploymentProperties();
        custom.setApiServer("https://pre-registered:6443");
        runner.withPropertyValues(enabledProps())
                .withBean("preRegisteredProperties", DeploymentProperties.class, () -> custom)
                .run(context -> {
                    assertHasSingleClient(context, "the api client must be built from the override");
                    assertSame(custom, context.getBean(DeploymentProperties.class),
                            "the user-supplied properties bean must win the back-off");
                });
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

    private void assertHasSingleClient(final AssertableApplicationContext context, final String message) {
        assertEquals(1, context.getBeansOfType(AppsV1Api.class).size(), message);
    }

    private String[] enabledProps() {
        return new String[]{
                "shenyu.k8s.scale.enabled=true",
                "shenyu.k8s.deployment.name=shenyu-bootstrap",
                "shenyu.k8s.deployment.namespace=shenyu",
                "shenyu.k8s.deployment.api-server=https://127.0.0.1:6443",
                "shenyu.k8s.deployment.token=token",
                "shenyu.k8s.deployment.ca-cert-path=" + certFile(),
        };
    }

    private String certFile() {
        try {
            final Path cert = tempDir.resolve("ca.crt");
            if (Files.notExists(cert)) {
                Files.write(cert, SELF_SIGNED_CA.getBytes(StandardCharsets.UTF_8));
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
