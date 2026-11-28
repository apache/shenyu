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

package org.apache.shenyu.springboot.starter.client.common.config;

import com.google.gson.JsonObject;
import org.apache.shenyu.client.core.register.ClientApiRefreshedEventListener;
import org.apache.shenyu.client.core.register.ShenyuClientRegisterRepositoryFactory;
import org.apache.shenyu.client.core.utils.PortUtils;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.config.ShenyuClientConfig;
import org.apache.shenyu.register.common.config.ShenyuDiscoveryConfig;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.MockedStatic;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.env.YamlPropertySourceLoader;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.PropertySource;
import org.springframework.core.io.ByteArrayResource;

import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Objects;
import java.util.stream.StreamSupport;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;

/** Bound configuration, default resolution and validation ownership. */
class ClientRegistrationValidatorTest {

    private final ApplicationContextRunner runner = new ApplicationContextRunner()
            .withConfiguration(AutoConfigurations.of(ShenyuClientCommonBeanConfiguration.class))
            .withUserConfiguration(Binding.class)
            .withBean("httpValidator", ClientRegistrationValidator.class, () -> new ClientRegistrationValidator(RpcTypeEnum.HTTP))
            .withPropertyValues("shenyu.register.register-type=http", "shenyu.register.server-lists=http://localhost:9095",
                    "shenyu.client.http.props.appName=fixture", "shenyu.client.http.props.port=8080");

    @ParameterizedTest
    @CsvSource({"shenyu.register.register-type=,shenyu.register.registerType",
        "shenyu.namespace=one;;two,shenyu.namespace", "shenyu.namespace=;,shenyu.namespace",
        "shenyu.register.props.shutdownWaitTime=2147483648,shenyu.register.props.shutdownWaitTime",
        "shenyu.register.props.delayOtherHooksExecTime=secret-fixture,shenyu.register.props.delayOtherHooksExecTime"})
    void testInvalidCommonInputsDoNotAllocateResources(final String setting, final String property) {
        try (MockedStatic<ShenyuClientRegisterRepositoryFactory> factory = mockStatic(ShenyuClientRegisterRepositoryFactory.class)) {
            runner.withPropertyValues(setting).run(context -> {
                assertThat(context).hasFailed();
                assertThat(context.getStartupFailure()).hasStackTraceContaining(property);
                assertThat(org.apache.commons.lang3.exception.ExceptionUtils.getStackTrace(context.getStartupFailure())).doesNotContain("secret-fixture");
                factory.verifyNoInteractions();
            });
        }
    }

    @Test
    void testDisabledRegistrationIgnoresInvalidInputs() {
        try (MockedStatic<ShenyuClientRegisterRepositoryFactory> factory = mockStatic(ShenyuClientRegisterRepositoryFactory.class)) {
            runner.withPropertyValues("shenyu.register.enabled=false", "shenyu.register.server-lists=secret-fixture",
                    "shenyu.client.http.props.port=invalid").run(context -> {
                        assertThat(context).hasNotFailed().doesNotHaveBean(ShenyuClientRegisterRepository.class);
                        factory.verifyNoInteractions();
                    });
        }
    }

    @Test
    void testInactiveProtocolsAndCustomPropertiesAreNotValidated() {
        valid(runner.withPropertyValues("shenyu.client.grpc.props.port=invalid", "shenyu.client.grpc.props.appName=unused",
                "shenyu.client.http.props.customRetryOption=opaque", "shenyu.register.props.sessionTimeout=extension-owned"));
    }

    @Test
    void testCustomTransportRetainsOpaqueAddresses() {
        valid(runner.withPropertyValues("shenyu.register.register-type=custom", "shenyu.register.server-lists=opaque:extension-owned"));
    }

    @ParameterizedTest
    @ValueSource(strings = {"", " ", "one;two"})
    void testNamespaceDefaultsAndMultipleNamespacesRemainValid(final String namespace) {
        valid(runner.withPropertyValues("shenyu.namespace=" + namespace));
    }

    @ParameterizedTest
    @ValueSource(strings = {"-1", "0", "1", "65535"})
    void testAutomaticInputsAndPortBoundariesDoNotResolveServerEarly(final String port) {
        try (MockedStatic<PortUtils> ports = mockStatic(PortUtils.class)) {
            valid(runner.withPropertyValues("server.port=0", "shenyu.client.http.props.port=" + port));
            ports.verifyNoInteractions();
        }
    }

    @Test
    void testProtocolDeclarationSelectsOnlyItsOwnSettings() {
        ApplicationContextRunner grpc = new ApplicationContextRunner()
                .withConfiguration(AutoConfigurations.of(ShenyuClientCommonBeanConfiguration.class))
                .withUserConfiguration(Binding.class)
                .withBean("grpcValidator", ClientRegistrationValidator.class, () -> new ClientRegistrationValidator(RpcTypeEnum.GRPC))
                .withPropertyValues("shenyu.register.register-type=http", "shenyu.register.server-lists=https://localhost:9095",
                        "shenyu.client.grpc.props.contextPath=/grpc", "shenyu.client.grpc.props.port=8181",
                        "shenyu.client.grpc.props.ipAndPort=localhost:8181", "shenyu.client.http.props.port=invalid");
        valid(grpc);
    }

    @Test
    void testOptInHttpRetainsContextOnlyIdentity() {
        valid(runner.withPropertyValues("shenyu.client.http.props.appName=", "shenyu.client.http.props.contextPath=/context-only")
                .withBean("apiListener", ClientApiRefreshedEventListener.class, () -> new ClientApiRefreshedEventListener(List.of(), context -> List.of())));
    }

    @ParameterizedTest
    @CsvSource({"zookeeper,maxRetries,-1", "zookeeper,sessionTimeout,invalid", "zookeeper,baseSleepTime,2147483648",
        "etcd,etcdTimeout,invalid", "etcd,etcdTTL,0", "etcd,etcdTTL,9223372036854775808"})
    void testEnabledDiscoveryValidatesItsConsumedNumbers(final String type, final String key, final String value) {
        try (MockedStatic<ShenyuClientRegisterRepositoryFactory> factory = mockStatic(ShenyuClientRegisterRepositoryFactory.class)) {
            runner.withPropertyValues("shenyu.discovery.enable=true", "shenyu.discovery.register=true",
                    "shenyu.discovery.type=" + type, "shenyu.discovery.server-list=http://localhost:2379",
                    "shenyu.discovery.props[" + key + "]=" + value).run(context -> {
                        assertThat(context).hasFailed();
                        assertThat(context.getStartupFailure()).hasStackTraceContaining("shenyu.discovery.props." + key);
                        factory.verifyNoInteractions();
                    });
        }
    }

    @Test
    void testDisabledDiscoveryDoesNotValidateProviderSettings() {
        valid(runner.withPropertyValues("shenyu.discovery.enable=false", "shenyu.discovery.register=true",
                "shenyu.discovery.type=zookeeper", "shenyu.discovery.props.maxRetries=invalid"));
    }

    @Test
    void testDiscoveryOnlyMetadataDoesNotValidateProviderRuntimeProperties() {
        valid(runner.withPropertyValues("shenyu.discovery.enable=true", "shenyu.discovery.type=zookeeper",
                "shenyu.discovery.server-list=localhost:2181", "shenyu.discovery.props.maxRetries=invalid"));
    }

    @Test
    void testYamlAndMetadataDescribeActualCaseSensitiveProperties() throws Exception {
        String yaml = "shenyu:\n  register:\n    register-type: http\n    server-lists: https://localhost:9095\n"
                + "    props:\n      shutdownWaitTime: 0\n  namespace: one;two\n  client:\n    http:\n      props:\n"
                + "        appName: yaml-fixture\n        contextPath: /yaml\n        port: 8181\n        protocol: https://\n"
                + "  discovery:\n    enable: true\n    register: true\n    type: zookeeper\n    server-list: localhost:2181\n    props:\n      maxRetries: 0\n";
        PropertySource<?> source = new YamlPropertySourceLoader().load("fixture", new ByteArrayResource(yaml.getBytes(StandardCharsets.UTF_8))).get(0);
        try (MockedStatic<ShenyuClientRegisterRepositoryFactory> factory = mockStatic(ShenyuClientRegisterRepositoryFactory.class)) {
            factory.when(() -> ShenyuClientRegisterRepositoryFactory.newInstance(any())).thenReturn(mock(ShenyuClientRegisterRepository.class));
            runner.withInitializer(context -> context.getEnvironment().getPropertySources().addFirst(source)).run(context -> {
                assertThat(context).hasNotFailed();
                ShenyuClientConfig config = context.getBean(ShenyuClientConfig.class);
                assertThat(config.getNamespace()).isEqualTo("one;two");
                assertThat(config.getClient().get("http").getProps()).containsEntry("appName", "yaml-fixture").containsEntry("contextPath", "/yaml")
                        .containsEntry("port", "8181").containsEntry("protocol", "https://");
                assertThat(context.getBean(ShenyuDiscoveryConfig.class).getServerList()).isEqualTo("localhost:2181");
                factory.verify(() -> ShenyuClientRegisterRepositoryFactory.newInstance(any()), times(1));
            });
        }
        try (InputStreamReader reader = new InputStreamReader(Objects.requireNonNull(
                getClass().getResourceAsStream("/META-INF/spring-configuration-metadata.json")), StandardCharsets.UTF_8)) {
            JsonObject metadata = GsonUtils.getGson().fromJson(reader, JsonObject.class);
            List<String> names = StreamSupport.stream(metadata.getAsJsonArray("properties").spliterator(), false)
                    .map(item -> item.getAsJsonObject().get("name").getAsString()).toList();
            assertThat(names).contains("shenyu.client.http.props.appName", "shenyu.client.http.props.contextPath", "shenyu.client.http.props.protocol", "shenyu.register.props.shutdownWaitTime");
            assertThat(names).doesNotContain("shenyu.client.http.props.app-name");
        }
    }

    @ParameterizedTest
    @CsvSource({"zookeeper,,shenyu.discovery.serverList", ",localhost:2181,shenyu.discovery.type"})
    void testActiveDiscoveryRequiresApplicableSettings(final String type, final String servers, final String property) {
        try (MockedStatic<ShenyuClientRegisterRepositoryFactory> factory = mockStatic(ShenyuClientRegisterRepositoryFactory.class)) {
            runner.withPropertyValues("shenyu.discovery.enable=true", "shenyu.discovery.register=true",
                    "shenyu.discovery.type=" + Objects.toString(type, ""),
                    "shenyu.discovery.server-list=" + Objects.toString(servers, "")).run(context -> {
                        assertThat(context).hasFailed();
                        assertThat(context.getStartupFailure()).hasStackTraceContaining(property);
                        factory.verifyNoInteractions();
                    });
        }
    }

    @Test
    void testCustomDiscoveryPropertiesRemainExtensionOwned() {
        valid(runner.withPropertyValues("shenyu.discovery.enable=true", "shenyu.discovery.register=true",
                "shenyu.discovery.type=custom", "shenyu.discovery.props.maxRetries=opaque"));
    }

    @Test
    void testMalformedDiscoveryFlagHasSafeDiagnostic() {
        try (MockedStatic<ShenyuClientRegisterRepositoryFactory> factory = mockStatic(ShenyuClientRegisterRepositoryFactory.class)) {
            runner.withPropertyValues("shenyu.discovery.enable=true", "shenyu.discovery.register=secret-fixture").run(context -> {
                assertThat(context).hasFailed();
                assertThat(context.getStartupFailure()).hasStackTraceContaining("shenyu.discovery.register");
                assertThat(org.apache.commons.lang3.exception.ExceptionUtils.getStackTrace(context.getStartupFailure())).doesNotContain("secret-fixture");
                factory.verifyNoInteractions();
            });
        }
    }

    @Test
    void testOptInBlankAppNameIsNotSilentlyReplacedBySpringName() {
        try (MockedStatic<ShenyuClientRegisterRepositoryFactory> factory = mockStatic(ShenyuClientRegisterRepositoryFactory.class)) {
            runner.withPropertyValues("shenyu.client.http.props.appName=", "spring.application.name=present")
                    .withBean("apiListener", ClientApiRefreshedEventListener.class,
                            () -> new ClientApiRefreshedEventListener(List.of(), context -> List.of())).run(context -> {
                                assertThat(context).hasFailed();
                                assertThat(context.getStartupFailure()).hasStackTraceContaining("shenyu.client.http.props.appName");
                                factory.verifyNoInteractions();
                            });
        }
    }

    private void valid(final ApplicationContextRunner application) {
        try (MockedStatic<ShenyuClientRegisterRepositoryFactory> factory = mockStatic(ShenyuClientRegisterRepositoryFactory.class)) {
            factory.when(() -> ShenyuClientRegisterRepositoryFactory.newInstance(any())).thenReturn(mock(ShenyuClientRegisterRepository.class));
            application.run(context -> {
                assertThat(context).hasNotFailed();
                factory.verify(() -> ShenyuClientRegisterRepositoryFactory.newInstance(any()), times(1));
            });
        }
    }

    @Configuration(proxyBeanMethods = false)
    @EnableConfigurationProperties
    static class Binding {
    }
}
