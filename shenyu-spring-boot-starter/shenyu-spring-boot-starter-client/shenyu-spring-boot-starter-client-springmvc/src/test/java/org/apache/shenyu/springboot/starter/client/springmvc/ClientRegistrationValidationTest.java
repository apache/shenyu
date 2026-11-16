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

package org.apache.shenyu.springboot.starter.client.springmvc;

import org.apache.shenyu.client.auto.config.EnableClientRegister;
import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.core.register.ClientRegisterConfig;
import org.apache.shenyu.client.core.register.ShenyuClientRegisterRepositoryFactory;
import org.apache.shenyu.client.core.register.extractor.RpcApiBeansExtractor;
import org.apache.shenyu.client.springmvc.init.SpringMvcClientEventListener;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.MockedStatic;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.context.annotation.Configuration;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;

/** Invalid bound properties must fail before the repository or publisher is touched. */
class ClientRegistrationValidationTest {

    private final ApplicationContextRunner runner = new ApplicationContextRunner()
            .withConfiguration(AutoConfigurations.of(ShenyuSpringMvcClientConfiguration.class))
            .withUserConfiguration(Binding.class)
            .withPropertyValues("shenyu.register.register-type=http", "shenyu.register.server-lists=http://localhost:9095",
                    "spring.application.name=validation-fixture", "shenyu.client.http.props.port=8189");

    @ParameterizedTest
    @ValueSource(strings = {"invalid", "65536", "-2"})
    void testInvalidPortFailsBeforeResources(final String port) {
        assertInvalid("shenyu.client.http.props.port=" + port, "shenyu.client.http.props.port");
    }

    @ParameterizedTest
    @ValueSource(strings = {"", "not-a-url", "http://user:secret-fixture@", "http://localhost:65536", "http://localhost:9095,"})
    void testInvalidHttpServersDoNotLeakValues(final String servers) {
        assertInvalid("shenyu.register.server-lists=" + servers, "shenyu.register.serverLists");
    }

    @Test
    void testInvalidShutdownDurationFailsBeforeResources() {
        assertInvalid("shenyu.register.props.shutdownWaitTime=-1", "shenyu.register.props.shutdownWaitTime");
    }

    @Test
    void testOptInAlsoValidatesBeforeConfigurationConstruction() {
        assertInvalid(runner.withUserConfiguration(OptInBinding.class).withBean(RpcApiBeansExtractor.class, () -> mock(RpcApiBeansExtractor.class)),
                "shenyu.client.http.props.port=65536", "shenyu.client.http.props.port");
    }

    @Test
    void testMissingActiveProtocolMapHasPropertyDiagnostic() {
        ApplicationContextRunner missing = new ApplicationContextRunner()
                .withConfiguration(AutoConfigurations.of(ShenyuSpringMvcClientConfiguration.class))
                .withUserConfiguration(Binding.class)
                .withPropertyValues("shenyu.register.register-type=http", "shenyu.register.server-lists=http://localhost:9095");
        assertInvalid(missing, "spring.application.name=fixture", "shenyu.client.http.props");
    }

    @Test
    void testLegacyFallbackAndBlankNamespaceArePreserved() {
        try (MockedStatic<ShenyuClientRegisterRepositoryFactory> factory = mockStatic(ShenyuClientRegisterRepositoryFactory.class);
                MockedStatic<ShenyuClientRegisterEventPublisher> publishers = mockStatic(ShenyuClientRegisterEventPublisher.class)) {
            factory.when(() -> ShenyuClientRegisterRepositoryFactory.newInstance(any())).thenReturn(mock(ShenyuClientRegisterRepository.class));
            publishers.when(ShenyuClientRegisterEventPublisher::getInstance).thenReturn(mock(ShenyuClientRegisterEventPublisher.class));
            runner.withPropertyValues("shenyu.namespace= ").run(context -> {
                assertThat(context).hasNotFailed();
                SpringMvcClientEventListener listener = context.getBean(SpringMvcClientEventListener.class);
                assertThat(listener.getAppName()).isEqualTo("validation-fixture");
                assertThat(listener.getContextPath()).isEqualTo("/validation-fixture");
                assertThat(context.getBean(ClientRegisterConfig.class).getNamespace()).containsExactly(Constants.SYS_DEFAULT_NAMESPACE_ID);
                assertThat(context.getBean(ClientRegisterConfig.class).getPort()).isEqualTo(8189);
            });
        }
    }

    @Test
    void testDisabledOptInDoesNotConstructPipeline() {
        try (MockedStatic<ShenyuClientRegisterRepositoryFactory> factory = mockStatic(ShenyuClientRegisterRepositoryFactory.class);
                MockedStatic<ShenyuClientRegisterEventPublisher> publishers = mockStatic(ShenyuClientRegisterEventPublisher.class)) {
            runner.withUserConfiguration(OptInBinding.class).withPropertyValues("shenyu.register.enabled=false",
                    "shenyu.client.http.props.port=invalid").run(context -> {
                        assertThat(context).hasNotFailed().doesNotHaveBean("publisher").doesNotHaveBean("apiListener");
                        factory.verifyNoInteractions();
                        publishers.verifyNoInteractions();
                    });
        }
    }

    private void assertInvalid(final String setting, final String property) {
        assertInvalid(runner, setting, property);
    }

    private void assertInvalid(final ApplicationContextRunner application, final String setting, final String property) {
        try (MockedStatic<ShenyuClientRegisterRepositoryFactory> factory = mockStatic(ShenyuClientRegisterRepositoryFactory.class);
                MockedStatic<ShenyuClientRegisterEventPublisher> publishers = mockStatic(ShenyuClientRegisterEventPublisher.class)) {
            factory.when(() -> ShenyuClientRegisterRepositoryFactory.newInstance(any())).thenReturn(mock(ShenyuClientRegisterRepository.class));
            publishers.when(ShenyuClientRegisterEventPublisher::getInstance).thenReturn(mock(ShenyuClientRegisterEventPublisher.class));
            application.withPropertyValues(setting).run(context -> {
                assertThat(context).hasFailed();
                assertThat(context.getStartupFailure()).hasStackTraceContaining(property)
                        .hasStackTraceContaining("expected").hasStackTraceContaining("configure");
                assertThat(context.getStartupFailure()).hasStackTraceContaining("Invalid client registration property");
                assertThat(org.apache.commons.lang3.exception.ExceptionUtils.getStackTrace(context.getStartupFailure())).doesNotContain("secret-fixture");
                factory.verifyNoInteractions();
                publishers.verifyNoInteractions();
            });
        }
    }

    @Configuration(proxyBeanMethods = false)
    @EnableConfigurationProperties
    @EnableClientRegister
    static class OptInBinding {
    }

    @Configuration(proxyBeanMethods = false)
    @EnableConfigurationProperties
    static class Binding {
    }
}
