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

package org.apache.shenyu.springboot.starter.sdk.feign;

import org.apache.shenyu.registry.api.ShenyuInstanceRegisterRepository;
import org.apache.shenyu.registry.api.config.RegisterConfig;
import org.apache.shenyu.registry.api.entity.InstanceEntity;
import org.apache.shenyu.spi.Join;
import org.junit.jupiter.api.Test;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.autoconfigure.context.ConfigurationPropertiesAutoConfiguration;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;

import static org.assertj.core.api.Assertions.assertThat;

class RegistryClasspathTest {

    private final ApplicationContextRunner runner = new ApplicationContextRunner()
            .withConfiguration(AutoConfigurations.of(ConfigurationPropertiesAutoConfiguration.class, ShenyuSdkAutoConfiguration.class))
            .withPropertyValues("shenyu.sdk.enabled=true", "shenyu.sdk.server-lists=localhost:9195");

    @Test
    void localModeStartsWithoutARegistryImplementation() {
        runner.withPropertyValues("shenyu.sdk.register-type=local").run(context -> {
            assertThat(context).hasNotFailed();
            assertThat(context.getBeanProvider(ShenyuInstanceRegisterRepository.class).getIfAvailable()).isNull();
        });
    }

    @Test
    void missingConfiguredProviderFailsWithDependencyHint() {
        runner.withPropertyValues("shenyu.sdk.register-type=missing").run(context -> {
            assertThat(context).hasFailed();
            assertThat(context.getStartupFailure()).hasRootCauseInstanceOf(IllegalArgumentException.class)
                    .hasStackTraceContaining("shenyu-registry-missing").hasStackTraceContaining("runtime classpath");
        });
    }

    @Test
    void explicitlyProvidedSpiStartsAndReceivesBoundConfiguration() {
        runner.withPropertyValues("shenyu.sdk.register-type=test-sdk-feign").run(context -> {
            assertThat(context).hasNotFailed();
            assertThat(context).hasSingleBean(ShenyuInstanceRegisterRepository.class);
            TestRepository repository = (TestRepository) context.getBean(ShenyuInstanceRegisterRepository.class);
            assertThat(repository.config.getRegisterType()).isEqualTo("test-sdk-feign");
            assertThat(repository.config.getServerLists()).isEqualTo("localhost:9195");
        });
    }

    /**
     * External implementation supplied only on the test runtime classpath.
     */
    @Join(isSingleton = false)
    public static class TestRepository implements ShenyuInstanceRegisterRepository {

        private RegisterConfig config;

        @Override
        public void init(final RegisterConfig registerConfig) {
            config = registerConfig;
        }

        @Override
        public void persistInstance(final InstanceEntity instance) {
        }
    }
}
