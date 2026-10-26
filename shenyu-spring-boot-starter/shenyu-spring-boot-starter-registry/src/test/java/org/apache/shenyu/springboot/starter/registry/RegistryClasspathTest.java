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

package org.apache.shenyu.springboot.starter.registry;

import org.apache.shenyu.registry.api.ShenyuInstanceRegisterRepository;
import org.apache.shenyu.registry.api.config.RegisterConfig;
import org.apache.shenyu.registry.api.entity.InstanceEntity;
import org.apache.shenyu.registry.core.ShenyuInstanceRegisterRepositoryFactory;
import org.apache.shenyu.spi.ExtensionLoader;
import org.apache.shenyu.spi.Join;
import org.junit.jupiter.api.Test;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.autoconfigure.context.ConfigurationPropertiesAutoConfiguration;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.boot.web.context.WebServerInitializedEvent;
import org.springframework.boot.web.server.WebServer;

import java.util.Properties;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class RegistryClasspathTest {

    private final ApplicationContextRunner runner = new ApplicationContextRunner()
            .withConfiguration(AutoConfigurations.of(ConfigurationPropertiesAutoConfiguration.class, ShenyuRegistryConfiguration.class))
            .withPropertyValues("shenyu.register.enabled=true", "shenyu.register.server-lists=localhost:9195");

    @Test
    void disabledRegistrationStartsWithoutARegistryConnection() {
        runner.withPropertyValues("shenyu.register.enabled=false").run(context -> {
            assertThat(context).hasNotFailed();
            assertThat(context).doesNotHaveBean(RegistryListener.class);
        });
    }

    @Test
    void compatibilityStarterStillDiscoversItsOriginalProviders() {
        assertThat(ExtensionLoader.getExtensionLoader(ShenyuInstanceRegisterRepository.class).getExtensionClasses1())
                .containsKeys("zookeeper", "etcd", "nacos", "eureka", "kubernetes");
    }

    @Test
    void missingConfiguredProviderFailsWithDependencyHint() {
        runner.withPropertyValues("shenyu.register.register-type=missing").run(context -> {
            assertThat(context).hasFailed();
            assertThat(context.getStartupFailure()).hasRootCauseInstanceOf(IllegalArgumentException.class)
                    .hasStackTraceContaining("shenyu-registry-missing").hasStackTraceContaining("runtime classpath");
        });
    }

    @Test
    void explicitlyProvidedSpiStartsAndReceivesBoundConfiguration() {
        runner.withPropertyValues("shenyu.register.register-type=test-registry").run(context -> {
            assertThat(context).hasNotFailed();
            assertThat(context).hasSingleBean(RegistryListener.class);
            TestRepository repository = (TestRepository) ShenyuInstanceRegisterRepositoryFactory.newInstance("test-registry");
            assertThat(repository.config.getRegisterType()).isEqualTo("test-registry");
            assertThat(repository.config.getServerLists()).isEqualTo("localhost:9195");
        });
    }

    @Test
    void selectedProviderRegistersGatewayOnce() {
        Properties props = new Properties();
        props.setProperty("host", "127.0.0.1");
        props.setProperty("name", "test-gateway");
        RegisterConfig config = new RegisterConfig("test-registry-event", "localhost:9195", props);
        RegistryListener listener = new RegistryListener(config);
        WebServer server = mock(WebServer.class);
        when(server.getPort()).thenReturn(9195);
        WebServerInitializedEvent event = mock(WebServerInitializedEvent.class);
        when(event.getWebServer()).thenReturn(server);
        listener.onApplicationEvent(event);
        listener.onApplicationEvent(event);
        TestRepository repository = (TestRepository) ShenyuInstanceRegisterRepositoryFactory.newInstance("test-registry-event");
        assertThat(repository.persistCount).isEqualTo(1);
        assertThat(repository.instance.getAppName()).isEqualTo("test-gateway");
        assertThat(repository.instance.getHost()).isEqualTo("127.0.0.1");
        assertThat(repository.instance.getPort()).isEqualTo(9195);
    }

    /**
     * External implementation supplied only on the test runtime classpath.
     */
    @Join(isSingleton = false)
    public static class TestRepository implements ShenyuInstanceRegisterRepository {

        private RegisterConfig config;

        private InstanceEntity instance;

        private int persistCount;

        @Override
        public void init(final RegisterConfig registerConfig) {
            config = registerConfig;
        }

        @Override
        public void persistInstance(final InstanceEntity instanceEntity) {
            instance = instanceEntity;
            persistCount++;
        }
    }
}
