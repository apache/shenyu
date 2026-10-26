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

package org.apache.shenyu.register.client.beat;

import org.apache.shenyu.common.config.ShenyuConfig;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.MockedConstruction;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.autoconfigure.context.ConfigurationPropertiesAutoConfiguration;
import org.springframework.boot.autoconfigure.web.ServerProperties;
import org.springframework.boot.test.context.FilteredClassLoader;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mockConstruction;

/**
 * Gateway heartbeats remain available without any business-client implementation.
 */
class HeartbeatListenerConfigurationTest {

    private final ApplicationContextRunner runner = new ApplicationContextRunner()
            .withClassLoader(new FilteredClassLoader("org.apache.shenyu.client", "org.apache.shenyu.register.client.http"))
            .withConfiguration(AutoConfigurations.of(ConfigurationPropertiesAutoConfiguration.class, HeartbeatListenerConfiguration.class))
            .withBean(ShenyuConfig.class, ShenyuConfig::new)
            .withBean(ServerProperties.class, ServerProperties::new)
            .withPropertyValues("shenyu.heartbeat.server-lists=http://127.0.0.1:9095");

    @Test
    void enablesHeartbeatWithoutBusinessClient() {
        try (MockedConstruction<HeartbeatListener> construction = mockConstruction(HeartbeatListener.class)) {
            runner.run(context -> {
                assertThat(context).hasNotFailed().hasSingleBean(HeartbeatListener.class);
                assertThat(context.getBean(ShenyuBootstrapHeartBeatConfig.class).getServerLists()).isEqualTo("http://127.0.0.1:9095");
                assertThat(construction.constructed()).hasSize(1);
            });
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"shenyu.heartbeat.enabled=false", "shenyu.sync.websocket.urls=ws://127.0.0.1:9095/websocket",
            "shenyu.sync.http.url=http://127.0.0.1:9095"})
    void preservesExistingDisableConditions(final String property) {
        runner.withPropertyValues(property).run(context -> assertThat(context).hasNotFailed().doesNotHaveBean(HeartbeatListener.class));
    }
}
