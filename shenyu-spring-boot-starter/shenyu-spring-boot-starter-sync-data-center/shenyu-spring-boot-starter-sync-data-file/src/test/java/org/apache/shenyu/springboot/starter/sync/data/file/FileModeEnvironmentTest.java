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

package org.apache.shenyu.springboot.starter.sync.data.file;

import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.plugin.sync.data.websocket.WebsocketSyncDataService;
import org.apache.shenyu.springboot.starter.sync.data.websocket.WebsocketSyncDataConfiguration;
import org.springframework.boot.autoconfigure.web.ServerProperties;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.mock.env.MockEnvironment;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.Mockito.mockConstruction;

/** Check exclusive source ownership before networking clients are constructed. */
class FileModeEnvironmentTest {

    @Test
    void leavesAdminModeUnchangedByDefault() {
        MockEnvironment environment = new MockEnvironment().withProperty("shenyu.sync.websocket.urls", "ws://admin:9095/websocket").withProperty("shenyu.heartbeat.enabled", "true");
        new FileModeEnvironmentPostProcessor().postProcessEnvironment(environment, null);
        assertThat(environment.getProperty("shenyu.sync.websocket.urls")).isEqualTo("ws://admin:9095/websocket");
        assertThat(environment.getProperty("shenyu.heartbeat.enabled")).isEqualTo("true");
        assertThat(environment.getPropertySources().contains("shenyuFileMode")).isFalse();
    }

    @Test
    void suppressesShippedAdminDefaultsAndImplicitLocalApi() {
        MockEnvironment environment = new MockEnvironment().withProperty("shenyu.sync.file.enabled", "true")
                .withProperty("shenyu.sync.websocket.urls", "ws://localhost:9095/websocket").withProperty("shenyu.heartbeat.enabled", "true")
                .withProperty("shenyu.extPlugin.enabled", "true").withProperty("shenyu.sync.websocket.initial-sync-readiness", "true");
        new FileModeEnvironmentPostProcessor().postProcessEnvironment(environment, null);
        for (String key : new String[]{"shenyu.sync.websocket.urls", "shenyu.sync.http.url", "shenyu.sync.zookeeper.url", "shenyu.sync.nacos.url", "shenyu.sync.etcd.url",
            "shenyu.sync.apollo.meta", "shenyu.heartbeat.enabled", "shenyu.local.enabled", "shenyu.extPlugin.enabled", "shenyu.sync.websocket.initial-sync-readiness"}) {
            assertThat(environment.getProperty(key)).as(key).isEqualTo("false");
        }
        new FileModeEnvironmentPostProcessor().postProcessEnvironment(environment, null);
        assertThat(environment.getProperty("shenyu.local.enabled")).isEqualTo("false");
    }

    @ParameterizedTest
    @ValueSource(strings = {"shenyu.sync.websocket.urls", "shenyu.sync.http.url", "shenyu.sync.zookeeper.url", "shenyu.sync.nacos.url", "shenyu.sync.etcd.url", "shenyu.sync.apollo.meta"})
    void rejectsExternalSourcesWithoutPrintingTheirValues(final String key) {
        MockEnvironment environment = new MockEnvironment().withProperty("shenyu.sync.file.enabled", "true").withProperty(key, "secret-endpoint");
        assertThatThrownBy(() -> new FileModeEnvironmentPostProcessor().postProcessEnvironment(environment, null))
                .hasMessageContaining(key).hasMessageNotContaining("secret-endpoint").hasNoCause();
    }

    @ParameterizedTest
    @ValueSource(strings = {"shenyu.local.enabled", "shenyu.register.enabled", "shenyu.alert.enabled"})
    void rejectsCompetingWritersAndAdminClients(final String key) {
        MockEnvironment environment = new MockEnvironment().withProperty("shenyu.sync.file.enabled", "true").withProperty(key, "true");
        assertThatThrownBy(() -> new FileModeEnvironmentPostProcessor().postProcessEnvironment(environment, null)).hasMessageContaining(key);
    }

    @Test
    void adminSyncStillStartsWithFileStarterPresentButDisabled() {
        try (var services = mockConstruction(WebsocketSyncDataService.class)) {
            new ApplicationContextRunner().withUserConfiguration(FileSyncDataConfiguration.class, WebsocketSyncDataConfiguration.class)
                    .withBean(ShenyuConfig.class, ShenyuConfig::new).withBean(ServerProperties.class, ServerProperties::new)
                    .withPropertyValues("shenyu.sync.file.enabled=false", "shenyu.sync.websocket.urls=ws://admin:9095/websocket")
                    .withInitializer(context -> new FileModeEnvironmentPostProcessor().postProcessEnvironment(context.getEnvironment(), null))
                    .run(context -> {
                        assertThat(context).hasSingleBean(WebsocketSyncDataService.class).doesNotHaveBean(FileConfigurationLoader.class);
                        assertThat(context.getEnvironment().getProperty("shenyu.sync.websocket.urls")).isEqualTo("ws://admin:9095/websocket");
                        assertThat(services.constructed()).hasSize(1);
                    });
        }
    }
}
