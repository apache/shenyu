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

package org.apache.shenyu.springboot.starter.client.spring.websocket;

import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.core.register.ShenyuClientRegisterRepositoryFactory;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.register.client.http.HttpClientRegisterRepository;
import org.apache.shenyu.register.common.config.ShenyuDiscoveryConfig;
import org.apache.shenyu.register.common.dto.DiscoveryConfigRegisterDTO;
import org.apache.shenyu.springboot.starter.client.springmvc.ShenyuSpringMvcClientConfiguration;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedStatic;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.autoconfigure.context.ConfigurationPropertiesAutoConfiguration;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;

class WebSocketDiscoveryConfigTest {

    @Test
    void webSocketOnlyUsesWebSocketContextPath() {
        assertWebSocketDiscoverySelector();
    }

    @Test
    void webSocketWithSpringMvcUsesWebSocketContextPath() {
        assertWebSocketDiscoverySelector(ShenyuSpringMvcClientConfiguration.class);
    }

    private void assertWebSocketDiscoverySelector(final Class<?>... additionalConfigurations) {
        HttpClientRegisterRepository repository = mock(HttpClientRegisterRepository.class);
        ShenyuClientRegisterEventPublisher publisher = mock(ShenyuClientRegisterEventPublisher.class);
        try (MockedStatic<ShenyuClientRegisterRepositoryFactory> repositoryFactory = mockStatic(ShenyuClientRegisterRepositoryFactory.class);
                MockedStatic<ShenyuClientRegisterEventPublisher> publisherFactory = mockStatic(ShenyuClientRegisterEventPublisher.class)) {
            repositoryFactory.when(() -> ShenyuClientRegisterRepositoryFactory.newInstance(any())).thenReturn(repository);
            publisherFactory.when(ShenyuClientRegisterEventPublisher::getInstance).thenReturn(publisher);
            new ApplicationContextRunner()
                    .withBean(HttpClientRegisterRepository.class, () -> repository, definition -> definition.setPrimary(true))
                    .withBean(ShenyuDiscoveryConfig.class, this::discoveryConfig)
                    .withConfiguration(AutoConfigurations.of(ConfigurationPropertiesAutoConfiguration.class,
                            ShenyuSpringWebSocketClientConfiguration.class, ShenyuSpringWebSocketDiscoveryConfiguration.class))
                    .withUserConfiguration(additionalConfigurations)
                    .withPropertyValues(
                            "shenyu.discovery.serverList=127.0.0.1:8848",
                            "shenyu.client.http.props[appName]=test",
                            "shenyu.client.http.props[contextPath]=/api",
                            "shenyu.client.websocket.props[appName]=test",
                            "shenyu.client.websocket.props[contextPath]=/ws")
                    .run(context -> {
                        assertNull(context.getStartupFailure());
                        ArgumentCaptor<DiscoveryConfigRegisterDTO> registration = ArgumentCaptor.forClass(DiscoveryConfigRegisterDTO.class);
                        verify(repository).doPersistDiscoveryConfig(registration.capture());
                        assertEquals(PluginEnum.WEB_SOCKET.getName(), registration.getValue().getPluginName());
                        assertEquals("/ws", registration.getValue().getSelectorName());
                    });
        }
    }

    private ShenyuDiscoveryConfig discoveryConfig() {
        ShenyuDiscoveryConfig config = new ShenyuDiscoveryConfig();
        config.setServerList("127.0.0.1:8848");
        config.setType("nacos");
        return config;
    }
}
