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

package org.apache.shenyu.springboot.plugin.websocket;

import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.context.ShenyuContextDecorator;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.websocket.WebSocketPlugin;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.reactive.socket.client.ReactorNettyWebSocketClient;
import org.springframework.web.reactive.socket.server.WebSocketService;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Test case for {@link WebSocketPluginConfiguration}.
 */
@Configuration
@EnableConfigurationProperties
public class WebSocketPluginConfigurationTest {

    private ApplicationContextRunner applicationContextRunner;

    @BeforeEach
    public void before() {
        applicationContextRunner = new ApplicationContextRunner()
            .withConfiguration(AutoConfigurations.of(WebSocketPluginConfiguration.class))
            .withBean(WebSocketPluginConfigurationTest.class)
            .withConfiguration(AutoConfigurations.of(ShenyuConfig.class))
            .withPropertyValues("debug=true");
    }

    @Test
    public void testWebSocketPluginDataHandler() {
        applicationContextRunner.run(context -> {
                PluginDataHandler handler = context.getBean("websocketPluginDataHandler", PluginDataHandler.class);
                assertNotNull(handler);
            }
        );
    }

    @Test
    public void testWebSocketPlugin() {
        applicationContextRunner.run(context -> {
                WebSocketPlugin plugin = context.getBean("webSocketPlugin", WebSocketPlugin.class);
                assertNotNull(plugin);
                assertThat(plugin.named()).isEqualTo(PluginEnum.WEB_SOCKET.getName());
            }
        );
    }

    @Test
    public void testReactorNettyWebSocketClient() {
        applicationContextRunner.run(context -> {
                ReactorNettyWebSocketClient client = context.getBean("reactorNettyWebSocketClient", ReactorNettyWebSocketClient.class);
                assertNotNull(client);
            }
        );
    }

    @Test
    public void testHandshakeWebSocketService() {
        applicationContextRunner.run(context -> {
                WebSocketService service = context.getBean("webSocketService", WebSocketService.class);
                assertNotNull(service);
            }
        );
    }

    @Test
    public void testWebSocketShenyuContextDecorator() {
        applicationContextRunner.run(context -> {
                ShenyuContextDecorator decorator = context.getBean("webSocketShenyuContextDecorator", ShenyuContextDecorator.class);
                assertNotNull(decorator);
            }
        );
    }
}
