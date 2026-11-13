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

import io.netty.channel.ChannelInboundHandlerAdapter;
import io.netty.channel.ChannelPipeline;
import io.netty.channel.embedded.EmbeddedChannel;
import io.netty.handler.timeout.IdleStateHandler;
import io.netty.handler.timeout.ReadTimeoutHandler;
import io.netty.handler.timeout.WriteTimeoutHandler;
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
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.reactive.socket.client.ReactorNettyWebSocketClient;
import org.springframework.web.reactive.socket.server.WebSocketService;
import reactor.core.publisher.Mono;
import reactor.netty.DisposableServer;
import reactor.netty.http.client.HttpClient;
import reactor.netty.http.client.WebsocketClientSpec;
import reactor.netty.http.server.HttpServer;

import java.net.URI;
import java.time.Duration;
import java.util.concurrent.TimeUnit;
import java.util.function.Supplier;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

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
                @SuppressWarnings("unchecked")
                Supplier<WebsocketClientSpec.Builder> builderSupplier =
                        (Supplier<WebsocketClientSpec.Builder>) ReflectionTestUtils.getField(client, "specBuilderSupplier");
                assertNotNull(builderSupplier);
                assertThat(builderSupplier.get()).isNotSameAs(builderSupplier.get());
            }
        );
    }

    @Test
    public void testRemoveHttpTimeoutHandlers() {
        EmbeddedChannel channel = new EmbeddedChannel();
        ChannelPipeline pipeline = channel.pipeline();
        pipeline.addLast(new IdleStateHandler(3, 3, 3, TimeUnit.SECONDS));
        pipeline.addLast(new ReadTimeoutHandler(3, TimeUnit.SECONDS));
        pipeline.addLast(new WriteTimeoutHandler(3, TimeUnit.SECONDS));
        pipeline.addLast("keep", new ChannelInboundHandlerAdapter());

        WebSocketPluginConfiguration.removeHandler(pipeline, IdleStateHandler.class);
        WebSocketPluginConfiguration.removeHandler(pipeline, ReadTimeoutHandler.class);
        WebSocketPluginConfiguration.removeHandler(pipeline, WriteTimeoutHandler.class);

        assertThat(pipeline.get(IdleStateHandler.class)).isNull();
        assertThat(pipeline.get(ReadTimeoutHandler.class)).isNull();
        assertThat(pipeline.get(WriteTimeoutHandler.class)).isNull();
        assertThat(pipeline.get("keep")).isNotNull();
        channel.finishAndReleaseAll();
    }

    @Test
    public void testWebSocketSurvivesHttpTimeouts() {
        DisposableServer server = HttpServer.create()
                .port(0)
                .handle((request, response) -> response.sendWebsocket((in, out) -> out.send(in.receive().retain())))
                .bindNow();
        try {
            // Mirrors the shared client built by HttpClientFactory: the three connection-level
            // timeout handlers are added through doOnConnected and responseTimeout is 3s by default.
            HttpClient httpClient = HttpClient.create()
                    .responseTimeout(Duration.ofSeconds(3))
                    .doOnConnected(connection -> {
                        connection.addHandlerLast(new IdleStateHandler(3, 3, 3, TimeUnit.SECONDS));
                        connection.addHandlerLast(new WriteTimeoutHandler(3, TimeUnit.SECONDS));
                        connection.addHandlerLast(new ReadTimeoutHandler(3, TimeUnit.SECONDS));
                    });
            ReactorNettyWebSocketClient client =
                    new ReactorNettyWebSocketClient(WebSocketPluginConfiguration.webSocketHttpClient(httpClient));
            client.execute(URI.create("ws://localhost:" + server.port() + "/"), session ->
                    session.send(Mono.just(session.textMessage("first")))
                            .then(Mono.delay(Duration.ofSeconds(4)))
                            .then(Mono.fromRunnable(() -> assertTrue(session.isOpen())))
                            .then(session.send(Mono.just(session.textMessage("second"))))
                            .then())
                    .block(Duration.ofSeconds(15));
        } finally {
            server.disposeNow();
        }
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
