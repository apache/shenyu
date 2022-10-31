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
import org.apache.shenyu.plugin.api.context.ShenyuContextDecorator;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.websocket.WebSocketPlugin;
import org.apache.shenyu.plugin.websocket.context.WebSocketShenyuContextDecorator;
import org.apache.shenyu.plugin.websocket.handler.WebSocketPluginDataHandler;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.reactive.socket.client.ReactorNettyWebSocketClient;
import org.springframework.web.reactive.socket.client.WebSocketClient;
import org.springframework.web.reactive.socket.server.WebSocketService;
import org.springframework.web.reactive.socket.server.support.HandshakeWebSocketService;
import reactor.netty.http.client.HttpClient;
import reactor.netty.http.client.WebsocketClientSpec;

import java.util.function.Supplier;

/**
 * The type Web socket plugin configuration.
 */
@Configuration
@ConditionalOnProperty(value = {"shenyu.plugins.websocket.enabled"}, havingValue = "true", matchIfMissing = true)
public class WebSocketPluginConfiguration {

    /**
     * Websocket plugin data handler.
     *
     * @return the plugin data handler
     */
    @Bean
    public PluginDataHandler websocketPluginDataHandler() {
        return new WebSocketPluginDataHandler();
    }

    /**
     * Web socket plugin web socket plugin.
     *
     * @param webSocketClient the web socket client
     * @param webSocketService the web socket service
     * @return the web socket plugin
     */
    @Bean
    public WebSocketPlugin webSocketPlugin(final WebSocketClient webSocketClient, final WebSocketService webSocketService) {
        return new WebSocketPlugin(webSocketClient, webSocketService);
    }

    /**
     * Reactor netty web socket client.
     *
     * @param shenyuConfig the shenyu config
     * @param httpClient   the http client
     * @return the reactor netty web socket client
     */
    @Bean
    public ReactorNettyWebSocketClient reactorNettyWebSocketClient(final ShenyuConfig shenyuConfig,
                                                                   final ObjectProvider<HttpClient> httpClient) {
        Supplier<WebsocketClientSpec.Builder> builder = WebsocketClientSpec.builder()
                .maxFramePayloadLength(shenyuConfig.getWebsocket().getMaxFramePayloadSize() * 1024 * 1024);
        return new ReactorNettyWebSocketClient(httpClient.getIfAvailable(HttpClient::create), builder);
    }

    /**
     * Web socket service.
     *
     * @return the web socket service
     */
    @Bean
    public WebSocketService webSocketService() {
        return new HandshakeWebSocketService();
    }

    /**
     * Web socket shenyu context decorator.
     *
     * @return the shenyu context decorator
     */
    @Bean
    public ShenyuContextDecorator webSocketShenyuContextDecorator() {
        return new WebSocketShenyuContextDecorator();
    }
}
