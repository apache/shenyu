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

package org.apache.shenyu.springboot.plugin.divide;

import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.context.ShenyuContextDecorator;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.divide.DividePlugin;
import org.apache.shenyu.plugin.divide.context.DivideShenyuContextDecorator;
import org.apache.shenyu.plugin.divide.handler.DividePluginDataHandler;
import org.apache.shenyu.plugin.divide.websocket.WebSocketPlugin;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.reactive.socket.client.ReactorNettyWebSocketClient;
import org.springframework.web.reactive.socket.client.WebSocketClient;
import org.springframework.web.reactive.socket.server.WebSocketService;
import org.springframework.web.reactive.socket.server.support.HandshakeWebSocketService;

/**
 * ShenyuConfiguration.
 */
@Configuration
public class DividePluginConfiguration {

    /**
     * init dividePlugin.
     *
     * @return {@linkplain DividePlugin}
     */
    @Bean
    public ShenyuPlugin dividePlugin() {
        return new DividePlugin();
    }


    /**
     * Divide plugin data handler plugin data handler.
     *
     * @return the plugin data handler
     */
    @Bean
    public PluginDataHandler dividePluginDataHandler() {
        return new DividePluginDataHandler();
    }

    /**
     * Web socket plugin web socket plugin.
     *
     * @param webSocketClient  the web socket client
     * @param webSocketService the web socket service
     * @return the web socket plugin
     */
    @Bean
    public WebSocketPlugin webSocketPlugin(final WebSocketClient webSocketClient, final WebSocketService webSocketService) {
        return new WebSocketPlugin(webSocketClient, webSocketService);
    }

    /**
     * Reactor netty web socket client reactor netty web socket client.
     *
     * @return the reactor netty web socket client
     */
    @Bean
    public ReactorNettyWebSocketClient reactorNettyWebSocketClient() {
        return new ReactorNettyWebSocketClient();
    }

    /**
     * Web socket service web socket service.
     *
     * @return the web socket service
     */
    @Bean
    public WebSocketService webSocketService() {
        return new HandshakeWebSocketService();
    }
    
    /**
     * Divide shenyu context decorator shenyu context decorator.
     *
     * @return the shenyu context decorator
     */
    @Bean
    public ShenyuContextDecorator divideShenyuContextDecorator() {
        return new DivideShenyuContextDecorator();
    }
}
