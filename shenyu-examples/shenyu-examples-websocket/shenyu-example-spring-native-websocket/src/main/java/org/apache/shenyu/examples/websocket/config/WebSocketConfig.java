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

package org.apache.shenyu.examples.websocket.config;

import org.apache.shenyu.examples.websocket.handler.HttpAuthHandler;
import org.apache.shenyu.examples.websocket.interceptor.WebSocketInterceptor;
import org.apache.shenyu.client.spring.websocket.annotation.ShenyuSpringWebSocketClient;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.socket.config.annotation.EnableWebSocket;
import org.springframework.web.socket.config.annotation.WebSocketConfigurer;
import org.springframework.web.socket.config.annotation.WebSocketHandlerRegistry;

/**
 * The type Web socket configuration.
 * eg:   ws://127.0.0.1:9195/ws-native/myWebSocket?token=shenyu
 */
@Configuration
@EnableWebSocket
@ShenyuSpringWebSocketClient("/myWebSocket")
public class WebSocketConfig implements WebSocketConfigurer {

    @Autowired
    private HttpAuthHandler httpAuthHandler;
    
    @Autowired
    private WebSocketInterceptor myInterceptor;

    @Override
    public void registerWebSocketHandlers(final WebSocketHandlerRegistry registry) {
        registry.addHandler(httpAuthHandler, "myWebSocket")
                .addInterceptors(myInterceptor)
                .setAllowedOrigins("*");
    }
}
