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

package org.apache.shenyu.admin.config;

import org.apache.shenyu.admin.config.properties.WebsocketSyncProperties;
import org.apache.shenyu.admin.listener.DataChangedListener;
import org.apache.shenyu.admin.listener.websocket.WebsocketCollector;
import org.apache.shenyu.admin.listener.websocket.WebsocketDataChangedListener;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.socket.server.standard.ServerEndpointExporter;

/**
 * The WebsocketListener(default strategy).
 */
@Configuration
@ConditionalOnProperty(name = "shenyu.sync.websocket.enabled", havingValue = "true", matchIfMissing = true)
@EnableConfigurationProperties(WebsocketSyncProperties.class)
public class WebSocketSyncConfiguration {

    /**
     * Config event listener data changed listener.
     *
     * @return the data changed listener
     */
    @Bean
    @ConditionalOnMissingBean(WebsocketDataChangedListener.class)
    public DataChangedListener websocketDataChangedListener() {
        return new WebsocketDataChangedListener();
    }

    /**
     * Websocket collector.
     *
     * @return the websocket collector
     */
    @Bean
    @ConditionalOnMissingBean(WebsocketCollector.class)
    public WebsocketCollector websocketCollector() {
        return new WebsocketCollector();
    }

    /**
     * Server endpoint exporter server endpoint exporter.
     *
     * @return the server endpoint exporter
     */
    @Bean
    @ConditionalOnMissingBean(ServerEndpointExporter.class)
    public ServerEndpointExporter serverEndpointExporter() {
        return new ServerEndpointExporter();
    }
}
