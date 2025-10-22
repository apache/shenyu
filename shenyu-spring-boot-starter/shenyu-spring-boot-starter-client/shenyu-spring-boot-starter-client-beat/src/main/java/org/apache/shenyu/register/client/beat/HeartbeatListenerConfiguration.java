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
import org.springframework.boot.autoconfigure.condition.ConditionalOnExpression;
import org.springframework.boot.autoconfigure.web.ServerProperties;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
@ConditionalOnExpression(
        "${shenyu.heartbeat.enabled:true} and "
           + "'${shenyu.sync.websocket.urls:}'.isEmpty() and "
           + "'${shenyu.sync.http.url:}'.isEmpty()"
)
public class HeartbeatListenerConfiguration {

    /**
     * Heartbeat bean listener.
     *
     * @param shenyuBootstrapHeartBeatConfig the shenyuBootstrapHeartBeatConfig
     * @param shenyuConfig                   the shenyu config
     * @param serverProperties               the server properties
     * @return the heartbeat bean listener.
     */
    @Bean
    public HeartbeatListener heartbeatListener(final ShenyuBootstrapHeartBeatConfig shenyuBootstrapHeartBeatConfig,
                                               final ShenyuConfig shenyuConfig,
                                               final ServerProperties serverProperties) {
        return new HeartbeatListener(shenyuBootstrapHeartBeatConfig, shenyuConfig, serverProperties);
    }

    /**
     * ShenyuBootstrapHeartBeatConfig.
     *
     * @return the shenyuBootstrapHeartBeatConfig.
     */
    @Bean
    @ConfigurationProperties(prefix = "shenyu.heartbeat")
    public ShenyuBootstrapHeartBeatConfig shenyuBootstrapHeartBeatConfig() {
        return new ShenyuBootstrapHeartBeatConfig();
    }

}
