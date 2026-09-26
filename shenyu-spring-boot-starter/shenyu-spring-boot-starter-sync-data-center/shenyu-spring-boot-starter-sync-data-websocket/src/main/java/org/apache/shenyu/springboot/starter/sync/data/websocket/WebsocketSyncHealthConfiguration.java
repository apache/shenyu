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

package org.apache.shenyu.springboot.starter.sync.data.websocket;

import org.apache.shenyu.plugin.sync.data.websocket.WebsocketSyncDataService;
import org.springframework.boot.actuate.health.Health;
import org.springframework.boot.actuate.health.HealthEndpointGroup;
import org.springframework.boot.actuate.health.HealthEndpointGroups;
import org.springframework.boot.actuate.health.HealthEndpointGroupsPostProcessor;
import org.springframework.boot.actuate.health.HealthIndicator;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.HashMap;
import java.util.Map;

/**
 * Optional synchronization health contributor. Include websocketSync in the readiness group only.
 */
@Configuration(proxyBeanMethods = false)
@ConditionalOnClass(HealthIndicator.class)
@ConditionalOnProperty(prefix = "shenyu.sync.websocket", name = "initial-sync-readiness", havingValue = "true")
public class WebsocketSyncHealthConfiguration {

    /**
     * Restrict synchronization health to readiness, preserving all other group members.
     * This also protects deployments still probing the aggregate endpoint for liveness.
     * @return health group customizer
     */
    @Bean
    public HealthEndpointGroupsPostProcessor websocketSyncHealthGroups() {
        return groups -> {
            Map<String, HealthEndpointGroup> customized = new HashMap<>();
            groups.getNames().forEach(name -> customized.put(name,
                    new WebsocketSyncHealthGroup(groups.get(name), "readiness".equals(name))));
            customized.putIfAbsent("readiness", new WebsocketSyncHealthGroup(groups.getPrimary(), true));
            return HealthEndpointGroups.of(new WebsocketSyncHealthGroup(groups.getPrimary(), false), customized);
        };
    }

    /**
     * Create the startup synchronization health indicator.
     * @param service synchronization service
     * @return health indicator
     */
    @Bean
    public HealthIndicator websocketSyncHealthIndicator(final WebsocketSyncDataService service) {
        return () -> service.isInitialSyncReady() ? Health.up().build()
                : Health.outOfService().withDetail("reason", "Initial WebSocket synchronization has not completed; a compatible Admin is required").build();
    }
}
