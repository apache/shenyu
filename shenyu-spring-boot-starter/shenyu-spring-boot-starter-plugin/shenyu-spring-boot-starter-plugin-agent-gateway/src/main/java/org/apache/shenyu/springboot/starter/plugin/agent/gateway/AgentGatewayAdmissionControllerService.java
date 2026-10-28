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

package org.apache.shenyu.springboot.starter.plugin.agent.gateway;

import org.apache.shenyu.common.dto.AgentGatewayAdmissionConfig;
import org.apache.shenyu.common.dto.AgentGatewayAggregationConfig;
import org.apache.shenyu.plugin.agent.gateway.protocol.AgentMcpResourceAdmission;

/** Applies trusted global/local limits independently of optional remote discovery. */
public final class AgentGatewayAdmissionControllerService {

    private final AgentMcpResourceAdmission.ResourceAdmissionController controller;

    AgentGatewayAdmissionControllerService(final AgentMcpResourceAdmission.ResourceAdmissionController controller) {
        this.controller = controller;
    }

    /**
     * Validate limits and atomically update the active controller's accounting state.
     * @param source complete trusted plugin configuration
     * @return immutable active limits
     */
    public AgentGatewayAdmissionConfig update(final String source) {
        AgentGatewayAdmissionConfig config = AgentGatewayAggregationConfig.parsePluginConfig(source).admission();
        controller.replace(AgentMcpResourceAdmission.create(config));
        return config;
    }
}
