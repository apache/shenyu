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

package org.apache.shenyu.admin.validation.validator;

import org.apache.shenyu.admin.exception.ShenyuAdminException;
import org.apache.shenyu.common.dto.AgentGatewayAggregationConfig;
import org.apache.shenyu.common.enums.PluginEnum;

/** Reject invalid aggregation references before persistence and data-sync publication. */
public final class AgentGatewayPluginConfigValidator {

    private AgentGatewayPluginConfigValidator() {
    }

    /**
     * Leave other plugins' configuration contracts unchanged.
     * @param name trusted stored plugin name
     * @param config supplied plugin configuration
     */
    public static void validate(final String name, final String config) {
        if (PluginEnum.AGENT_GATEWAY.getName().equals(name)) {
            try {
                AgentGatewayAggregationConfig.parsePluginConfig(config);
            } catch (IllegalArgumentException error) {
                throw new ShenyuAdminException("Invalid Agent Gateway aggregation configuration");
            }
        }
    }
}
