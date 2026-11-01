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

package org.apache.shenyu.common.config.contract;

import org.apache.shenyu.common.dto.AgentGatewayAggregationConfig;

import java.util.List;
import java.util.Map;

/** Behavior-preserving adapter for the existing shared Agent Gateway aggregation parser. */
public final class AgentGatewayPluginContract implements ConfigurationContractProvider {

    private static final ConfigurationContract CONTRACT = new ConfigurationContract("agentGateway", ConfigurationScope.PLUGIN, "1", false,
            ConfigurationContract.UnknownFields.SEMANTIC_VALIDATOR,
            Map.of("aggregation", new ConfigurationContract.Field(ConfigurationContract.Type.OBJECT, false, false,
                    "local-only when missing/blank/empty object", "", false, null, null, List.of())),
            Map.of("missing", "local-only", "blank", "local-only", "emptyObject", "local-only", "explicitNull", "invalid",
                    "invalidUpdate", "reject before persistence/preparation; retain current remote catalog", "remove", "owned by catalog lifecycle"));

    @Override
    public ConfigurationContract contract() {
        return CONTRACT;
    }

    @Override
    public List<ConfigurationViolation> validateSemantic(final String source) {
        try {
            AgentGatewayAggregationConfig.parsePluginConfig(source);
            return List.of();
        } catch (IllegalArgumentException error) {
            return List.of(new ConfigurationViolation("$", ConfigurationViolation.Code.SEMANTIC_INVALID));
        }
    }
}
