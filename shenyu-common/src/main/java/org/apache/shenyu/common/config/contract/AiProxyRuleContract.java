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

import org.apache.shenyu.common.dto.convert.rule.AiProxyRuleOverrides;

import java.util.List;
import java.util.Map;

/** Shared Admin/gateway contract for the opt-in generation block; legacy root values remain inert. */
public final class AiProxyRuleContract implements ConfigurationContractProvider {

    private static final ConfigurationContract CONTRACT = new ConfigurationContract("aiProxy", ConfigurationScope.RULE, "1", false,
            ConfigurationContract.UnknownFields.SEMANTIC_VALIDATOR,
            Map.of("generationOverrides", field(ConfigurationContract.Type.OBJECT, "Explicit opt-in; absent/null/empty block inherits Selector defaults"),
                    "generationOverrides.model", field(ConfigurationContract.Type.STRING, "Nonblank model up to 256 characters; null restores request model"),
                    "generationOverrides.temperature", field(ConfigurationContract.Type.NUMBER, "Finite number in [0,2]; null restores request/default"),
                    "generationOverrides.maxTokens", field(ConfigurationContract.Type.INTEGER, "Positive int; null restores request/default"),
                    "generationOverrides.stream", field(ConfigurationContract.Type.BOOLEAN, "Default only; explicit client stream takes precedence; null clears default")),
            Map.of("legacyRoot", "Valid existing root fields remain ignored", "precedence", "Plugin < Selector < Rule; explicit client stream wins",
                    "nullField", "Clear this configured generation default", "missingField", "Inherit Selector/Plugin",
                    "protectedFields", "Reject unknown nested fields, including provider/protocol/credentials/URL/fallback/body patches",
                    "invalidUpdate", "Reject before publication; retain prior gateway overrides", "remove", "Evict by Rule id; Selector removal evicts its Rules"));

    @Override
    public ConfigurationContract contract() {
        return CONTRACT;
    }

    @Override
    public List<ConfigurationViolation> validateSemantic(final String source) {
        try {
            AiProxyRuleOverrides.parse(source);
            return List.of();
        } catch (IllegalArgumentException error) {
            return List.of(new ConfigurationViolation("generationOverrides", ConfigurationViolation.Code.SEMANTIC_INVALID));
        }
    }

    private static ConfigurationContract.Field field(final ConfigurationContract.Type type, final String description) {
        return new ConfigurationContract.Field(type, false, true, null, "", false, null, null, List.of(), description);
    }
}
