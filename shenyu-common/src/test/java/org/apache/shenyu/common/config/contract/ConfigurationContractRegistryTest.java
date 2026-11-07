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
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.apache.shenyu.common.config.contract.ConfigurationContract.Type.INTEGER;
import static org.apache.shenyu.common.config.contract.ConfigurationContract.Type.STRING;
import static org.apache.shenyu.common.config.contract.ConfigurationViolation.Code.INVALID_ENUM;
import static org.apache.shenyu.common.config.contract.ConfigurationViolation.Code.INVALID_JSON;
import static org.apache.shenyu.common.config.contract.ConfigurationViolation.Code.INVALID_TYPE;
import static org.apache.shenyu.common.config.contract.ConfigurationViolation.Code.NULL_NOT_ALLOWED;
import static org.apache.shenyu.common.config.contract.ConfigurationViolation.Code.OUT_OF_RANGE;
import static org.apache.shenyu.common.config.contract.ConfigurationViolation.Code.REQUIRED;
import static org.apache.shenyu.common.config.contract.ConfigurationViolation.Code.UNKNOWN_FIELD;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ConfigurationContractRegistryTest {

    @ParameterizedTest
    @EnumSource(ConfigurationScope.class)
    void selectsAllThreeScopesAndPreservesInput(final ConfigurationScope scope) {
        ConfigurationContractRegistry registry = new ConfigurationContractRegistry();
        for (ConfigurationScope candidate : ConfigurationScope.values()) {
            registry.register(provider(candidate));
        }
        String source = "{\"count\":2,\"mode\":\"custom\"}";
        ConfigurationValidation result = registry.validate("fixture", scope, "1", source);
        assertEquals(ConfigurationValidation.Status.VALID, result.status());
        assertEquals(scope, result.scope());
        assertEquals(3, registry.catalog().size());
        assertEquals("milliseconds", registry.catalog().get(0).fields().get("count").unit());
        assertEquals("2", registry.catalog().get(0).fields().get("count").effectiveDefault());
        assertEquals("{\"count\":2,\"mode\":\"custom\"}", source);
    }

    @Test
    void structuralConstraintsAreSeparateFromLegacySemanticCoercion() {
        ConfigurationContractRegistry registry = new ConfigurationContractRegistry();
        registry.register(provider(ConfigurationScope.RULE));
        assertCode(registry, "not-json", INVALID_JSON);
        assertCode(registry, "[]", INVALID_TYPE);
        assertCode(registry, "{\"count\":null}", NULL_NOT_ALLOWED);
        assertCode(registry, "{}", REQUIRED);
        assertCode(registry, "{\"count\":\"2\"}", INVALID_TYPE);
        assertCode(registry, "{\"count\":1.5}", INVALID_TYPE);
        assertCode(registry, "{\"count\":0}", OUT_OF_RANGE);
        assertCode(registry, "{\"count\":4}", OUT_OF_RANGE);
        assertCode(registry, "{\"count\":2,\"mode\":\"other\"}", INVALID_ENUM);
        assertCode(registry, "{\"count\":2,\"SECRET-IN-UNKNOWN-KEY\":\"SECRET-IN-VALUE\"}", UNKNOWN_FIELD);
    }

    @Test
    void missingOptionalAndExplicitNullableValuesDoNotInsertDefaults() {
        ConfigurationContractRegistry registry = new ConfigurationContractRegistry();
        registry.register(provider(ConfigurationScope.RULE));
        assertEquals(ConfigurationValidation.Status.VALID, registry.validate("fixture", ConfigurationScope.RULE, "1", "{\"count\":2}").status());
        assertEquals(ConfigurationValidation.Status.VALID, registry.validate("fixture", ConfigurationScope.RULE, "1", "{\"count\":2,\"mode\":null}").status());
    }

    @Test
    void duplicateAndUnknownVersionsAreDistinctFromLegacyFallback() {
        ConfigurationContractRegistry registry = new ConfigurationContractRegistry();
        registry.register(provider(ConfigurationScope.RULE));
        assertThrows(IllegalArgumentException.class, () -> registry.register(provider(ConfigurationScope.RULE)));
        assertEquals(ConfigurationValidation.Status.UNSUPPORTED_VERSION, registry.validate("fixture", ConfigurationScope.RULE, "2", "{}").status());
        assertEquals(ConfigurationValidation.Status.UNREGISTERED, registry.validate("legacy", ConfigurationScope.RULE, "1", "not-json").status());
        assertEquals(ConfigurationValidation.Status.UNREGISTERED, registry.validate("fixture", ConfigurationScope.SELECTOR, "1", "not-json").status());
    }

    @Test
    void providerExceptionsAndExcessErrorsAreBoundedAndSecretFree() {
        ConfigurationContractRegistry registry = new ConfigurationContractRegistry();
        registry.register(new ConfigurationContractProvider() {
            @Override
            public ConfigurationContract contract() {
                return descriptor(ConfigurationScope.PLUGIN, false);
            }

            @Override
            public List<ConfigurationViolation> validateSemantic(final String source) {
                throw new IllegalArgumentException("SECRET-PARSER-DETAIL");
            }
        });
        ConfigurationValidation result = registry.validate("fixture", ConfigurationScope.PLUGIN, "1", "SECRET-INPUT");
        assertEquals(ConfigurationViolation.Code.PROVIDER_FAILURE, result.violations().get(0).code());
        assertFalse(result.toString().contains("SECRET"));
        ConfigurationContractRegistry excessive = new ConfigurationContractRegistry();
        excessive.register(new ConfigurationContractProvider() {
            @Override
            public ConfigurationContract contract() {
                return descriptor(ConfigurationScope.PLUGIN, false);
            }

            @Override
            public List<ConfigurationViolation> validateSemantic(final String source) {
                List<ConfigurationViolation> errors = new ArrayList<>();
                for (int index = 0; index < 40; index++) {
                    errors.add(new ConfigurationViolation("count", OUT_OF_RANGE));
                }
                return errors;
            }
        });
        assertEquals(16, excessive.validate("fixture", ConfigurationScope.PLUGIN, "1", "{}").violations().size());
    }

    @Test
    void descriptorsAndResultsAreImmutable() {
        ConfigurationContractRegistry registry = new ConfigurationContractRegistry();
        registry.register(provider(ConfigurationScope.RULE));
        assertThrows(UnsupportedOperationException.class, () -> registry.catalog().clear());
        assertThrows(UnsupportedOperationException.class, () -> registry.catalog().get(0).fields().clear());
        assertThrows(UnsupportedOperationException.class, () -> registry.catalog().get(0).fields().get("mode").values().clear());
        assertThrows(UnsupportedOperationException.class, () -> registry.validate("fixture", ConfigurationScope.RULE, "1", "{}").violations().clear());
    }

    @ParameterizedTest(name = "{0}")
    @MethodSource("configurationFixtures")
    void discoveredProductionAdapterIsEquivalentToExistingParser(final String name, final boolean expected, final String source) {
        boolean accepted;
        try {
            AgentGatewayAggregationConfig.parsePluginConfig(source);
            accepted = true;
        } catch (IllegalArgumentException error) {
            accepted = false;
        }
        ConfigurationValidation result = ConfigurationContractRegistry.shared().validate("agentGateway", ConfigurationScope.PLUGIN, "1", source);
        assertEquals(expected, accepted);
        assertEquals(accepted ? ConfigurationValidation.Status.VALID : ConfigurationValidation.Status.INVALID, result.status());
        assertFalse(result.toString().contains("SECRET"));
        assertTrue(ConfigurationContractRegistry.shared().catalog().stream().anyMatch(contract -> contract.plugin().equals("agentGateway")));
    }

    private void assertCode(final ConfigurationContractRegistry registry, final String source, final ConfigurationViolation.Code code) {
        ConfigurationValidation result = registry.validate("fixture", ConfigurationScope.RULE, "1", source);
        assertEquals(ConfigurationValidation.Status.INVALID, result.status());
        assertEquals(code, result.violations().get(0).code());
        assertFalse(result.toString().contains("SECRET"));
    }

    private ConfigurationContractProvider provider(final ConfigurationScope scope) {
        return () -> descriptor(scope, true);
    }

    private static java.util.stream.Stream<org.junit.jupiter.params.provider.Arguments> configurationFixtures() throws java.io.IOException {
        try (var input = java.util.Objects.requireNonNull(ConfigurationContractRegistryTest.class.getResourceAsStream(
                "/META-INF/shenyu/config-contract-fixtures/agentGateway-plugin-v1.tsv"));
             var reader = new java.io.BufferedReader(new java.io.InputStreamReader(input, java.nio.charset.StandardCharsets.UTF_8))) {
            return reader.lines().filter(line -> !line.isBlank() && !line.startsWith("#")).map(line -> {
                String[] fields = line.split("\\t", 3);
                String source = "@missing".equals(fields[2]) ? null : "@blank".equals(fields[2]) ? "" : fields[2];
                return org.junit.jupiter.params.provider.Arguments.of(fields[0], Boolean.parseBoolean(fields[1]), source);
            }).toList().stream();
        }
    }


    private ConfigurationContract descriptor(final ConfigurationScope scope, final boolean strict) {
        return new ConfigurationContract("fixture", scope, "1", strict, ConfigurationContract.UnknownFields.REJECT,
                Map.of("count", new ConfigurationContract.Field(INTEGER, true, false, "2", "milliseconds", false, 1L, 3L, List.of()),
                        "mode", new ConfigurationContract.Field(STRING, false, true, "custom", "", true, null, null, List.of("custom"))),
                Map.of("invalidUpdate", "adapter-owned"));
    }
}
