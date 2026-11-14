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
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertThrows;

class AiProxyRuleContractTest {

    @ParameterizedTest
    @ValueSource(strings = {"", "{}", "null", "{\"provider\":\"legacy\",\"model\":\"legacy\"}", "{\"generationOverrides\":null}",
        "{\"generationOverrides\":{}}", "{\"generationOverrides\":{\"model\":\"fixture\",\"temperature\":0.25,\"maxTokens\":1024,\"stream\":false}}"})
    void existingRootHandlesAndExplicitGenerationValuesAreValidatedWithoutRewriting(final String source) {
        assertEquals(ConfigurationValidation.Status.VALID, ConfigurationContractRegistry.shared().validate("aiProxy", ConfigurationScope.RULE, "1", source).status());
        AiProxyRuleOverrides.parse(source);
    }

    @ParameterizedTest
    @ValueSource(strings = {"[]", "bad-json", "{\"generationOverrides\":1}", "{\"generationOverrides\":{\"model\":\"\"}}",
        "{\"generationOverrides\":{\"temperature\":\"0.2\"}}", "{\"generationOverrides\":{\"temperature\":3}}",
        "{\"generationOverrides\":{\"maxTokens\":0}}", "{\"generationOverrides\":{\"maxTokens\":1.5}}",
        "{\"generationOverrides\":{\"stream\":\"false\"}}", "{\"generationOverrides\":{\"apiKey\":\"FAKE_SECRET\"}}",
        "{\"generationOverrides\":{\"provider\":\"new-provider\"}}", "{\"generationOverrides\":{\"baseUrl\":\"https://fixture.invalid\"}}",
        "{\"generationOverrides\":{\"protocol\":\"other\"}}", "{\"generationOverrides\":{\"fallbackConfig\":{}}}",
        "{\"generationOverrides\":{\"requestBodyOverrides\":{}}}", "{\"generationOverrides\":{\"model\":\"one\",\"model\":\"two\"}}"})
    void protectedAndInvalidValuesHaveBoundedSharedDiagnostics(final String source) {
        assertThrows(IllegalArgumentException.class, () -> AiProxyRuleOverrides.parse(source));
        var validation = ConfigurationContractRegistry.shared().validate("aiProxy", ConfigurationScope.RULE, "1", source);
        assertEquals(ConfigurationValidation.Status.INVALID, validation.status());
        assertTrue(!validation.toString().contains("FAKE_SECRET"));
    }

    @Test
    void explicitNullClearsAFieldAndPublishedPresenceIsImmutable() {
        AiProxyRuleOverrides cleared = AiProxyRuleOverrides.parse("{\"generationOverrides\":{\"model\":null,\"temperature\":null}}");
        assertEquals(Set.of(AiProxyRuleOverrides.Setting.MODEL, AiProxyRuleOverrides.Setting.TEMPERATURE), cleared.specified());
        assertThrows(UnsupportedOperationException.class, () -> cleared.specified().clear());
        assertThrows(IllegalArgumentException.class, () -> AiProxyRuleOverrides.parse(" ".repeat(65536) + "{}"));
    }
}
