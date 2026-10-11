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

package org.apache.shenyu.common.dto.convert.rule.canary;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.google.re2j.PatternSyntaxException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Canary wire contract validation tests.
 */
class CanaryConfigValidatorTest {

    @Test
    void testLegacyHandleDoesNotAcquireCanaryConfiguration() {
        assertNull(CanaryConfigValidator.parseHandle("{\"timeout\":5000}").getCanary());
        assertNull(CanaryConfigValidator.parseHandle("{\"canary\":null}").getCanary());
    }

    @ParameterizedTest
    @ValueSource(strings = {
        "{\"percentage\":-1}", "{\"percentage\":101}", "{\"percentage\":1.5}", "{\"percentage\":\"20\"}",
        "{\"percentage\":null}", "{\"percentage\":1e20}", "{\"enabled\":\"false\"}", "{\"enabled\":null}",
        "{\"matchMode\":2}", "{\"matchMode\":0.5}", "{\"matchMode\":null}", "{\"fallbackPolicy\":\"stable\"}",
        "{\"fallbackPolicy\":null}", "{\"stickyKey\":null}", "{\"stickyKey\":{}}", "{\"stickyKey\":{\"paramType\":\" \"}}",
        "{\"stickyKey\":{\"paramType\":\"header\"}}", "{\"enabled\":true,\"percentage\":20}",
        "{\"conditions\":{}}", "{\"conditions\":null}", "{\"conditions\":[null]}", "{\"conditions\":[{}]}",
        "{\"conditions\":[{\"paramType\":\"header\",\"operator\":\"=\",\"paramValue\":\"east\"}]}",
        "{\"conditions\":[{\"paramType\":\"uri\",\"operator\":\"=\"}]}",
        "{\"conditions\":[{\"paramType\":\"uri\",\"operator\":\"regex\",\"paramValue\":\"[\"}]}",
        "{\"conditions\":[{\"paramType\":\"uri\",\"operator\":\"TimeAfter\",\"paramValue\":\"yesterday\"}]}",
        "{\"stableLabels\":{}}", "{\"stableLabels\":null}", "{\"stableLabels\":{\"release\":1}}",
        "{\"stableLabels\":{\"release\":true}}", "{\"stableLabels\":{\"release\":null}}",
        "{\"stableLabels\":{\" \" :\"stable\"}}", "{\"stableLabels\":{\"release\":\" \"}}",
        "{\"stableLabels\":{\"release\":\"canary\"}}", "{\"stableLabels\":{\"region\":\"east\"}}"
    })
    void testRejectsInvalidConfigurationBeforeCoercion(final String patch) {
        assertThrows(IllegalArgumentException.class, () -> CanaryConfigValidator.parseHandle(handle(patch)));
    }

    @ParameterizedTest
    @ValueSource(strings = {
        "{}", "{\"enabled\":true,\"percentage\":0}", "{\"enabled\":true,\"percentage\":100}",
        "{\"enabled\":false,\"percentage\":20}",
        "{\"enabled\":true,\"percentage\":20,\"stickyKey\":{\"paramType\":\"header\",\"paramName\":\"X-User-Id\"}}",
        "{\"enabled\":true,\"percentage\":20,\"stickyKey\":{\"paramType\":\"ip\"}}",
        "{\"stickyKey\":{\"paramType\":\"custom-extension\"}}",
        "{\"conditions\":[]}", "{\"matchMode\":1,\"fallbackPolicy\":\"REJECT\"}",
        "{\"conditions\":[{\"paramType\":\"header\",\"paramName\":\"X-Optional\",\"operator\":\"isBlank\"}]}",
        "{\"conditions\":[{\"paramType\":\"uri\",\"operator\":\"regex\",\"paramValue\":\"/orders/.*\"}]}"
    })
    void testAcceptsValidBoundariesAndExtensionNames(final String patch) {
        assertDoesNotThrow(() -> CanaryConfigValidator.parseHandle(handle(patch)));
    }

    @ParameterizedTest
    @ValueSource(strings = {"[", "(?=a)a", "(?<=a)b", "(a)\\1"})
    void testRejectsRegexUnsupportedByGatewayWithFieldError(final String regex) {
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
                () -> CanaryConfigValidator.parseHandle(regexHandle(regex)));
        assertEquals("canary.conditions[0].paramValue is invalid for regex", exception.getMessage());
        assertEquals(PatternSyntaxException.class, exception.getCause().getClass());
    }

    @Test
    void testDefaultsRemainStable() {
        CanaryConfig config = CanaryConfigValidator.parseHandle(handle("{}")).getCanary();
        assertEquals(0, config.getPercentage());
        assertEquals(0, config.getMatchMode());
        assertEquals("STABLE", config.getFallbackPolicy());
    }

    private String regexHandle(final String regex) {
        JsonObject condition = new JsonObject();
        condition.addProperty("paramType", "uri");
        condition.addProperty("operator", "regex");
        condition.addProperty("paramValue", regex);
        JsonArray conditions = new JsonArray();
        conditions.add(condition);
        JsonObject config = new JsonObject();
        config.add("conditions", conditions);
        return handle(config.toString());
    }

    private String handle(final String patch) {
        JsonObject config = JsonParser.parseString("{\"stableLabels\":{\"release\":\"stable\"},\"canaryLabels\":{\"release\":\"canary\"}}")
                .getAsJsonObject();
        JsonParser.parseString(patch).getAsJsonObject().entrySet().forEach(entry -> config.add(entry.getKey(), entry.getValue()));
        JsonObject handle = new JsonObject();
        handle.add("canary", config);
        return handle.toString();
    }
}
