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

package org.apache.shenyu.common.dto;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AgentGatewayResultFilterConfigTest {

    @Test
    void acceptsAbsentAndEmptyPolicies() {
        assertTrue(AgentGatewayMcpConfig.parse(new JsonObject(), true).getResultFilters().getPlan("crm.profile").isEmpty());
        assertTrue(config("{\"allowedTools\":[\"crm.profile\"],\"structuredContentRemovals\":{}}", true)
                .getResultFilters().getPlan("crm.profile").isEmpty());
    }

    @Test
    void capturesDecodedImmutableExactToolPlans() {
        JsonObject input = JsonParser.parseString("{\"allowedTools\":[\"crm.profile\"],"
                + "\"structuredContentRemovals\":{\"crm.profile\":[\"/a~1b/~0\",\"/customer/secret\"]}}").getAsJsonObject();
        AgentGatewayMcpConfig snapshot = AgentGatewayMcpConfig.parse(input, true);
        input.getAsJsonObject("structuredContentRemovals").remove("crm.profile");
        assertEquals(List.of(List.of("a/b", "~"), List.of("customer", "secret")), snapshot.getResultFilters().getPlan("crm.profile"));
        assertTrue(snapshot.getResultFilters().getPlan("profile").isEmpty());
        assertThrows(UnsupportedOperationException.class, () -> snapshot.getResultFilters().getPlan("crm.profile").clear());
        assertThrows(UnsupportedOperationException.class, () -> snapshot.getResultFilters().getPlan("crm.profile").get(0).clear());
    }

    @ParameterizedTest
    @ValueSource(strings = {"null", "[]", "true", "{\"other.profile\":[\"/secret\"]}", "{\"profile\":[\"/secret\"]}",
            "{\"crm.profile\":null}", "{\"crm.profile\":[]}", "{\"crm.profile\":true}", "{\"crm.profile\":[null]}",
            "{\"crm.profile\":[1]}", "{\"crm.profile\":[\"\"]}", "{\"crm.profile\":[\"secret\"]}",
            "{\"crm.profile\":[\"/a~2\"]}", "{\"crm.profile\":[\"/secret\",\"/secret\"]}"})
    void rejectsInvalidPlansEquallyAtAdminAndGateway(final String value) {
        String input = "{\"allowedTools\":[\"crm.profile\"],\"structuredContentRemovals\":" + value + "}";
        assertThrows(IllegalArgumentException.class, () -> config(input, true));
        assertThrows(IllegalArgumentException.class, () -> config(input, false));
    }

    @Test
    void enforcesPerToolAndTotalPathLimits() {
        JsonObject input = new JsonObject();
        input.add("allowedTools", new com.google.gson.JsonArray());
        JsonObject policy = new JsonObject();
        input.add("structuredContentRemovals", policy);
        for (int tool = 0; tool < 5; tool++) {
            String name = "server.tool" + tool;
            input.getAsJsonArray("allowedTools").add(name);
            var paths = new com.google.gson.JsonArray();
            for (int path = 0; path < 32; path++) {
                paths.add("/field" + path);
            }
            policy.add(name, paths);
        }
        assertThrows(IllegalArgumentException.class, () -> AgentGatewayMcpConfig.parse(input, true));
        policy.remove("server.tool4");
        AgentGatewayMcpConfig.parse(input, true);
        policy.getAsJsonArray("server.tool0").add("/extra");
        assertThrows(IllegalArgumentException.class, () -> AgentGatewayMcpConfig.parse(input, true));
    }

    private AgentGatewayMcpConfig config(final String input, final boolean strict) {
        return AgentGatewayMcpConfig.parse(JsonParser.parseString(input), strict);
    }
}
