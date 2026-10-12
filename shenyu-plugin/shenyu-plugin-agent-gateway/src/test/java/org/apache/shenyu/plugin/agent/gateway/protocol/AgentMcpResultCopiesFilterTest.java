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

package org.apache.shenyu.plugin.agent.gateway.protocol;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.google.gson.JsonParser;
import org.apache.shenyu.common.dto.AgentGatewayResultFilterConfig;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.util.Set;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AgentMcpResultCopiesFilterTest {

    private final ObjectMapper mapper = new ObjectMapper();

    @Test
    void rebuildsAllCompleteCopiesAndKeepsPrivateInputUnchanged() throws Exception {
        ObjectNode original = result();
        original.withArray("content").addObject().put("type", "text").put("text", " { \"secret\": \"private\", \"keep\": 3 } ");
        original.putObject("_meta");
        original.put("resultType", "complete");
        original.put("isError", true);
        ObjectNode snapshot = original.deepCopy();
        ObjectNode filtered = filter().apply(original);
        assertEquals(snapshot, original);
        assertTrue(filtered.path("isError").asBoolean());
        assertTrue(filtered.path("_meta").isEmpty());
        for (var block : filtered.path("content")) {
            assertEquals(filtered.path("structuredContent"), mapper.readTree(block.get("text").asText()));
        }
        assertFalse(filtered.toString().contains("private"));
    }

    @ParameterizedTest
    @ValueSource(strings = {"private", "{}", "[]", "null", "\"\"", " ", "{\"secret\":\"private\",\"keep\":3} {}",
            "{\"secret\":\"private\",\"secret\":\"private\",\"keep\":3}", "{\"secret\":\"private\",\"keep\":4}",
            "{\"keep\":3}", "{\"secret\":\"private\",\"keep\":3,\"extra\":\"private\"}", "{broken"})
    void refusesUnverifiableTextWithoutLeakingItInExceptions(final String text) {
        ObjectNode original = result();
        ((ObjectNode) original.path("content").get(0)).put("text", text);
        IllegalArgumentException error = assertThrows(IllegalArgumentException.class, () -> filter().apply(original));
        assertFalse(error.getMessage().contains("private"));
        assertEquals(null, error.getCause());
        assertTrue(original.path("structuredContent").has("secret"));
    }

    @ParameterizedTest
    @ValueSource(strings = {"image", "audio", "resource", "resource_link", "text-extra", "annotations", "block-meta",
            "result-meta", "extension", "absent-structured", "absent-content", "content-object", "invalid-error", "invalid-type"})
    void refusesOpaqueChannelsEvenWhenTheFieldPathIsMissing(final String variant) {
        ObjectNode original = unsafe(variant);
        AgentGatewayResultFilterConfig config = AgentGatewayResultFilterConfig.parse(
                JsonParser.parseString("{\"crm.profile\":[\"/missing\"]}"), Set.of("crm.profile"));
        assertThrows(IllegalArgumentException.class, () -> new AgentMcpResultFilter(config, "crm.profile", null).apply(original));
    }

    @ParameterizedTest
    @ValueSource(strings = {"image", "audio", "resource", "resource_link", "text-extra", "annotations", "block-meta",
            "result-meta", "extension", "absent-structured", "absent-content", "content-object", "invalid-error", "invalid-type"})
    void disabledOrUnrelatedToolPlanRemainsPassThrough(final String variant) {
        ObjectNode original = unsafe(variant);
        assertTrue(original == new AgentMcpResultFilter(AgentGatewayResultFilterConfig.empty(), "crm.profile", null).apply(original));
        assertTrue(original == new AgentMcpResultFilter(config(), "other.tool", null).apply(original));
    }

    @Test
    void boundsCopyCountDepthAndTokenAllocation() {
        ObjectNode many = result();
        for (int index = 0; index < 32; index++) {
            many.withArray("content").add(many.path("content").get(0).deepCopy());
        }
        assertThrows(IllegalArgumentException.class, () -> filter().apply(many));
        ObjectNode deep = result();
        ((ObjectNode) deep.path("content").get(0)).put("text", "[".repeat(33) + "0" + "]".repeat(33));
        assertThrows(IllegalArgumentException.class, () -> filter().apply(deep));
        ObjectNode tokens = result();
        ((ObjectNode) tokens.path("content").get(0)).put("text", "{\"values\":[" + "0,".repeat(50000) + "0]}");
        assertThrows(IllegalArgumentException.class, () -> filter().apply(tokens));
    }

    @Test
    void thirtyTwoCallsNeverShareCopies() {
        ObjectNode original = result();
        IntStream.range(0, 32).parallel().forEach(index -> {
            ObjectNode filtered = filter().apply(original);
            ((ObjectNode) filtered.get("structuredContent")).put("owner", index);
            assertFalse(filtered.toString().contains("private"));
        });
        assertTrue(original.path("structuredContent").has("secret"));
        assertFalse(original.path("structuredContent").has("owner"));
    }

    private ObjectNode unsafe(final String variant) {
        ObjectNode result = result();
        ObjectNode block = (ObjectNode) result.path("content").get(0);
        switch (variant) {
            case "text-extra" -> block.put("extra", "private");
            case "annotations" -> block.putObject("annotations").put("audience", "private");
            case "block-meta" -> block.putObject("_meta").put("secret", "private");
            case "result-meta" -> result.putObject("_meta").put("secret", "private");
            case "extension" -> result.put("extension", "private");
            case "absent-structured" -> result.remove("structuredContent");
            case "absent-content" -> result.remove("content");
            case "content-object" -> result.putObject("content");
            case "invalid-error" -> result.put("isError", "private");
            case "invalid-type" -> result.put("resultType", "private");
            default -> block.put("type", variant);
        }
        return result;
    }

    private ObjectNode result() {
        ObjectNode result = mapper.createObjectNode().put("isError", false);
        result.putObject("structuredContent").put("keep", 3).put("secret", "private");
        result.putArray("content").addObject().put("type", "text").put("text", result.get("structuredContent").toString());
        return result;
    }

    private AgentGatewayResultFilterConfig config() {
        return AgentGatewayResultFilterConfig.parse(JsonParser.parseString("{\"crm.profile\":[\"/secret\"]}"), Set.of("crm.profile"));
    }

    private AgentMcpResultFilter filter() {
        return new AgentMcpResultFilter(config(), "crm.profile", null);
    }
}
