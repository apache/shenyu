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

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;
import reactor.test.StepVerifier;

import java.util.ArrayList;
import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AgentMcpStructuredContentFilterTest {

    private final ObjectMapper mapper = new ObjectMapper();

    @ParameterizedTest
    @ValueSource(strings = {"", "secret", "#/secret", "~secret", "/a~", "/a~2", "/a~3b"})
    void rejectsInvalidPaths(final String path) {
        assertThrows(IllegalArgumentException.class, () -> new AgentMcpStructuredContentFilter(List.of(path)));
    }

    @Test
    void rejectsNullPlanAndNullPath() {
        assertThrows(IllegalArgumentException.class, () -> new AgentMcpStructuredContentFilter(null));
        List<String> paths = new ArrayList<>();
        paths.add(null);
        assertThrows(IllegalArgumentException.class, () -> new AgentMcpStructuredContentFilter(paths));
    }

    @Test
    void rejectsDuplicatePaths() {
        assertThrows(IllegalArgumentException.class, () -> new AgentMcpStructuredContentFilter(List.of("/secret", "/secret")));
    }

    @Test
    void rejectsExcessivePathCountLengthAndDepth() {
        List<String> paths = new ArrayList<>();
        for (int index = 0; index < 33; index++) {
            paths.add("/field" + index);
        }
        assertThrows(IllegalArgumentException.class, () -> new AgentMcpStructuredContentFilter(paths));
        assertThrows(IllegalArgumentException.class, () -> new AgentMcpStructuredContentFilter(List.of("/" + "a".repeat(512))));
        assertThrows(IllegalArgumentException.class, () -> new AgentMcpStructuredContentFilter(List.of("/a".repeat(17))));
    }

    @Test
    void removesOnlyStructuredFieldsAndPreservesProtocolData() throws JsonProcessingException {
        ObjectNode source = result("{\"customer\":{\"name\":\"Ada\",\"secret\":\"private\"},\"secret\":\"top\",\"keep\":7}");
        source.put("isError", true);
        source.putObject("_meta").put("secret", "metadata");
        source.putArray("content").addObject().put("type", "text").put("text", "{\"secret\":\"still-present\"}");
        source.put("resultType", "complete");
        source.put("extension", "unchanged");
        final ObjectNode snapshot = source.deepCopy();
        ObjectNode filtered = new AgentMcpStructuredContentFilter(List.of("/secret", "/customer/secret")).filter(source);
        assertFalse(filtered.path("structuredContent").has("secret"));
        assertFalse(filtered.path("structuredContent").path("customer").has("secret"));
        assertEquals("Ada", filtered.path("structuredContent").path("customer").path("name").asText());
        assertEquals(7, filtered.path("structuredContent").path("keep").asInt());
        assertEquals(source.path("content"), filtered.path("content"));
        assertEquals(source.path("_meta"), filtered.path("_meta"));
        assertEquals(source.path("isError"), filtered.path("isError"));
        assertEquals(source.path("resultType"), filtered.path("resultType"));
        assertEquals(source.path("extension"), filtered.path("extension"));
        assertEquals(snapshot, source);
        assertNotSame(source, filtered);
    }

    @Test
    void disabledPlanDoesNotCopyOrRejectAnOtherwiseUntouchedResult() throws JsonProcessingException {
        ObjectNode source = result("null");
        assertSame(source, new AgentMcpStructuredContentFilter(List.of()).filter(source));
    }

    @Test
    void supportsEscapedSlashesTildesAndEmptyKeys() throws JsonProcessingException {
        ObjectNode source = result("{\"a/b\":1,\"a~b\":2,\"\":3,\"~1\":4,\"keep\":5}");
        ObjectNode filtered = new AgentMcpStructuredContentFilter(List.of("/a~1b", "/a~0b", "/", "/~01")).filter(source);
        assertEquals(mapper.readTree("{\"keep\":5}"), filtered.path("structuredContent"));
        assertEquals(5, source.path("structuredContent").size());
    }

    @Test
    void treatsNumericAndWildcardObjectKeysAsLiterals() throws JsonProcessingException {
        ObjectNode source = result("{\"01\":1,\"*\":2,\"secret\":3}");
        ObjectNode filtered = new AgentMcpStructuredContentFilter(List.of("/01", "/*")).filter(source);
        assertEquals(mapper.readTree("{\"secret\":3}"), filtered.path("structuredContent"));
    }

    @Test
    void traversesArrayObjectsWithoutRemovingOrShiftingElements() throws JsonProcessingException {
        ObjectNode source = result("{\"items\":[{\"secret\":\"a\",\"id\":1},{\"secret\":\"b\",\"id\":2}]}");
        ObjectNode filtered = new AgentMcpStructuredContentFilter(List.of("/items/0/secret", "/items/1/secret")).filter(source);
        assertEquals(2, filtered.path("structuredContent").path("items").size());
        assertEquals(1, filtered.path("structuredContent").path("items").get(0).path("id").asInt());
        assertEquals(2, filtered.path("structuredContent").path("items").get(1).path("id").asInt());
        assertTrue(source.path("structuredContent").path("items").get(0).has("secret"));
    }

    @ParameterizedTest
    @ValueSource(strings = {"/items/0", "/items/-", "/items/01/secret", "/items/-1/secret",
            "/items/+0/secret", "/items/2147483648/secret"})
    void rejectsArrayElementDeletionAndInvalidArrayIndexes(final String path) throws JsonProcessingException {
        ObjectNode source = result("{\"items\":[{\"secret\":\"private\"}]}");
        ObjectNode snapshot = source.deepCopy();
        assertThrows(IllegalArgumentException.class, () -> new AgentMcpStructuredContentFilter(List.of(path)).filter(source));
        assertEquals(snapshot, source);
    }

    @Test
    void missingOptionalPathsAndOutOfRangeParentsAreNoOps() throws JsonProcessingException {
        ObjectNode source = result("{\"items\":[{\"name\":\"Ada\"}]}");
        ObjectNode filtered = new AgentMcpStructuredContentFilter(List.of("/missing", "/absent/secret", "/items/5/secret")).filter(source);
        assertEquals(source, filtered);
        assertNotSame(source, filtered);
    }

    @Test
    void resultWithoutStructuredContentRetainsItsError() {
        ObjectNode source = mapper.createObjectNode().put("isError", true);
        source.putArray("content").addObject().put("type", "text").put("text", "tool failed");
        assertEquals(source, new AgentMcpStructuredContentFilter(List.of("/secret")).filter(source));
    }

    @ParameterizedTest
    @ValueSource(strings = {"null", "[]", "1", "true", "\"value\""})
    void rejectsNonObjectStructuredContent(final String structured) throws JsonProcessingException {
        ObjectNode source = result(structured);
        assertThrows(IllegalArgumentException.class, () -> new AgentMcpStructuredContentFilter(List.of("/secret")).filter(source));
    }

    @Test
    void incompatibleTraversalFailsWithoutMutatingOrLeakingInput() throws JsonProcessingException {
        ObjectNode source = result("{\"secret\":\"private-value\",\"customer\":null}");
        ObjectNode snapshot = source.deepCopy();
        IllegalArgumentException failure = assertThrows(IllegalArgumentException.class,
                () -> new AgentMcpStructuredContentFilter(List.of("/secret", "/customer/secret")).filter(source));
        assertEquals(snapshot, source);
        assertFalse(failure.getMessage().contains("private-value"));
    }

    @Test
    void snapshotsThePlanAndOwnsEachFilteredResult() throws JsonProcessingException {
        List<String> paths = new ArrayList<>(List.of("/secret"));
        AgentMcpStructuredContentFilter filter = new AgentMcpStructuredContentFilter(paths);
        paths.clear();
        paths.add("/keep");
        ObjectNode source = result("{\"secret\":\"private\",\"keep\":1}");
        ObjectNode first = filter.filter(source);
        ObjectNode second = filter.filter(source);
        ((ObjectNode) first.path("structuredContent")).put("keep", 99);
        assertEquals(1, second.path("structuredContent").path("keep").asInt());
        assertTrue(source.path("structuredContent").has("secret"));
        assertFalse(second.path("structuredContent").has("secret"));
    }

    @Test
    void overlappingDeletesHaveNoOrderingDependentEffect() throws JsonProcessingException {
        ObjectNode source = result("{\"customer\":{\"secret\":\"private\"},\"keep\":1}");
        ObjectNode first = new AgentMcpStructuredContentFilter(List.of("/customer", "/customer/secret")).filter(source);
        ObjectNode second = new AgentMcpStructuredContentFilter(List.of("/customer/secret", "/customer")).filter(source);
        assertEquals(first, second);
        assertFalse(first.path("structuredContent").has("customer"));
    }

    @Test
    void rejectsExcessiveDepthBeforeCopying() {
        ObjectNode source = mapper.createObjectNode();
        ObjectNode nested = source.putObject("structuredContent");
        for (int index = 0; index < 33; index++) {
            nested = nested.putObject("nested");
        }
        assertThrows(IllegalArgumentException.class, () -> new AgentMcpStructuredContentFilter(List.of("/secret")).filter(source));
    }

    @Test
    void rejectsExcessiveNodesAndCharactersBeforeCopying() {
        ObjectNode source = mapper.createObjectNode();
        ArrayNode values = source.putObject("structuredContent").putArray("values");
        for (int index = 0; index < 16384; index++) {
            values.add(index);
        }
        AgentMcpStructuredContentFilter filter = new AgentMcpStructuredContentFilter(List.of("/secret"));
        assertThrows(IllegalArgumentException.class, () -> filter.filter(source));
        ObjectNode oversized = mapper.createObjectNode();
        oversized.putObject("structuredContent").put("secret", "x".repeat(2097153));
        assertThrows(IllegalArgumentException.class, () -> filter.filter(oversized));
    }

    @Test
    void rejectsCyclesAndUnsupportedNodesBeforeDeepCopying() {
        ObjectNode source = mapper.createObjectNode();
        source.set("structuredContent", source);
        AgentMcpStructuredContentFilter filter = new AgentMcpStructuredContentFilter(List.of("/secret"));
        assertThrows(IllegalArgumentException.class, () -> filter.filter(source));
        ObjectNode pojo = mapper.createObjectNode();
        pojo.putObject("structuredContent").putPOJO("secret", new Object());
        assertThrows(IllegalArgumentException.class, () -> filter.filter(pojo));
    }

    @Test
    void concurrentCallsWithDifferentPlansDoNotShareResults() throws JsonProcessingException {
        ObjectNode source = result("{\"secret\":\"private\",\"keep\":\"public\"}");
        AgentMcpStructuredContentFilter privateFilter = new AgentMcpStructuredContentFilter(List.of("/secret"));
        AgentMcpStructuredContentFilter publicFilter = new AgentMcpStructuredContentFilter(List.of("/keep"));
        StepVerifier.create(Flux.range(0, 32).flatMap(index -> Mono.fromSupplier(() -> {
            ObjectNode filtered = (index % 2 == 0 ? privateFilter : publicFilter).filter(source);
            assertEquals(index % 2 != 0, filtered.path("structuredContent").has("secret"));
            assertEquals(index % 2 == 0, filtered.path("structuredContent").has("keep"));
            return filtered;
        }).subscribeOn(Schedulers.parallel())).collectList())
                .assertNext(results -> {
                    assertEquals(32, results.size());
                    Set<ObjectNode> identities = Collections.newSetFromMap(new IdentityHashMap<>());
                    identities.addAll(results);
                    assertEquals(32, identities.size());
                }).verifyComplete();
        assertEquals(2, source.path("structuredContent").size());
    }

    private ObjectNode result(final String structured) throws JsonProcessingException {
        ObjectNode result = mapper.createObjectNode();
        result.set("structuredContent", mapper.readTree(structured));
        return result;
    }
}
