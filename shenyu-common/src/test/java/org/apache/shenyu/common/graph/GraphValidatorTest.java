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

package org.apache.shenyu.common.graph;

import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.GraphBranchData;
import org.apache.shenyu.common.dto.GraphData;
import org.apache.shenyu.common.dto.GraphNodeData;
import org.apache.shenyu.common.enums.GraphNodeTypeEnum;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Test case for {@link GraphValidator}.
 */
public class GraphValidatorTest {

    private static final Set<String> PLUGINS = Set.of(
            "global", "sign", "divide", "response", "grpc", "mock", "rewrite", "uri");

    private final GraphValidator validator = new GraphValidator(PLUGINS);

    @Test
    public void testNullGraphRejected() {
        assertThat(validator.validate(null), hasItem(containsString("must not be null")));
    }

    @Test
    public void testValidLinearGraph() {
        final GraphData graph = linearGraph();
        assertThat(validator.validate(graph), is(empty()));
    }

    @Test
    public void testValidChoiceGraph() {
        final GraphData graph = choiceGraph();
        assertThat(validator.validate(graph), is(empty()));
    }

    @Test
    public void testUnsupportedSchemaVersion() {
        final GraphData graph = linearGraph();
        graph.setSchemaVersion(2);
        assertThat(validator.validate(graph), hasItem(containsString("unsupported schemaVersion")));
    }

    @Test
    public void testBlankIdentityRejected() {
        final GraphData graph = linearGraph();
        graph.setGraphId(" ");
        graph.setNamespaceId(null);
        graph.setEntryNodeId("");
        final List<String> violations = validator.validate(graph);
        assertThat(violations, hasItem(containsString("graphId must not be blank")));
        assertThat(violations, hasItem(containsString("namespaceId must not be blank")));
        assertThat(violations, hasItem(containsString("entryNodeId must not be blank")));
    }

    @Test
    public void testEmptyNodesRejected() {
        final GraphData graph = linearGraph();
        graph.setNodes(Collections.emptyList());
        assertThat(validator.validate(graph), hasItem(containsString("nodes must not be empty")));
    }

    @Test
    public void testDuplicateNodeIdRejected() {
        final GraphData graph = graph("n0",
                plugin("n0", "global", "n1"),
                plugin("n0", "sign", "end"),
                end("end"));
        assertThat(validator.validate(graph), hasItem(containsString("duplicate nodeId 'n0'")));
    }

    @Test
    public void testUnknownNodeTypeRejected() {
        final GraphNodeData bad = new GraphNodeData();
        bad.setNodeId("n1");
        bad.setType("SWITCH");
        final GraphData graph = graph("n0", plugin("n0", "global", "n1"), bad, end("end"));
        assertThat(validator.validate(graph), hasItem(containsString("unknown type 'SWITCH'")));
    }

    @Test
    public void testEntryReferencesMissingNode() {
        final GraphData graph = linearGraph();
        graph.setEntryNodeId("ghost");
        assertThat(validator.validate(graph), hasItem(containsString("entryNodeId 'ghost' does not reference a node")));
    }

    @Test
    public void testDanglingPluginNextRejected() {
        final GraphData graph = graph("n0",
                plugin("n0", "global", "ghost"),
                end("end"));
        assertThat(validator.validate(graph), hasItem(containsString("successor 'next' -> unknown node 'ghost'")));
    }

    @Test
    public void testChoiceWithoutDefaultTargetRejected() {
        final GraphNodeData choice = choice("c", null, branch("n1", condition("req_method", "GET")));
        final GraphData graph = graph("n0",
                plugin("n0", "global", "c"),
                choice,
                plugin("n1", "divide", "end"),
                end("end"));
        assertThat(validator.validate(graph), hasItem(containsString("must declare a defaultTarget")));
    }

    @Test
    public void testChoiceBranchUnknownTargetRejected() {
        final GraphNodeData choice = choice("c", "n1", branch("ghost", condition("req_method", "GET")));
        final GraphData graph = graph("n0",
                plugin("n0", "global", "c"),
                choice,
                plugin("n1", "divide", "end"),
                end("end"));
        assertThat(validator.validate(graph), hasItem(containsString("branch target 'ghost' -> unknown node")));
    }

    @Test
    public void testDuplicateBranchesRejected() {
        final GraphNodeData choice = choice("c", "n1",
                branch("n1", condition("req_method", "GET")),
                branch("n1", condition("req_method", "GET")));
        final GraphData graph = graph("n0",
                plugin("n0", "global", "c"),
                choice,
                plugin("n1", "divide", "end"),
                end("end"));
        assertThat(validator.validate(graph), hasItem(containsString("duplicate branch")));
    }

    @Test
    public void testCycleRejected() {
        final GraphData graph = graph("n0",
                plugin("n0", "global", "n1"),
                plugin("n1", "divide", "n0"),
                end("end"));
        assertThat(validator.validate(graph), hasItem(containsString("contains a cycle")));
    }

    @Test
    public void testUnreachableNodeRejected() {
        final GraphData graph = graph("n0",
                plugin("n0", "global", "end"),
                plugin("orphan", "mock", "end"),
                end("end"));
        assertThat(validator.validate(graph), hasItem(containsString("node 'orphan' is not reachable")));
    }

    @Test
    public void testNoPathToEndRejected() {
        final GraphData graph = graph("n0",
                plugin("n0", "global", "n1"),
                plugin("n1", "divide", null));
        final List<String> violations = validator.validate(graph);
        assertThat(violations, hasItem(containsString("must have exactly one successor")));
    }

    @Test
    public void testPluginWithoutNameRejected() {
        final GraphData graph = graph("n0",
                plugin("n0", null, "end"),
                end("end"));
        assertThat(validator.validate(graph), hasItem(containsString("must bind a pluginName")));
    }

    @Test
    public void testEndNodeWithNextRejected() {
        final GraphNodeData badEnd = end("end");
        badEnd.setNext("n0");
        final GraphData graph = graph("n0", plugin("n0", "global", "end"), badEnd);
        assertThat(validator.validate(graph), hasItem(containsString("END node 'end' must not declare a successor")));
    }

    @Test
    public void testRequiredPluginMissingFromCatalogFailsClosed() {
        final GraphNodeData required = plugin("n0", "global", "end");
        required.setRequired(true);
        final GraphData graph = graph("n0", required, end("end"));
        final GraphValidator emptyCatalog = new GraphValidator(Set.of());
        assertThat(emptyCatalog.validate(graph), hasItem(containsString("required PLUGIN node 'n0' binds plugin 'global' which is not installed")));
    }

    @Test
    public void testOptionalPluginMissingFromCatalogRejected() {
        final GraphData graph = graph("n0", plugin("n0", "unknownPlugin", "end"), end("end"));
        assertThat(validator.validate(graph), hasItem(containsString("which is not installed")));
    }

    @Test
    public void testDuplicatePluginOnPathRejected() {
        final GraphNodeData choice = choice("c", "n2",
                branch("n1", condition("req_method", "GET")));
        final GraphData graph = graph("n0",
                plugin("n0", "divide", "c"),
                choice,
                plugin("n1", "divide", "end"),
                plugin("n2", "mock", "end"),
                end("end"));
        assertThat(validator.validate(graph), hasItem(containsString("executes plugin 'divide' more than once")));
    }

    @Test
    public void testMaxNodesLimit() {
        final GraphValidator small = new GraphValidator(limits(2, 16, 32, 64), PLUGINS);
        assertThat(small.validate(linearGraph()), hasItem(containsString("exceeding max 2")));
    }

    @Test
    public void testMaxOutDegreeLimit() {
        final GraphValidator small = new GraphValidator(limits(128, 1, 32, 64), PLUGINS);
        final GraphNodeData choice = choice("c", "n3",
                branch("n1", condition("req_method", "GET")),
                branch("n2", condition("req_method", "POST")));
        final GraphData graph = graph("n0",
                plugin("n0", "global", "c"),
                choice,
                plugin("n1", "divide", "end"),
                plugin("n2", "mock", "end"),
                plugin("n3", "uri", "end"),
                end("end"));
        assertThat(small.validate(graph), hasItem(containsString("exceeding max out-degree 1")));
    }

    @Test
    public void testMaxConditionsPerBranchLimit() {
        final GraphValidator small = new GraphValidator(limits(128, 16, 1, 64), PLUGINS);
        final GraphNodeData choice = choice("c", "n1",
                branch("n1", condition("req_method", "GET"), condition("uri", "/x")));
        final GraphData graph = graph("n0",
                plugin("n0", "global", "c"),
                choice,
                plugin("n1", "divide", "end"),
                end("end"));
        assertThat(small.validate(graph), hasItem(containsString("exceeding max 1")));
    }

    @Test
    public void testMaxPathLengthLimit() {
        final GraphValidator small = new GraphValidator(limits(128, 16, 32, 2), PLUGINS);
        assertThat(small.validate(linearGraph()), hasItem(containsString("exceeding max 2")));
    }

    @Test
    public void testValidateOrThrowRaises() {
        final GraphData graph = linearGraph();
        graph.setEntryNodeId("ghost");
        final GraphValidationException ex = assertThrows(GraphValidationException.class, () -> validator.validateOrThrow(graph));
        assertThat(ex.getViolations(), hasItem(containsString("does not reference a node")));
    }

    @Test
    public void testValidateOrThrowPassesForValidGraph() {
        validator.validateOrThrow(linearGraph());
        validator.validateOrThrow(choiceGraph());
    }

    private GraphData linearGraph() {
        return graph("n0",
                plugin("n0", "global", "n1", true),
                plugin("n1", "sign", "n2", true),
                plugin("n2", "divide", "n3"),
                plugin("n3", "response", "end"),
                end("end"));
    }

    private GraphData choiceGraph() {
        final GraphNodeData choice = choice("c", "n2", branch("n1", condition("req_method", "grpc")));
        return graph("n0",
                plugin("n0", "global", "c", true),
                choice,
                plugin("n1", "grpc", "n3"),
                plugin("n2", "divide", "n3"),
                plugin("n3", "response", "end"),
                end("end"));
    }

    private GraphData graph(final String entry, final GraphNodeData... nodes) {
        final GraphData graph = new GraphData();
        graph.setSchemaVersion(GraphValidator.SUPPORTED_SCHEMA_VERSION);
        graph.setNamespaceId("ns-test");
        graph.setGraphId("g-test");
        graph.setRevision(1L);
        graph.setContentHash("hash");
        graph.setEntryNodeId(entry);
        graph.setNodes(new ArrayList<>(Arrays.asList(nodes)));
        return graph;
    }

    private GraphNodeData plugin(final String id, final String pluginName, final String next) {
        return plugin(id, pluginName, next, false);
    }

    private GraphNodeData plugin(final String id, final String pluginName, final String next, final boolean required) {
        final GraphNodeData node = new GraphNodeData();
        node.setNodeId(id);
        node.setType(GraphNodeTypeEnum.PLUGIN.getName());
        node.setPluginName(pluginName);
        node.setNext(next);
        node.setRequired(required);
        return node;
    }

    private GraphNodeData end(final String id) {
        final GraphNodeData node = new GraphNodeData();
        node.setNodeId(id);
        node.setType(GraphNodeTypeEnum.END.getName());
        return node;
    }

    private GraphNodeData choice(final String id, final String defaultTarget, final GraphBranchData... branches) {
        final GraphNodeData node = new GraphNodeData();
        node.setNodeId(id);
        node.setType(GraphNodeTypeEnum.CHOICE.getName());
        node.setDefaultTarget(defaultTarget);
        node.setConditions(new ArrayList<>(Arrays.asList(branches)));
        return node;
    }

    private GraphBranchData branch(final String target, final ConditionData... conditions) {
        return new GraphBranchData(new ArrayList<>(Arrays.asList(conditions)), 0, target);
    }

    private ConditionData condition(final String paramType, final String paramValue) {
        final ConditionData data = new ConditionData();
        data.setParamType(paramType);
        data.setOperator("match");
        data.setParamValue(paramValue);
        return data;
    }

    private GraphLimits limits(final int maxNodes, final int maxOutDegree, final int maxConditions, final int maxPathLength) {
        return new GraphLimits(maxNodes, maxOutDegree, maxConditions, maxPathLength,
                GraphLimits.DEFAULT_MAX_FORK_RECURSION_DEPTH, GraphLimits.DEFAULT_MAX_RETAINED_REVISIONS,
                GraphLimits.DEFAULT_MAX_GRAPHS_PER_NAMESPACE);
    }
}
