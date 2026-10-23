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
import java.util.Set;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Test case for {@link GraphCompiler} and the immutability of {@link GraphPlan}.
 */
public class GraphCompilerTest {

    private static final Set<String> PLUGINS = Set.of("global", "divide", "grpc", "response", "mock");

    private final GraphCompiler compiler = new GraphCompiler(new GraphValidator(PLUGINS));

    @Test
    public void testCompileLinearPlanResolvesSuccessors() {
        final GraphPlan plan = compiler.compile(linearGraph());
        assertThat(plan.getGraphId(), is("g-test"));
        assertThat(plan.getNamespaceId(), is("ns-test"));
        assertThat(plan.getRevision(), is(1L));
        assertThat(plan.getEntryNodeId(), is("n0"));
        assertThat(plan.size(), is(4));
        assertThat(plan.getSuccessors("n0"), contains("n1"));
        assertThat(plan.getSuccessors("n2"), contains("end"));
        assertThat(plan.getSuccessors("end").isEmpty(), is(true));
        assertThat(plan.getNode("n0").getPluginName(), is("global"));
        assertThat(plan.getNode("ghost"), is(nullValue()));
    }

    @Test
    public void testCompileChoicePlanResolvesBranchThenDefault() {
        final GraphPlan plan = compiler.compile(choiceGraph());
        assertThat(plan.getSuccessors("c"), contains("n1", "n2"));
    }

    @Test
    public void testCompileInvalidGraphThrows() {
        final GraphData graph = linearGraph();
        graph.setEntryNodeId("ghost");
        assertThrows(GraphValidationException.class, () -> compiler.compile(graph));
    }

    @Test
    public void testPlanNodeIdsAreImmutable() {
        final GraphPlan plan = compiler.compile(linearGraph());
        assertThrows(UnsupportedOperationException.class, () -> plan.nodeIds().add("x"));
    }

    @Test
    public void testPlanSuccessorsAreImmutable() {
        final GraphPlan plan = compiler.compile(linearGraph());
        assertThrows(UnsupportedOperationException.class, () -> plan.getSuccessors("n0").add("x"));
    }

    @Test
    public void testEqualGraphsCompileToEqualPlans() {
        assertThat(compiler.compile(linearGraph()), is(compiler.compile(linearGraph())));
    }

    private GraphData linearGraph() {
        return graph("n0",
                plugin("n0", "global", "n1"),
                plugin("n1", "divide", "n2"),
                plugin("n2", "response", "end"),
                end("end"));
    }

    private GraphData choiceGraph() {
        final GraphNodeData choice = choice("c", "n2", branch("n1", condition("req_method", "grpc")));
        return graph("n0",
                plugin("n0", "global", "c"),
                choice,
                plugin("n1", "grpc", "n2"),
                plugin("n2", "response", "end"),
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
        final GraphNodeData node = new GraphNodeData();
        node.setNodeId(id);
        node.setType(GraphNodeTypeEnum.PLUGIN.getName());
        node.setPluginName(pluginName);
        node.setNext(next);
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
}
