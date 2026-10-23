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

import org.apache.shenyu.common.dto.GraphData;
import org.apache.shenyu.common.dto.GraphNodeData;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Compiles a validated {@link GraphData} revision into an immutable {@link GraphPlan}.
 *
 * <p>Compilation is fail-closed: the graph is validated first and a {@link GraphValidationException}
 * aborts the compile, so an invalid graph never produces a plan (contract section 13 forbids any
 * fallback). On success every node's successors are pre-resolved once, so the GRAPH executor
 * (issue #7996) traverses the plan without re-deriving topology per request.</p>
 *
 * @since 2.7.2
 */
public final class GraphCompiler {

    private final GraphValidator validator;

    /**
     * Instantiates a new GraphCompiler.
     *
     * @param validator the validator used to fail closed on invalid graphs
     */
    public GraphCompiler(final GraphValidator validator) {
        this.validator = Objects.requireNonNull(validator, "validator");
    }

    /**
     * Compile a graph definition into an immutable plan.
     *
     * @param graph the graph definition to compile
     * @return the compiled, immutable {@link GraphPlan}
     * @throws GraphValidationException when the graph fails validation
     */
    public GraphPlan compile(final GraphData graph) {
        validator.validateOrThrow(graph);
        final Map<String, GraphNodeData> nodesById = new LinkedHashMap<>();
        final Map<String, List<String>> successors = new LinkedHashMap<>();
        for (GraphNodeData node : graph.getNodes()) {
            nodesById.put(node.getNodeId(), node);
            successors.put(node.getNodeId(), GraphTopology.successorsOf(node));
        }
        return new GraphPlan(graph, nodesById, successors);
    }
}
