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

import org.apache.shenyu.common.dto.GraphBranchData;
import org.apache.shenyu.common.dto.GraphData;
import org.apache.shenyu.common.dto.GraphNodeData;
import org.apache.shenyu.common.enums.GraphNodeTypeEnum;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Validates a {@link GraphData} against the Phase-1 contract before it may be compiled.
 *
 * <p>Validation is fail-closed and reports every violation at once (each naming the offending node id
 * or plugin name). It enforces the rules derived in {@code docs/plugin-graph-compatibility-audit.md}
 * (issue #7994) section 5: header/schema integrity, unique node ids, resource limits, per-type node
 * shape, installed-plugin catalog resolution, CHOICE determinism, reference integrity, acyclicity,
 * reachability, and path-wise checks (bounded length, no plugin executed twice on a path, termination
 * at an {@code END} node).</p>
 *
 * @since 2.7.2
 */
public final class GraphValidator {

    /**
     * the only schema major version accepted in Phase 1.
     */
    public static final int SUPPORTED_SCHEMA_VERSION = 1;

    private static final int MAX_ENUMERATED_PATHS = 100000;

    private final GraphLimits limits;

    private final Set<String> installedPlugins;

    /**
     * Instantiates a new GraphValidator with the default limits.
     *
     * @param installedPlugins the names of plugins installed in the runtime catalog
     */
    public GraphValidator(final Set<String> installedPlugins) {
        this(GraphLimits.defaults(), installedPlugins);
    }

    /**
     * Instantiates a new GraphValidator.
     *
     * @param limits           the resource limits to enforce
     * @param installedPlugins the names of plugins installed in the runtime catalog
     */
    public GraphValidator(final GraphLimits limits, final Set<String> installedPlugins) {
        this.limits = Objects.requireNonNull(limits, "limits");
        this.installedPlugins = Objects.requireNonNull(installedPlugins, "installedPlugins");
    }

    /**
     * Validate a graph, returning all violations.
     *
     * @param graph the graph to validate
     * @return the ordered, de-duplicated list of violations; empty when the graph is valid
     */
    public List<String> validate(final GraphData graph) {
        final List<String> violations = new ArrayList<>();
        if (Objects.isNull(graph)) {
            violations.add("graph must not be null");
            return violations;
        }
        validateHeader(graph, violations);
        final Map<String, GraphNodeData> byId = indexNodes(graph, violations);
        if (byId.isEmpty()) {
            return dedupe(violations);
        }
        validateLimits(graph, byId, violations);
        validateNodeShape(byId, violations);
        validateChoiceDeterminism(byId, violations);
        if (validateReferences(graph, byId, violations)) {
            final boolean acyclic = validateAcyclic(graph, byId, violations);
            validateReachability(graph, byId, violations);
            if (acyclic) {
                validatePaths(graph, byId, violations);
            }
        }
        return dedupe(violations);
    }

    /**
     * Validate a graph or throw.
     *
     * @param graph the graph to validate
     * @throws GraphValidationException when the graph has at least one violation
     */
    public void validateOrThrow(final GraphData graph) {
        final List<String> violations = validate(graph);
        if (!violations.isEmpty()) {
            throw new GraphValidationException(violations);
        }
    }

    private List<String> dedupe(final List<String> violations) {
        return violations.stream().distinct().collect(Collectors.toList());
    }

    private void validateHeader(final GraphData graph, final List<String> violations) {
        if (graph.getSchemaVersion() != SUPPORTED_SCHEMA_VERSION) {
            violations.add("unsupported schemaVersion " + graph.getSchemaVersion() + ", expected " + SUPPORTED_SCHEMA_VERSION);
        }
        if (GraphTopology.isBlank(graph.getGraphId())) {
            violations.add("graphId must not be blank");
        }
        if (GraphTopology.isBlank(graph.getNamespaceId())) {
            violations.add("namespaceId must not be blank");
        }
        if (Objects.isNull(graph.getNodes()) || graph.getNodes().isEmpty()) {
            violations.add("nodes must not be empty");
        }
        if (GraphTopology.isBlank(graph.getEntryNodeId())) {
            violations.add("entryNodeId must not be blank");
        }
    }

    private Map<String, GraphNodeData> indexNodes(final GraphData graph, final List<String> violations) {
        final Map<String, GraphNodeData> byId = new LinkedHashMap<>();
        if (Objects.isNull(graph.getNodes())) {
            return byId;
        }
        for (GraphNodeData node : graph.getNodes()) {
            if (Objects.isNull(node) || GraphTopology.isBlank(node.getNodeId())) {
                violations.add("every node must have a non-blank nodeId");
                continue;
            }
            if (byId.containsKey(node.getNodeId())) {
                violations.add("duplicate nodeId '" + node.getNodeId() + "'");
                continue;
            }
            if (!GraphNodeTypeEnum.isValidName(node.getType())) {
                violations.add("node '" + node.getNodeId() + "' has unknown type '" + node.getType() + "'");
            }
            byId.put(node.getNodeId(), node);
        }
        return byId;
    }

    private void validateLimits(final GraphData graph, final Map<String, GraphNodeData> byId, final List<String> violations) {
        if (byId.size() > limits.getMaxNodes()) {
            violations.add("graph '" + graph.getGraphId() + "' has " + byId.size() + " nodes, exceeding max " + limits.getMaxNodes());
        }
        for (GraphNodeData node : byId.values()) {
            if (!GraphTopology.isChoice(node)) {
                continue;
            }
            final int branches = Objects.isNull(node.getConditions()) ? 0 : node.getConditions().size();
            if (branches > limits.getMaxOutDegree()) {
                violations.add("CHOICE node '" + node.getNodeId() + "' has " + branches + " branches, exceeding max out-degree " + limits.getMaxOutDegree());
            }
            validateBranchConditionCount(node, violations);
        }
    }

    private void validateBranchConditionCount(final GraphNodeData node, final List<String> violations) {
        if (Objects.isNull(node.getConditions())) {
            return;
        }
        for (GraphBranchData branch : node.getConditions()) {
            if (Objects.isNull(branch)) {
                continue;
            }
            final int conditions = Objects.isNull(branch.getConditions()) ? 0 : branch.getConditions().size();
            if (conditions > limits.getMaxConditionsPerBranch()) {
                violations.add("branch to '" + branch.getTarget() + "' on CHOICE node '" + node.getNodeId() + "' has "
                        + conditions + " conditions, exceeding max " + limits.getMaxConditionsPerBranch());
            }
        }
    }

    private void validateNodeShape(final Map<String, GraphNodeData> byId, final List<String> violations) {
        for (GraphNodeData node : byId.values()) {
            if (GraphTopology.isPlugin(node)) {
                validatePluginNode(node, violations);
            } else if (GraphTopology.isChoice(node)) {
                validateChoiceNode(node, violations);
            } else if (GraphTopology.isEnd(node)) {
                validateEndNode(node, violations);
            }
        }
    }

    private void validatePluginNode(final GraphNodeData node, final List<String> violations) {
        if (GraphTopology.isBlank(node.getPluginName())) {
            violations.add("PLUGIN node '" + node.getNodeId() + "' must bind a pluginName");
        } else if (!installedPlugins.contains(node.getPluginName())) {
            violations.add((node.isRequired() ? "required " : "") + "PLUGIN node '" + node.getNodeId()
                    + "' binds plugin '" + node.getPluginName() + "' which is not installed (fail closed)");
        }
        if (GraphTopology.isBlank(node.getNext())) {
            violations.add("PLUGIN node '" + node.getNodeId() + "' must have exactly one successor 'next'");
        }
    }

    private void validateChoiceNode(final GraphNodeData node, final List<String> violations) {
        if (Objects.isNull(node.getConditions()) || node.getConditions().isEmpty()) {
            violations.add("CHOICE node '" + node.getNodeId() + "' must declare at least one branch");
        }
        if (GraphTopology.isBlank(node.getDefaultTarget())) {
            violations.add("CHOICE node '" + node.getNodeId() + "' must declare a defaultTarget (selection must be total)");
        }
    }

    private void validateEndNode(final GraphNodeData node, final List<String> violations) {
        if (!GraphTopology.isBlank(node.getNext())) {
            violations.add("END node '" + node.getNodeId() + "' must not declare a successor 'next'");
        }
        if (Objects.nonNull(node.getConditions()) && !node.getConditions().isEmpty()) {
            violations.add("END node '" + node.getNodeId() + "' must not declare branches");
        }
    }

    private void validateChoiceDeterminism(final Map<String, GraphNodeData> byId, final List<String> violations) {
        for (GraphNodeData node : byId.values()) {
            if (!GraphTopology.isChoice(node) || Objects.isNull(node.getConditions())) {
                continue;
            }
            final Set<GraphBranchData> seen = new HashSet<>();
            for (GraphBranchData branch : node.getConditions()) {
                if (Objects.nonNull(branch) && !seen.add(branch)) {
                    violations.add("CHOICE node '" + node.getNodeId() + "' declares a duplicate branch to '"
                            + branch.getTarget() + "' (ambiguous authoring)");
                }
            }
        }
    }

    private boolean validateReferences(final GraphData graph, final Map<String, GraphNodeData> byId, final List<String> violations) {
        boolean ok = true;
        if (!GraphTopology.isBlank(graph.getEntryNodeId()) && !byId.containsKey(graph.getEntryNodeId())) {
            violations.add("entryNodeId '" + graph.getEntryNodeId() + "' does not reference a node");
            ok = false;
        }
        for (GraphNodeData node : byId.values()) {
            if (GraphTopology.isPlugin(node) && !GraphTopology.isBlank(node.getNext()) && !byId.containsKey(node.getNext())) {
                violations.add("PLUGIN node '" + node.getNodeId() + "' successor 'next' -> unknown node '" + node.getNext() + "'");
                ok = false;
            }
            if (GraphTopology.isChoice(node)) {
                ok = validateChoiceReferences(node, byId, violations) && ok;
            }
        }
        return ok;
    }

    private boolean validateChoiceReferences(final GraphNodeData node, final Map<String, GraphNodeData> byId, final List<String> violations) {
        boolean ok = true;
        if (Objects.nonNull(node.getConditions())) {
            for (GraphBranchData branch : node.getConditions()) {
                if (Objects.isNull(branch) || GraphTopology.isBlank(branch.getTarget()) || !byId.containsKey(branch.getTarget())) {
                    final String target = Objects.isNull(branch) ? null : branch.getTarget();
                    violations.add("CHOICE node '" + node.getNodeId() + "' branch target '" + target + "' -> unknown node");
                    ok = false;
                }
            }
        }
        if (!GraphTopology.isBlank(node.getDefaultTarget()) && !byId.containsKey(node.getDefaultTarget())) {
            violations.add("CHOICE node '" + node.getNodeId() + "' defaultTarget '" + node.getDefaultTarget() + "' -> unknown node");
            ok = false;
        }
        return ok;
    }

    private boolean validateAcyclic(final GraphData graph, final Map<String, GraphNodeData> byId, final List<String> violations) {
        final Set<String> visited = new HashSet<>();
        final Set<String> onStack = new LinkedHashSet<>();
        boolean acyclic = true;
        for (String start : byId.keySet()) {
            if (!visited.contains(start) && hasCycle(start, byId, visited, onStack)) {
                violations.add("graph '" + graph.getGraphId() + "' contains a cycle reachable from node '" + start + "'");
                acyclic = false;
            }
        }
        return acyclic;
    }

    private boolean hasCycle(final String nodeId, final Map<String, GraphNodeData> byId, final Set<String> visited, final Set<String> onStack) {
        visited.add(nodeId);
        onStack.add(nodeId);
        final GraphNodeData node = byId.get(nodeId);
        if (Objects.nonNull(node)) {
            for (String next : GraphTopology.successorsOf(node)) {
                if (onStack.contains(next)) {
                    return true;
                }
                if (!visited.contains(next) && byId.containsKey(next) && hasCycle(next, byId, visited, onStack)) {
                    return true;
                }
            }
        }
        onStack.remove(nodeId);
        return false;
    }

    private void validateReachability(final GraphData graph, final Map<String, GraphNodeData> byId, final List<String> violations) {
        final Set<String> reachable = new HashSet<>();
        final Deque<String> queue = new ArrayDeque<>();
        queue.add(graph.getEntryNodeId());
        while (!queue.isEmpty()) {
            final String current = queue.poll();
            if (!reachable.add(current)) {
                continue;
            }
            final GraphNodeData node = byId.get(current);
            if (Objects.isNull(node)) {
                continue;
            }
            for (String next : GraphTopology.successorsOf(node)) {
                if (byId.containsKey(next)) {
                    queue.add(next);
                }
            }
        }
        for (String nodeId : byId.keySet()) {
            if (!reachable.contains(nodeId)) {
                violations.add("node '" + nodeId + "' is not reachable from entryNodeId '" + graph.getEntryNodeId() + "'");
            }
        }
    }

    private void validatePaths(final GraphData graph, final Map<String, GraphNodeData> byId, final List<String> violations) {
        final int[] pathCount = {0};
        final boolean[] overflow = {false};
        walkPaths(graph, graph.getEntryNodeId(), byId, new ArrayList<>(), new ArrayList<>(), pathCount, overflow, violations);
        if (pathCount[0] == 0) {
            violations.add("graph '" + graph.getGraphId() + "' has no path terminating at an END node");
        }
        if (overflow[0]) {
            violations.add("graph '" + graph.getGraphId() + "' exceeds the maximum enumerable path count " + MAX_ENUMERATED_PATHS);
        }
    }

    private void walkPaths(final GraphData graph, final String nodeId, final Map<String, GraphNodeData> byId,
                           final List<String> path, final List<String> plugins, final int[] pathCount,
                           final boolean[] overflow, final List<String> violations) {
        if (overflow[0] || pathCount[0] > MAX_ENUMERATED_PATHS) {
            overflow[0] = true;
            return;
        }
        final GraphNodeData node = byId.get(nodeId);
        if (Objects.isNull(node)) {
            return;
        }
        path.add(nodeId);
        final boolean tracksPlugin = GraphTopology.isPlugin(node) && !GraphTopology.isBlank(node.getPluginName());
        if (tracksPlugin) {
            plugins.add(node.getPluginName());
        }
        if (GraphTopology.isEnd(node)) {
            checkPath(graph, path, plugins, pathCount, violations);
        } else {
            for (String next : GraphTopology.successorsOf(node)) {
                walkPaths(graph, next, byId, path, plugins, pathCount, overflow, violations);
            }
        }
        path.remove(path.size() - 1);
        if (tracksPlugin) {
            plugins.remove(plugins.size() - 1);
        }
    }

    private void checkPath(final GraphData graph, final List<String> path, final List<String> plugins,
                           final int[] pathCount, final List<String> violations) {
        pathCount[0] = pathCount[0] + 1;
        if (path.size() > limits.getMaxPathLength()) {
            violations.add("graph '" + graph.getGraphId() + "' has a path of length " + path.size()
                    + " exceeding max " + limits.getMaxPathLength() + ": " + path);
        }
        final Set<String> seen = new HashSet<>();
        for (String pluginName : plugins) {
            if (!seen.add(pluginName)) {
                violations.add("graph '" + graph.getGraphId() + "' path " + path + " executes plugin '"
                        + pluginName + "' more than once");
            }
        }
    }
}
