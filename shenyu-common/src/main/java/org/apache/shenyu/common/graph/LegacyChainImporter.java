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
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.GraphNodeTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Imports the legacy sorted plugin chain into an equivalent linear {@link GraphData}.
 *
 * <p>This gives operators a migration path from the {@code PluginData.sort}-ordered chain to an
 * explicit graph: the produced graph is a single common prefix of {@code PLUGIN} nodes followed by one
 * {@code END} node, reproducing legacy execution order exactly. Plugins whose name matches the
 * mandatory global/auth class (see {@code docs/plugin-graph-compatibility-audit.md}, issue #7994) are
 * marked {@code required} so the fail-closed semantics of the legacy chain are preserved.</p>
 *
 * @since 2.7.2
 */
public final class LegacyChainImporter {

    /**
     * the node id of the terminal END node produced by the importer.
     */
    public static final String END_NODE_ID = "end";

    private static final Set<String> MANDATORY_PLUGINS = Set.of(
            PluginEnum.GLOBAL.getName(),
            PluginEnum.SIGN.getName(),
            PluginEnum.JWT.getName(),
            PluginEnum.OAUTH2.getName(),
            PluginEnum.CASDOOR.getName(),
            PluginEnum.WAF.getName(),
            PluginEnum.KEY_AUTH.getName(),
            PluginEnum.BASIC_AUTH.getName());

    private LegacyChainImporter() {
    }

    /**
     * Import an ordered legacy plugin chain into a linear graph definition.
     *
     * <p>Disabled plugins and entries without a name are dropped; the remainder are ordered by
     * {@link PluginData#getSort()} ascending (nulls last), mirroring the legacy chain ordering.</p>
     *
     * @param plugins     the legacy plugins (need not be pre-sorted)
     * @param namespaceId the namespace to bind the graph to
     * @param graphId     the stable graph identity
     * @param revision    the revision number to publish
     * @return a linear {@link GraphData} equivalent to the legacy chain
     */
    public static GraphData importLinearChain(final List<PluginData> plugins, final String namespaceId,
                                              final String graphId, final long revision) {
        Objects.requireNonNull(plugins, "plugins");
        final List<PluginData> ordered = orderEnabled(plugins);
        final List<GraphNodeData> nodes = new ArrayList<>();
        for (int i = 0; i < ordered.size(); i++) {
            nodes.add(pluginNode(ordered.get(i), i, ordered.size()));
        }
        nodes.add(endNode());
        final GraphData graph = new GraphData();
        graph.setSchemaVersion(GraphValidator.SUPPORTED_SCHEMA_VERSION);
        graph.setNamespaceId(namespaceId);
        graph.setGraphId(graphId);
        graph.setRevision(revision);
        graph.setEntryNodeId(ordered.isEmpty() ? END_NODE_ID : nodeId(0));
        graph.setNodes(nodes);
        graph.setContentHash(contentHash(graph));
        return graph;
    }

    /**
     * Compute a deterministic content fingerprint of a graph's normalized node/edge content.
     *
     * <p>Phase 1 uses a stable hash of the canonicalized node sequence for change detection and
     * idempotent sync; a stronger digest may replace it without a schema change.</p>
     *
     * @param graph the graph whose content to fingerprint
     * @return the hex content hash
     */
    public static String contentHash(final GraphData graph) {
        final StringBuilder canonical = new StringBuilder();
        if (Objects.isNull(graph) || Objects.isNull(graph.getNodes())) {
            return Integer.toHexString(canonical.toString().hashCode());
        }
        for (GraphNodeData node : graph.getNodes()) {
            appendNode(canonical, node);
        }
        return Integer.toHexString(canonical.toString().hashCode());
    }

    private static void appendNode(final StringBuilder canonical, final GraphNodeData node) {
        canonical.append(node.getNodeId()).append('|')
                .append(node.getType()).append('|')
                .append(node.getPluginName()).append('|')
                .append(node.getNext()).append('|')
                .append(node.getDefaultTarget()).append('|')
                .append(node.isRequired()).append(';');
        if (Objects.isNull(node.getConditions())) {
            return;
        }
        for (GraphBranchData branch : node.getConditions()) {
            canonical.append(branch.getTarget()).append('=').append(branch.getMatchMode())
                    .append('=').append(branch.getConditions()).append(',');
        }
    }

    private static List<PluginData> orderEnabled(final List<PluginData> plugins) {
        return plugins.stream()
                .filter(Objects::nonNull)
                .filter(plugin -> !Boolean.FALSE.equals(plugin.getEnabled()))
                .filter(plugin -> !GraphTopology.isBlank(plugin.getName()))
                .sorted(Comparator.comparingInt(plugin -> Objects.isNull(plugin.getSort()) ? Integer.MAX_VALUE : plugin.getSort()))
                .collect(Collectors.toList());
    }

    private static GraphNodeData pluginNode(final PluginData plugin, final int index, final int total) {
        final GraphNodeData node = new GraphNodeData();
        node.setNodeId(nodeId(index));
        node.setType(GraphNodeTypeEnum.PLUGIN.getName());
        node.setPluginName(plugin.getName());
        node.setRequired(MANDATORY_PLUGINS.contains(plugin.getName()));
        node.setNext(index == total - 1 ? END_NODE_ID : nodeId(index + 1));
        return node;
    }

    private static GraphNodeData endNode() {
        final GraphNodeData node = new GraphNodeData();
        node.setNodeId(END_NODE_ID);
        node.setType(GraphNodeTypeEnum.END.getName());
        return node;
    }

    private static String nodeId(final int index) {
        return "n" + index;
    }
}
