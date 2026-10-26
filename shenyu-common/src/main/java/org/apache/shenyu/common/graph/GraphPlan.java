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

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * The compiled, immutable, request-path-free form of a validated {@link GraphData} revision.
 *
 * <p>A plan freezes the graph topology: every node is indexed by id and every node's successors are
 * pre-resolved (a {@code PLUGIN} node has its single {@code next}; a {@code CHOICE} node has its
 * ordered branch targets followed by its {@code defaultTarget}; an {@code END} node has none). The
 * GRAPH executor (issue #7996) traverses a plan snapshot without mutating it, so all collections
 * exposed here are unmodifiable. See {@code docs/plugin-graph-orchestration.md} (issue #7994)
 * sections 3 and 10.</p>
 *
 * @since 2.7.2
 */
public final class GraphPlan {

    private final String graphId;

    private final long revision;

    private final String contentHash;

    private final String namespaceId;

    private final String entryNodeId;

    private final Map<String, GraphNodeData> nodesById;

    private final Map<String, List<String>> successors;

    /**
     * Instantiates a new GraphPlan.
     *
     * @param graph      the validated graph definition this plan was compiled from
     * @param nodesById  node id to node index (copied, exposed unmodifiable)
     * @param successors node id to resolved successor node ids (copied, exposed unmodifiable)
     */
    public GraphPlan(final GraphData graph, final Map<String, GraphNodeData> nodesById,
                     final Map<String, List<String>> successors) {
        Objects.requireNonNull(graph, "graph");
        Objects.requireNonNull(nodesById, "nodesById");
        Objects.requireNonNull(successors, "successors");
        this.graphId = graph.getGraphId();
        this.revision = graph.getRevision();
        this.contentHash = graph.getContentHash();
        this.namespaceId = graph.getNamespaceId();
        this.entryNodeId = graph.getEntryNodeId();
        this.nodesById = Collections.unmodifiableMap(new LinkedHashMap<>(nodesById));
        this.successors = freezeSuccessors(successors);
    }

    private static Map<String, List<String>> freezeSuccessors(final Map<String, List<String>> raw) {
        Map<String, List<String>> frozen = new LinkedHashMap<>(raw.size());
        for (Map.Entry<String, List<String>> entry : raw.entrySet()) {
            frozen.put(entry.getKey(), Collections.unmodifiableList(new ArrayList<>(entry.getValue())));
        }
        return Collections.unmodifiableMap(frozen);
    }

    /**
     * get graphId.
     *
     * @return graphId
     */
    public String getGraphId() {
        return graphId;
    }

    /**
     * get revision.
     *
     * @return revision
     */
    public long getRevision() {
        return revision;
    }

    /**
     * get contentHash.
     *
     * @return contentHash
     */
    public String getContentHash() {
        return contentHash;
    }

    /**
     * get namespaceId.
     *
     * @return namespaceId
     */
    public String getNamespaceId() {
        return namespaceId;
    }

    /**
     * get entryNodeId.
     *
     * @return entryNodeId
     */
    public String getEntryNodeId() {
        return entryNodeId;
    }

    /**
     * The node bound to the given id.
     *
     * @param nodeId the node id
     * @return the node, or {@code null} when the id is not part of this plan
     */
    public GraphNodeData getNode(final String nodeId) {
        return nodesById.get(nodeId);
    }

    /**
     * The resolved successor node ids of the given node, in evaluation order.
     *
     * @param nodeId the node id
     * @return an unmodifiable successor list; empty for an unknown id or an {@code END} node
     */
    public List<String> getSuccessors(final String nodeId) {
        return successors.getOrDefault(nodeId, Collections.emptyList());
    }

    /**
     * The unmodifiable set of all node ids in this plan.
     *
     * @return the node ids
     */
    public Set<String> nodeIds() {
        return nodesById.keySet();
    }

    /**
     * The number of nodes in this plan.
     *
     * @return the node count
     */
    public int size() {
        return nodesById.size();
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        GraphPlan that = (GraphPlan) o;
        return revision == that.revision && Objects.equals(graphId, that.graphId)
                && Objects.equals(contentHash, that.contentHash) && Objects.equals(namespaceId, that.namespaceId)
                && Objects.equals(entryNodeId, that.entryNodeId) && Objects.equals(nodesById, that.nodesById)
                && Objects.equals(successors, that.successors);
    }

    @Override
    public int hashCode() {
        return Objects.hash(graphId, revision, contentHash, namespaceId, entryNodeId, nodesById, successors);
    }

    @Override
    public String toString() {
        return "GraphPlan{"
                + "graphId='"
                + graphId
                + '\''
                + ", revision="
                + revision
                + ", contentHash='"
                + contentHash
                + '\''
                + ", namespaceId='"
                + namespaceId
                + '\''
                + ", entryNodeId='"
                + entryNodeId
                + '\''
                + ", nodes="
                + nodesById.size()
                + '}';
    }
}
