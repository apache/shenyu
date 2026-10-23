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

import java.util.List;
import java.util.Objects;

/**
 * A versioned plugin orchestration graph definition.
 *
 * <p>This is the editable, syncable source-of-truth for a graph; a published {@code revision} is
 * immutable and is compiled into a request-path-free plan by the graph compiler (issue #7995). The
 * field semantics are fixed by {@code docs/plugin-graph-orchestration.md} (issue #7994) section 4.</p>
 *
 * @since 2.7.2
 */
public class GraphData {

    /**
     * contract version of this schema; validators reject unknown majors.
     */
    private int schemaVersion;

    /**
     * the namespace this graph is bound to.
     */
    private String namespaceId;

    /**
     * optional explicit deployment/group id; else namespace-wide.
     */
    private String deploymentBinding;

    /**
     * stable identity across revisions.
     */
    private String graphId;

    /**
     * monotonic per {@code graphId}; activation ordering key.
     */
    private long revision;

    /**
     * hash of the normalized node/edge content; change detection and idempotent sync.
     */
    private String contentHash;

    /**
     * the single entry node id; must reference a node in {@code nodes}.
     */
    private String entryNodeId;

    /**
     * the graph vertices; non-empty with unique node ids.
     */
    private List<GraphNodeData> nodes;

    /**
     * no args constructor.
     */
    public GraphData() {
    }

    /**
     * get schemaVersion.
     *
     * @return schemaVersion
     */
    public int getSchemaVersion() {
        return schemaVersion;
    }

    /**
     * set schemaVersion.
     *
     * @param schemaVersion schemaVersion
     */
    public void setSchemaVersion(final int schemaVersion) {
        this.schemaVersion = schemaVersion;
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
     * set namespaceId.
     *
     * @param namespaceId namespaceId
     */
    public void setNamespaceId(final String namespaceId) {
        this.namespaceId = namespaceId;
    }

    /**
     * get deploymentBinding.
     *
     * @return deploymentBinding
     */
    public String getDeploymentBinding() {
        return deploymentBinding;
    }

    /**
     * set deploymentBinding.
     *
     * @param deploymentBinding deploymentBinding
     */
    public void setDeploymentBinding(final String deploymentBinding) {
        this.deploymentBinding = deploymentBinding;
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
     * set graphId.
     *
     * @param graphId graphId
     */
    public void setGraphId(final String graphId) {
        this.graphId = graphId;
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
     * set revision.
     *
     * @param revision revision
     */
    public void setRevision(final long revision) {
        this.revision = revision;
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
     * set contentHash.
     *
     * @param contentHash contentHash
     */
    public void setContentHash(final String contentHash) {
        this.contentHash = contentHash;
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
     * set entryNodeId.
     *
     * @param entryNodeId entryNodeId
     */
    public void setEntryNodeId(final String entryNodeId) {
        this.entryNodeId = entryNodeId;
    }

    /**
     * get nodes.
     *
     * @return nodes
     */
    public List<GraphNodeData> getNodes() {
        return nodes;
    }

    /**
     * set nodes.
     *
     * @param nodes nodes
     */
    public void setNodes(final List<GraphNodeData> nodes) {
        this.nodes = nodes;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        GraphData that = (GraphData) o;
        return schemaVersion == that.schemaVersion && revision == that.revision
                && Objects.equals(namespaceId, that.namespaceId) && Objects.equals(deploymentBinding, that.deploymentBinding)
                && Objects.equals(graphId, that.graphId) && Objects.equals(contentHash, that.contentHash)
                && Objects.equals(entryNodeId, that.entryNodeId) && Objects.equals(nodes, that.nodes);
    }

    @Override
    public int hashCode() {
        return Objects.hash(schemaVersion, namespaceId, deploymentBinding, graphId, revision, contentHash, entryNodeId, nodes);
    }

    @Override
    public String toString() {
        return "GraphData{"
                + "schemaVersion="
                + schemaVersion
                + ", namespaceId='"
                + namespaceId
                + '\''
                + ", deploymentBinding='"
                + deploymentBinding
                + '\''
                + ", graphId='"
                + graphId
                + '\''
                + ", revision="
                + revision
                + ", contentHash='"
                + contentHash
                + '\''
                + ", entryNodeId='"
                + entryNodeId
                + '\''
                + ", nodes="
                + nodes
                + '}';
    }
}
