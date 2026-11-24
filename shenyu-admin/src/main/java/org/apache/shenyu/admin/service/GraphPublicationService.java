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

package org.apache.shenyu.admin.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.MapperFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.json.JsonMapper;
import org.apache.commons.codec.digest.DigestUtils;
import org.apache.shenyu.admin.mapper.GraphPublicationMapper;
import org.apache.shenyu.admin.model.entity.GraphDraftDO;
import org.apache.shenyu.admin.model.entity.GraphRevisionDO;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.GraphBranchData;
import org.apache.shenyu.common.dto.GraphData;
import org.apache.shenyu.common.dto.GraphNodeData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.MatchModeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.graph.GraphTopology;
import org.apache.shenyu.common.graph.GraphValidator;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.server.ResponseStatusException;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;

/**
 * Stores namespace-scoped drafts and immutable desired revisions. The draft version
 * is a write token shared by edits, publication, rollback and deletion.
 */
@Service
public class GraphPublicationService {

    private static final ObjectMapper JSON = JsonMapper.builder().enable(MapperFeature.SORT_PROPERTIES_ALPHABETICALLY).build();

    // These standard gateway starters supply plugin beans independently of Admin plugin rows.
    // Binding on each gateway resolves the configured HTTP client and the actual installed beans.
    private static final Set<String> GATEWAY_PLUGIN_NAMES = Set.of(PluginEnum.GLOBAL.getName(), PluginEnum.URI.getName(),
            PluginEnum.NETTY_HTTP_CLIENT.getName(), PluginEnum.WEB_CLIENT.getName(), PluginEnum.RPC_PARAM_TRANSFORM.getName(), PluginEnum.RESPONSE.getName());

    private final GraphPublicationMapper mapper;

    public GraphPublicationService(final GraphPublicationMapper mapper) {
        this.mapper = mapper;
    }

    /**
     * Create an unpublished draft at version one.
     *
     * @param namespaceId namespace identity
     * @param graphId graph identity
     * @param graph editable graph
     * @return saved draft
     */
    @Transactional
    public GraphDraftDO createDraft(final String namespaceId, final String graphId, final GraphData graph) {
        GraphDraftDO draft = new GraphDraftDO();
        draft.setNamespaceId(namespaceId);
        draft.setGraphId(graphId);
        draft.setDraftVersion(1);
        draft.setDraftJson(draftJson(namespaceId, graphId, graph));
        try {
            if (mapper.insertDraft(draft) == 0) {
                throw new ResponseStatusException(HttpStatus.NOT_FOUND, "Namespace not found");
            }
        } catch (DuplicateKeyException ex) {
            throw new ResponseStatusException(HttpStatus.CONFLICT, "Graph identity already exists", ex);
        }
        return draft;
    }

    /**
     * Create or replace the editable body at an existing head, retaining its current publication.
     *
     * @param namespaceId namespace identity
     * @param graphId graph identity
     * @param expectedDraftVersion write token
     * @param graph editable graph
     * @return updated draft
     */
    @Transactional
    public GraphDraftDO updateDraft(final String namespaceId, final String graphId, final long expectedDraftVersion, final GraphData graph) {
        GraphDraftDO draft = forWrite(namespaceId, graphId, expectedDraftVersion);
        draft.setDraftJson(draftJson(namespaceId, graphId, graph));
        save(draft, expectedDraftVersion, draft.getPublishedRevision());
        return draft;
    }

    /**
     * Read a graph head, including the write token of a deleted graph.
     *
     * @param namespaceId namespace identity
     * @param graphId graph identity
     * @return current head
     */
    public GraphDraftDO getDraft(final String namespaceId, final String graphId) {
        requireIdentity(namespaceId, 50);
        requireIdentity(graphId, 128);
        GraphDraftDO head = mapper.findDraft(namespaceId, graphId);
        if (Objects.isNull(head)) {
            throw new ResponseStatusException(HttpStatus.NOT_FOUND, "Graph not found");
        }
        return head;
    }

    /**
     * Check the current draft against the namespace plugin catalog.
     *
     * @param namespaceId namespace identity
     * @param graphId graph identity
     * @return validation errors
     */
    public List<String> validateDraft(final String namespaceId, final String graphId) {
        GraphDraftDO draft = getDraft(namespaceId, graphId);
        requireLive(draft);
        return validate(decode(draft.getDraftJson()));
    }

    /**
     * Publish the saved draft as a new revision in the same transaction as its desired-state pointer.
     *
     * @param namespaceId namespace identity
     * @param graphId graph identity
     * @param expectedDraftVersion write token
     * @return committed desired revision
     */
    @Transactional
    public GraphRevisionDO publish(final String namespaceId, final String graphId, final long expectedDraftVersion) {
        GraphDraftDO draft = forWrite(namespaceId, graphId, expectedDraftVersion);
        requireLive(draft);
        return append(draft, decode(draft.getDraftJson()));
    }

    /**
     * Republish historical content after revalidation. Live drafts retain their independent edits.
     *
     * @param namespaceId namespace identity
     * @param graphId graph identity
     * @param expectedDraftVersion write token
     * @param sourceRevision historical revision to restore
     * @return new desired revision
     */
    @Transactional
    public GraphRevisionDO rollback(final String namespaceId, final String graphId, final long expectedDraftVersion, final long sourceRevision) {
        GraphDraftDO draft = forWrite(namespaceId, graphId, expectedDraftVersion);
        GraphRevisionDO source = getRevision(namespaceId, graphId, sourceRevision);
        if (Objects.isNull(source.getGraphJson())) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Cannot restore a deletion revision");
        }
        GraphData graph = decode(source.getGraphJson());
        if (Objects.isNull(draft.getDraftJson())) {
            draft.setDraftJson(draftJson(namespaceId, graphId, graph));
        }
        return append(draft, graph);
    }

    /**
     * Append a deletion tombstone and remove the editable body.
     *
     * @param namespaceId namespace identity
     * @param graphId graph identity
     * @param expectedDraftVersion write token
     * @return new desired tombstone
     */
    @Transactional
    public GraphRevisionDO delete(final String namespaceId, final String graphId, final long expectedDraftVersion) {
        GraphDraftDO draft = forWrite(namespaceId, graphId, expectedDraftVersion);
        requireLive(draft);
        draft.setDraftJson(null);
        return append(draft, null);
    }

    /**
     * Read a published revision or tombstone.
     *
     * @param namespaceId namespace identity
     * @param graphId graph identity
     * @param revision revision number
     * @return immutable revision
     */
    public GraphRevisionDO getRevision(final String namespaceId, final String graphId, final long revision) {
        requireIdentity(namespaceId, 50);
        requireIdentity(graphId, 128);
        GraphRevisionDO result = mapper.findRevision(namespaceId, graphId, revision);
        if (Objects.isNull(result)) {
            throw new ResponseStatusException(HttpStatus.NOT_FOUND, "Graph revision not found");
        }
        return result;
    }

    /**
     * Read the ordered history of one graph.
     *
     * @param namespaceId namespace identity
     * @param graphId graph identity
     * @return published revisions
     */
    public List<GraphRevisionDO> history(final String namespaceId, final String graphId) {
        getDraft(namespaceId, graphId);
        return mapper.listRevisions(namespaceId, graphId);
    }

    /**
     * Read the latest desired revision of each published graph, including deletion tombstones.
     *
     * @param namespaceId namespace identity
     * @return desired revisions for reconciliation
     */
    public List<GraphRevisionDO> snapshot(final String namespaceId) {
        requireIdentity(namespaceId, 50);
        return mapper.listPublished(namespaceId);
    }

    private String draftJson(final String namespaceId, final String graphId, final GraphData graph) {
        requireIdentity(namespaceId, 50);
        requireIdentity(graphId, 128);
        if (Objects.isNull(graph) || !namespaceId.equals(graph.getNamespaceId()) || !graphId.equals(graph.getGraphId())) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Graph identity must match the namespace and graph in the request path");
        }
        if (Objects.nonNull(graph.getDeploymentBinding()) && graph.getDeploymentBinding().isBlank()) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Deployment binding must be an identity or null for namespace scope");
        }
        GraphData editable = JSON.convertValue(graph, GraphData.class);
        editable.setRevision(0);
        editable.setContentHash(null);
        return encode(editable);
    }

    private void requireIdentity(final String value, final int maxLength) {
        if (GraphTopology.isBlank(value) || value.length() > maxLength || !value.equals(value.strip())) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Invalid graph or namespace identity");
        }
    }

    private GraphDraftDO forWrite(final String namespaceId, final String graphId, final long expectedVersion) {
        GraphDraftDO draft = getDraft(namespaceId, graphId);
        if (draft.getDraftVersion() != expectedVersion) {
            throw conflict();
        }
        return draft;
    }

    private void requireLive(final GraphDraftDO draft) {
        if (Objects.isNull(draft.getDraftJson())) {
            throw new ResponseStatusException(HttpStatus.CONFLICT, "Graph has been deleted");
        }
    }

    private GraphRevisionDO append(final GraphDraftDO draft, final GraphData graph) {
        long previousRevision = draft.getPublishedRevision();
        GraphRevisionDO revision = new GraphRevisionDO();
        revision.setNamespaceId(draft.getNamespaceId());
        revision.setGraphId(draft.getGraphId());
        revision.setRevision(Math.incrementExact(previousRevision));
        if (Objects.nonNull(graph)) {
            List<String> errors = validate(graph);
            if (!errors.isEmpty()) {
                throw new ResponseStatusException(HttpStatus.BAD_REQUEST, String.join("; ", errors));
            }
            // Node order is immaterial. CHOICE branch order must be preserved.
            graph.getNodes().sort(Comparator.comparing(GraphNodeData::getNodeId));
            graph.setRevision(0);
            graph.setContentHash(null);
            graph.setContentHash(DigestUtils.sha256Hex(encode(graph)));
            graph.setRevision(revision.getRevision());
            revision.setGraphJson(encode(graph));
        }
        draft.setPublishedRevision(revision.getRevision());
        save(draft, draft.getDraftVersion(), previousRevision);
        if (mapper.insertRevision(revision) != 1) {
            throw new IllegalStateException("Could not insert graph revision");
        }
        return revision;
    }

    private void save(final GraphDraftDO draft, final long expectedVersion, final long previousRevision) {
        draft.setDraftVersion(Math.incrementExact(expectedVersion));
        if (mapper.updateDraft(draft, expectedVersion, previousRevision) != 1) {
            throw conflict();
        }
    }

    private ResponseStatusException conflict() {
        return new ResponseStatusException(HttpStatus.CONFLICT, "Graph changed; reload the draft version");
    }

    private List<String> validate(final GraphData graph) {
        Set<String> plugins = new HashSet<>(GATEWAY_PLUGIN_NAMES);
        for (PluginData configured : mapper.listConfiguredPlugins(graph.getNamespaceId())) {
            if (Boolean.TRUE.equals(configured.getEnabled())) {
                plugins.add(configured.getName());
            } else {
                plugins.remove(configured.getName());
            }
        }
        GraphValidator validator = new GraphValidator(plugins);
        List<String> errors = new ArrayList<>(validator.validate(graph));
        if (Objects.nonNull(graph.getNodes())) {
            graph.getNodes().stream().filter(GraphTopology::isChoice).forEach(node -> validateConditions(node, errors));
        }
        return errors;
    }

    private void validateConditions(final GraphNodeData node, final List<String> errors) {
        if (Objects.isNull(node.getConditions())) {
            return;
        }
        for (GraphBranchData branch : node.getConditions()) {
            if (Objects.isNull(branch)) {
                continue;
            }
            if (!MatchModeEnum.match(branch.getMatchMode(), MatchModeEnum.AND) && !MatchModeEnum.match(branch.getMatchMode(), MatchModeEnum.OR)) {
                errors.add("Invalid match mode at CHOICE node " + node.getNodeId());
            }
            if (Objects.isNull(branch.getConditions()) || branch.getConditions().stream().anyMatch(this::invalidCondition)) {
                errors.add("Invalid conditions at CHOICE node " + node.getNodeId());
            }
        }
    }

    private boolean invalidCondition(final ConditionData condition) {
        return Objects.isNull(condition) || GraphTopology.isBlank(condition.getParamType()) || GraphTopology.isBlank(condition.getOperator());
    }

    private String encode(final GraphData graph) {
        try {
            return JSON.writeValueAsString(graph);
        } catch (JsonProcessingException ex) {
            throw new IllegalStateException("Cannot serialize graph", ex);
        }
    }

    private GraphData decode(final String json) {
        try {
            return JSON.readValue(json, GraphData.class);
        } catch (JsonProcessingException ex) {
            throw new IllegalStateException("Cannot read stored graph", ex);
        }
    }
}
