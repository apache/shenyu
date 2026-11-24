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

import org.apache.shenyu.admin.mapper.GraphPublicationMapper;
import org.apache.shenyu.admin.model.entity.GraphDraftDO;
import org.apache.shenyu.admin.model.entity.GraphRevisionDO;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.GraphBranchData;
import org.apache.shenyu.common.dto.GraphData;
import org.apache.shenyu.common.dto.GraphNodeData;
import org.apache.shenyu.common.utils.JsonUtils;
import org.junit.jupiter.api.Test;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ResponseStatusException;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class GraphPublicationServiceTest {

    private static final String NAMESPACE = "first";

    private static final String ID = "route";

    private final GraphPublicationMapper mapper = mock(GraphPublicationMapper.class);

    private final GraphPublicationService service = new GraphPublicationService(mapper);

    @Test
    void createRejectsCrossScopeAndHandlesConcurrentCreation() {
        GraphData input = graph();
        assertStatus(HttpStatus.BAD_REQUEST, () -> service.createDraft("other", ID, input));
        assertStatus(HttpStatus.BAD_REQUEST, () -> service.createDraft(NAMESPACE, ID + " ", input));
        input.setDeploymentBinding(" ");
        assertStatus(HttpStatus.BAD_REQUEST, () -> service.createDraft(NAMESPACE, ID, input));
        verify(mapper, never()).insertDraft(any());

        input.setDeploymentBinding(null);
        when(mapper.insertDraft(any())).thenReturn(0).thenThrow(new DuplicateKeyException("duplicate"));
        assertStatus(HttpStatus.NOT_FOUND, () -> service.createDraft(NAMESPACE, ID, input));
        assertStatus(HttpStatus.CONFLICT, () -> service.createDraft(NAMESPACE, ID, input));
    }

    @Test
    void losingCompareAndSetNeverAppendsARevision() {
        GraphDraftDO head = savedDraft(graph());
        head.setDraftVersion(7);
        head.setPublishedRevision(3);
        when(mapper.findDraft(NAMESPACE, ID)).thenReturn(head);

        assertStatus(HttpStatus.CONFLICT, () -> service.publish(NAMESPACE, ID, 6));
        verify(mapper, never()).updateDraft(any(), anyLong(), anyLong());

        assertStatus(HttpStatus.CONFLICT, () -> service.publish(NAMESPACE, ID, 7));
        verify(mapper).updateDraft(head, 7, 3);
        verify(mapper, never()).insertRevision(any());
    }

    @Test
    void editingADeletedHeadRecreatesTheDraftWithoutRepublishing() {
        GraphDraftDO head = new GraphDraftDO();
        head.setNamespaceId(NAMESPACE);
        head.setGraphId(ID);
        head.setPublishedRevision(1);
        head.setDraftVersion(2);
        when(mapper.findDraft(NAMESPACE, ID)).thenReturn(head);
        assertStatus(HttpStatus.CONFLICT, () -> service.updateDraft(NAMESPACE, ID, 1, graph()));

        when(mapper.updateDraft(head, 2, 1)).thenReturn(1);
        GraphDraftDO recreated = service.updateDraft(NAMESPACE, ID, 2, graph());
        assertEquals(3, recreated.getDraftVersion());
        assertEquals(1, recreated.getPublishedRevision());
        assertEquals(graph(), JsonUtils.jsonToObject(recreated.getDraftJson(), GraphData.class));
        verify(mapper, never()).insertRevision(any());
    }

    @Test
    void validationUsesNamespaceCatalogAndRejectsMalformedChoice() {
        GraphData graph = graph();
        GraphNodeData plugin = node("proxy", "PLUGIN");
        plugin.setPluginName("divide");
        plugin.setNext("end");
        graph.setEntryNodeId("proxy");
        graph.setNodes(List.of(plugin, graph.getNodes().get(0)));
        GraphDraftDO head = savedDraft(graph);
        when(mapper.findDraft(NAMESPACE, ID)).thenReturn(head);
        assertFalse(service.validateDraft(NAMESPACE, ID).isEmpty());
        when(mapper.listEnabledPluginNames(NAMESPACE)).thenReturn(List.of("divide"));
        assertTrue(service.validateDraft(NAMESPACE, ID).isEmpty());

        GraphNodeData choice = node("choose", "CHOICE");
        choice.setDefaultTarget("end");
        choice.setConditions(Arrays.asList(null, new GraphBranchData(null, null, "end"),
                new GraphBranchData(Collections.singletonList(null), 0, "end")));
        graph.setNodes(List.of(choice, node("end", "END")));
        graph.setEntryNodeId("choose");
        GraphDraftDO invalid = savedDraft(graph);
        when(mapper.findDraft(NAMESPACE, ID)).thenReturn(invalid);
        List<String> errors = service.validateDraft(NAMESPACE, ID);
        assertTrue(errors.stream().anyMatch(message -> message.contains("match mode")));
        assertTrue(errors.stream().anyMatch(message -> message.contains("conditions")));
        assertStatus(HttpStatus.BAD_REQUEST, () -> service.publish(NAMESPACE, ID, 1));
        verify(mapper, never()).insertRevision(any());
    }

    @Test
    void contentHashTracksBindingAndOrderedBranchesButIgnoresNodeOrderAndRevision() {
        ConditionData condition = new ConditionData();
        condition.setParamType("uri");
        condition.setOperator("=");
        condition.setParamValue("/foo");
        GraphBranchData first = new GraphBranchData(List.of(condition), 0, "end");
        GraphBranchData second = new GraphBranchData(List.of(condition), 0, "otherEnd");
        GraphNodeData choice = node("choose", "CHOICE");
        choice.setConditions(List.of(first, second));
        choice.setDefaultTarget("end");
        GraphData graph = graph();
        GraphNodeData end = graph.getNodes().get(0);
        GraphNodeData other = node("otherEnd", "END");
        graph.setNodes(List.of(choice, end, other));
        graph.setEntryNodeId("choose");
        graph.setRevision(777);
        graph.setContentHash("client-controlled");

        when(mapper.updateDraft(any(), anyLong(), anyLong())).thenReturn(1);
        when(mapper.insertRevision(any())).thenReturn(1);
        GraphData firstRevision = publish(graph, 0);
        assertNotEquals(graph.getContentHash(), firstRevision.getContentHash());
        assertEquals(1, firstRevision.getRevision());
        graph.setNodes(List.of(other, end, choice));
        GraphData reordered = publish(graph, 9);
        assertEquals(firstRevision.getContentHash(), reordered.getContentHash());
        assertEquals(10, reordered.getRevision());

        choice.setConditions(List.of(second, first));
        GraphData differentBranches = publish(graph, 10);
        assertNotEquals(reordered.getContentHash(), differentBranches.getContentHash());
        graph.setDeploymentBinding("canary");
        assertNotEquals(differentBranches.getContentHash(), publish(graph, 11).getContentHash());
    }

    @Test
    void rollbackRevalidatesAndKeepsIndependentDraftEdits() {
        GraphData edited = graph();
        edited.setDeploymentBinding("canary");
        GraphDraftDO head = savedDraft(edited);
        head.setPublishedRevision(5);
        when(mapper.findDraft(NAMESPACE, ID)).thenReturn(head);
        GraphRevisionDO source = new GraphRevisionDO();
        source.setRevision(2);
        source.setGraphJson(JsonUtils.toJson(graph()));
        when(mapper.findRevision(NAMESPACE, ID, 2)).thenReturn(source);
        when(mapper.updateDraft(head, 1, 5)).thenReturn(1);
        when(mapper.insertRevision(any())).thenReturn(1);

        final String savedEdit = head.getDraftJson();
        GraphRevisionDO restored = service.rollback(NAMESPACE, ID, 1, 2);
        verify(mapper).listEnabledPluginNames(NAMESPACE);
        assertEquals(6, restored.getRevision());
        assertEquals(2, head.getDraftVersion());
        assertEquals(savedEdit, head.getDraftJson());
        assertNull(JsonUtils.jsonToObject(restored.getGraphJson(), GraphData.class).getDeploymentBinding());
        assertEquals(2, source.getRevision());
    }

    private GraphData publish(final GraphData graph, final long previousRevision) {
        GraphDraftDO head = savedDraft(graph);
        GraphData draft = JsonUtils.jsonToObject(head.getDraftJson(), GraphData.class);
        assertEquals(0, draft.getRevision());
        assertNull(draft.getContentHash());
        head.setPublishedRevision(previousRevision);
        when(mapper.findDraft(NAMESPACE, ID)).thenReturn(head);
        return JsonUtils.jsonToObject(service.publish(NAMESPACE, ID, 1).getGraphJson(), GraphData.class);
    }

    private GraphDraftDO savedDraft(final GraphData graph) {
        when(mapper.insertDraft(any())).thenReturn(1);
        return service.createDraft(NAMESPACE, ID, graph);
    }

    private static GraphData graph() {
        GraphData graph = new GraphData();
        graph.setNamespaceId(NAMESPACE);
        graph.setGraphId(ID);
        graph.setSchemaVersion(1);
        graph.setEntryNodeId("end");
        graph.setNodes(List.of(node("end", "END")));
        return graph;
    }

    private static GraphNodeData node(final String id, final String type) {
        GraphNodeData node = new GraphNodeData();
        node.setNodeId(id);
        node.setType(type);
        return node;
    }

    private static void assertStatus(final HttpStatus expected, final Runnable call) {
        assertEquals(expected, assertThrows(ResponseStatusException.class, call::run).getStatusCode());
    }
}
