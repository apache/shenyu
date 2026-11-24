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

package org.apache.shenyu.admin.controller;

import org.apache.shenyu.admin.aspect.controller.RestControllerAspect;
import org.apache.shenyu.admin.exception.ExceptionHandlers;
import org.apache.shenyu.admin.model.custom.UserInfo;
import org.apache.shenyu.admin.model.entity.GraphDraftDO;
import org.apache.shenyu.admin.model.entity.GraphRevisionDO;
import org.apache.shenyu.admin.service.GraphPublicationService;
import org.apache.shenyu.admin.service.NamespaceUserService;
import org.apache.shenyu.common.dto.GraphData;
import org.apache.shiro.subject.Subject;
import org.apache.shiro.util.ThreadContext;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.aop.aspectj.annotation.AspectJProxyFactory;
import org.springframework.context.support.StaticMessageSource;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.server.ResponseStatusException;

import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

class GraphPublicationControllerTest {

    private static final String GRAPH = "/graph/team/g";

    private static final String VERSION = "expectedDraftVersion";

    private final Subject subject = mock(Subject.class);

    private final GraphPublicationService graphs = mock(GraphPublicationService.class);

    private final NamespaceUserService namespaceUsers = mock(NamespaceUserService.class);

    private MockMvc mvc;

    @BeforeEach
    void setUp() {
        ThreadContext.bind(subject);
        GraphPublicationController controller = new GraphPublicationController(graphs, namespaceUsers);
        AspectJProxyFactory factory = new AspectJProxyFactory(controller);
        factory.addAspect(new RestControllerAspect(List.of()));
        GraphPublicationController proxy = factory.getProxy();
        mvc = MockMvcBuilders.standaloneSetup(proxy).setControllerAdvice(new ExceptionHandlers(new StaticMessageSource())).build();
    }

    @AfterEach
    void cleanUp() {
        ThreadContext.unbindSubject();
    }

    @Test
    void rejectsAnonymousAndCrossNamespaceAccess() throws Exception {
        mvc.perform(get("/graph/team/snapshot")).andExpect(status().isUnauthorized());
        signIn("system:graph:edit");
        mvc.perform(get("/graph/other/snapshot")).andExpect(status().isForbidden());
        mvc.perform(get("/graph/other/g/revisions/1")).andExpect(status().isForbidden());
        mvc.perform(post("/graph/other/g/publish").param(VERSION, "1")).andExpect(status().isForbidden());
        verifyNoInteractions(graphs);
    }

    @Test
    void viewersCanReadAndValidateButCannotChangeGraphs() throws Exception {
        signIn("system:graph:view");
        mvc.perform(post(GRAPH + "/draft").contentType(MediaType.APPLICATION_JSON).content("{}")).andExpect(status().isForbidden());
        mvc.perform(put(GRAPH + "/draft").param(VERSION, "1").contentType(MediaType.APPLICATION_JSON).content("{}")).andExpect(status().isForbidden());
        mvc.perform(post(GRAPH + "/publish").param(VERSION, "1")).andExpect(status().isForbidden());
        mvc.perform(post(GRAPH + "/revisions/1/rollback").param(VERSION, "2")).andExpect(status().isForbidden());
        mvc.perform(delete(GRAPH).param(VERSION, "1")).andExpect(status().isForbidden());
        verifyNoInteractions(graphs);
        when(graphs.validateDraft("team", "g")).thenReturn(List.of("node endpoint is missing"));
        mvc.perform(post(GRAPH + "/validate")).andExpect(status().isOk()).andExpect(jsonPath("$.data[0]").value("node endpoint is missing"));
        GraphDraftDO draft = new GraphDraftDO();
        draft.setDraftJson("{\"graphId\":\"g\"}");
        when(graphs.getDraft("team", "g")).thenReturn(draft);
        mvc.perform(get(GRAPH + "/draft")).andExpect(status().isOk())
                .andExpect(jsonPath("$.data.graph.graphId").value("g")).andExpect(jsonPath("$.data.draftJson").doesNotExist());
        mvc.perform(get(GRAPH + "/revisions")).andExpect(status().isOk());
        GraphRevisionDO revision = new GraphRevisionDO();
        revision.setGraphJson("{\"graphId\":\"g\",\"revision\":3}");
        when(graphs.getRevision("team", "g", 3L)).thenReturn(revision);
        mvc.perform(get(GRAPH + "/revisions/3")).andExpect(status().isOk())
                .andExpect(jsonPath("$.data.graph.revision").value(3)).andExpect(jsonPath("$.data.graphJson").doesNotExist());
        mvc.perform(get("/graph/team/snapshot")).andExpect(status().isOk());
        verify(graphs).getDraft("team", "g");
        verify(graphs).history("team", "g");
        verify(graphs).getRevision("team", "g", 3L);
        verify(graphs).snapshot("team");
    }

    @Test
    void editorsCanSubmitDraftAndVersionedCommands() throws Exception {
        signIn("system:graph:edit");
        mvc.perform(post(GRAPH + "/draft").contentType(MediaType.APPLICATION_JSON).content("{}")).andExpect(status().isOk());
        mvc.perform(put(GRAPH + "/draft").param(VERSION, "1").contentType(MediaType.APPLICATION_JSON).content("{}")).andExpect(status().isOk());
        mvc.perform(post(GRAPH + "/publish").param(VERSION, "2")).andExpect(status().isOk());
        mvc.perform(post(GRAPH + "/revisions/1/rollback").param(VERSION, "7")).andExpect(status().isOk());
        mvc.perform(delete(GRAPH).param(VERSION, "8")).andExpect(status().isOk());
        verify(graphs).createDraft(eq("team"), eq("g"), any(GraphData.class));
        verify(graphs).updateDraft(eq("team"), eq("g"), eq(1L), any(GraphData.class));
        verify(graphs).publish("team", "g", 2L);
        verify(graphs).rollback("team", "g", 7L, 1L);
        verify(graphs).delete("team", "g", 8L);
    }

    @Test
    void preservesGraphFailureStatusWithAdminExceptionAdvice() throws Exception {
        signIn("system:graph:edit");
        mvc.perform(post(GRAPH + "/publish")).andExpect(status().isBadRequest());
        mvc.perform(delete(GRAPH).param(VERSION, "bad")).andExpect(status().isBadRequest());
        mvc.perform(post(GRAPH + "/draft").contentType(MediaType.APPLICATION_JSON).content("{")).andExpect(status().isBadRequest());
        when(graphs.publish("team", "g", 2L)).thenThrow(new ResponseStatusException(HttpStatus.CONFLICT, "Draft version conflict"));
        mvc.perform(post(GRAPH + "/publish").param(VERSION, "2"))
                .andExpect(status().isConflict()).andExpect(jsonPath("$.message").value("Draft version conflict"));
        when(graphs.getRevision("team", "g", 2L)).thenThrow(new ResponseStatusException(HttpStatus.NOT_FOUND, "Unknown revision"));
        mvc.perform(get(GRAPH + "/revisions/2")).andExpect(status().isNotFound()).andExpect(jsonPath("$.message").value("Unknown revision"));
        when(graphs.publish("team", "g", 3L)).thenThrow(new ResponseStatusException(HttpStatus.BAD_REQUEST, "Invalid graph"));
        mvc.perform(post(GRAPH + "/publish").param(VERSION, "3"))
                .andExpect(status().isBadRequest()).andExpect(jsonPath("$.message").value("Invalid graph"));
    }

    private void signIn(final String permission) {
        when(subject.isAuthenticated()).thenReturn(true);
        when(subject.getPrincipal()).thenReturn(UserInfo.builder().userName("operator").userId("alice").build());
        when(subject.isPermitted(permission)).thenReturn(true);
        when(namespaceUsers.listNamespaceIdByUserId("alice")).thenReturn(List.of("team"));
    }
}
