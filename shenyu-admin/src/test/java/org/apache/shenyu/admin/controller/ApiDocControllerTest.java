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

import java.util.ArrayList;
import java.util.List;
import org.apache.shenyu.admin.model.bean.DocInfo;
import org.apache.shenyu.admin.model.bean.DocItem;
import org.apache.shenyu.admin.model.bean.DocModule;
import org.apache.shenyu.admin.model.vo.ShenyuDictVO;
import org.apache.shenyu.admin.service.ShenyuDictService;
import org.apache.shenyu.admin.service.manager.DocManager;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Test cases for ApiDocControllerTest.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class ApiDocControllerTest {

    private MockMvc mockMvc;

    @InjectMocks
    private ApiDocController apiDocController;

    @Mock
    private ShenyuDictService shenyuDictService;

    @Mock
    private DocManager docManager;

    @BeforeEach
    public void setUp() {
        this.mockMvc = MockMvcBuilders.standaloneSetup(apiDocController)
                .build();
    }

    @Test
    public void testGetAllDoc() throws Exception {
        List<ShenyuDictVO> shenyuDictVOS = new ArrayList<>();
        given(this.shenyuDictService.list(any())).willReturn(shenyuDictVOS);
        this.mockMvc.perform(MockMvcRequestBuilders.get("/apidoc/getDocMenus"))
                .andExpect(status().isOk())
                .andReturn();
    }

    @Test
    public void testMenusIncludePersistedDocumentIds() throws Exception {
        DocItem docItem = new DocItem();
        docItem.setId("api-id");
        docItem.setName("/orders/find");
        docItem.setSummary("Find order");
        DocModule module = new DocModule();
        module.setModule("orders");
        module.setDocItems(List.of(docItem));
        DocInfo project = new DocInfo();
        project.setTitle("/orders");
        project.setDocModuleList(List.of(module));
        given(docManager.listAll()).willReturn(List.of(project));
        given(shenyuDictService.list(any())).willReturn(List.of());

        mockMvc.perform(MockMvcRequestBuilders.get("/apidoc/getDocMenus"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.code").value(200))
                .andExpect(jsonPath("$.data.menuProjects[0].label").value("/orders"))
                .andExpect(jsonPath("$.data.menuProjects[0].children[0].children[0].id").value("api-id"))
                .andExpect(jsonPath("$.data.menuProjects[0].children[0].children[0].name").value("/orders/find"));
    }

    @Test
    public void testGetDocItem() throws Exception {
        DocItem docItem = new DocItem();
        docItem.setId("api-id");
        docItem.setName("/orders/find");
        docItem.setDescription("Find an order by id");
        given(docManager.getDocItem("api-id")).willReturn(docItem);

        mockMvc.perform(MockMvcRequestBuilders.get("/apidoc/getDocItem").param("id", "api-id"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.code").value(200))
                .andExpect(jsonPath("$.data.id").value("api-id"))
                .andExpect(jsonPath("$.data.name").value("/orders/find"))
                .andExpect(jsonPath("$.data.description").value("Find an order by id"));
        verify(docManager).getDocItem("api-id");
    }

    @Test
    public void testGetDocItemRequiresId() throws Exception {
        mockMvc.perform(MockMvcRequestBuilders.get("/apidoc/getDocItem"))
                .andExpect(status().isBadRequest());
        verify(docManager, never()).getDocItem(any());
    }

    @Test
    public void testGetDocItemNotFound() throws Exception {
        mockMvc.perform(MockMvcRequestBuilders.get("/apidoc/getDocItem").param("id", "missing"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.code").value(200))
                .andExpect(jsonPath("$.data").doesNotExist());
    }
}
