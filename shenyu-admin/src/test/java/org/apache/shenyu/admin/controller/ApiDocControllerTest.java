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
import java.util.Collection;
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
    private DocManager docManager;

    @Mock
    private ShenyuDictService shenyuDictService;

    @BeforeEach
    public void setUp() {
        this.mockMvc = MockMvcBuilders.standaloneSetup(apiDocController)
                .build();
    }

    @Test
    public void testGetAllDoc() throws Exception {
        Collection<DocInfo> docInfos = buildDocInfos();
        List<ShenyuDictVO> shenyuDictVOS = new ArrayList<>();
        given(this.docManager.listAll()).willReturn(docInfos);
        given(this.shenyuDictService.list(any())).willReturn(shenyuDictVOS);
        this.mockMvc.perform(MockMvcRequestBuilders.get("/apidoc/getDocMenus"))
                .andExpect(status().isOk())
                .andReturn();
    }

    @Test
    public void testGetDocItem() throws Exception {
        DocItem docItem = new DocItem();
        docItem.setId("123");
        docItem.setApiOrder(1);
        docItem.setDescription("this is a test");
        given(this.docManager.getDocItem("123")).willReturn(docItem);
        this.mockMvc.perform(MockMvcRequestBuilders.get("/apidoc/getDocItem")
                        .param("id", "123"))
                .andExpect(status().isOk())
                .andReturn();
    }

    private Collection<DocInfo> buildDocInfos() {
        final List<DocInfo> infos = new ArrayList<>();
        DocInfo docInfo = new DocInfo();
        docInfo.setTitle("test");
        docInfo.setClusterName("cluster1");
        final List<DocModule> docModuleList = new ArrayList<>();
        DocModule docModule = new DocModule();
        docModule.setModule("test");
        docModule.setOrder(1);
        List<DocItem> docItems = new ArrayList<>();
        docModule.setDocItems(docItems);
        docModuleList.add(docModule);
        docInfo.setDocModuleList(docModuleList);
        infos.add(docInfo);
        return infos;
    }
}
