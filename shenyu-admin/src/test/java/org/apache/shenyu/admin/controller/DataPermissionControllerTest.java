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

import org.apache.shenyu.admin.model.dto.DataPermissionDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.RuleQuery;
import org.apache.shenyu.admin.model.query.SelectorQuery;
import org.apache.shenyu.admin.model.vo.DataPermissionPageVO;
import org.apache.shenyu.admin.service.DataPermissionService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.utils.GsonUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import java.util.Collections;
import static org.hamcrest.core.Is.is;
import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * add test case for {@link DataPermissionController}.
 */
@ExtendWith(MockitoExtension.class)
public class DataPermissionControllerTest {

    private MockMvc mockMvc;

    private final DataPermissionPageVO dataPermissionPageVO = new DataPermissionPageVO("testDataId", "testDataName", true);

    @InjectMocks
    private DataPermissionController dataPermissionController;

    @Mock
    private DataPermissionService dataPermissionService;

    @BeforeEach
    public void setUp() {
        this.mockMvc = MockMvcBuilders.standaloneSetup(dataPermissionController).build();
    }

    @Test
    public void listPageSelectorDataPermissions() throws Exception {
        Integer currentPage = 1;
        Integer pageSize = 10;
        String userId = "testUserId";
        String pluginId = "testPluginId";
        String name = "testName";

        final PageParameter pageParameter = new PageParameter(currentPage, pageSize);
        final CommonPager<DataPermissionPageVO> commonPager = new CommonPager<>(pageParameter, Collections.singletonList(dataPermissionPageVO));
        given(this.dataPermissionService.listSelectorsByPage(
                new SelectorQuery(pluginId, name, pageParameter), userId)).willReturn(commonPager);

        this.mockMvc.perform(MockMvcRequestBuilders.get("/data-permission/selector")
                .param("currentPage", currentPage + "")
                .param("pageSize", pageSize + "")
                .param("userId", userId)
                .param("pluginId", pluginId)
                .param("name", name))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.QUERY_SUCCESS)))
                .andExpect(jsonPath("$.data.dataList[0].dataId", is(dataPermissionPageVO.getDataId())))
                .andReturn();
    }

    @Test
    public void listPageRuleDataPermissions() throws Exception {
        Integer currentPage = 1;
        Integer pageSize = 10;
        String userId = "testUserId";
        String selectorId = "testSelectorId";
        String name = "testName";

        final PageParameter pageParameter = new PageParameter(currentPage, pageSize);
        final CommonPager<DataPermissionPageVO> commonPager = new CommonPager<>(pageParameter, Collections.singletonList(dataPermissionPageVO));
        given(this.dataPermissionService.listRulesByPage(
                new RuleQuery(selectorId, name, pageParameter), userId)).willReturn(commonPager);

        this.mockMvc.perform(MockMvcRequestBuilders.get("/data-permission/rules")
                .param("currentPage", currentPage + "")
                .param("pageSize", pageSize + "")
                .param("userId", userId)
                .param("selectorId", selectorId)
                .param("name", name))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.QUERY_SUCCESS)))
                .andExpect(jsonPath("$.data.dataList[0].dataId", is(dataPermissionPageVO.getDataId())))
                .andReturn();
    }

    @Test
    public void saveSelector() throws Exception {
        DataPermissionDTO dataPermissionDTO = new DataPermissionDTO();
        dataPermissionDTO.setDataId("testDataId");
        dataPermissionDTO.setUserId("testUserId");
        given(this.dataPermissionService.createSelector(dataPermissionDTO)).willReturn(1);
        this.mockMvc.perform(MockMvcRequestBuilders.post("/data-permission/selector")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(dataPermissionDTO)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.SAVE_SUCCESS)))
                .andExpect(jsonPath("$.data", is(1)))
                .andReturn();
    }

    @Test
    public void deleteSelector() throws Exception {
        DataPermissionDTO dataPermissionDTO = new DataPermissionDTO();
        dataPermissionDTO.setDataId("testDataId");
        dataPermissionDTO.setUserId("testUserId");
        given(this.dataPermissionService.deleteSelector(dataPermissionDTO)).willReturn(1);
        this.mockMvc.perform(MockMvcRequestBuilders.delete("/data-permission/selector")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(dataPermissionDTO)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.DELETE_SUCCESS)))
                .andExpect(jsonPath("$.data", is(1)))
                .andReturn();
    }

    @Test
    public void saveRule() throws Exception {
        DataPermissionDTO dataPermissionDTO = new DataPermissionDTO();
        dataPermissionDTO.setDataId("testDataId");
        dataPermissionDTO.setUserId("testUserId");
        given(this.dataPermissionService.createRule(dataPermissionDTO)).willReturn(1);
        this.mockMvc.perform(MockMvcRequestBuilders.post("/data-permission/rule")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(dataPermissionDTO)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.SAVE_SUCCESS)))
                .andExpect(jsonPath("$.data", is(1)))
                .andReturn();
    }

    @Test
    public void deleteRule() throws Exception {
        DataPermissionDTO dataPermissionDTO = new DataPermissionDTO();
        dataPermissionDTO.setDataId("testDataId");
        dataPermissionDTO.setUserId("testUserId");
        given(this.dataPermissionService.deleteRule(dataPermissionDTO)).willReturn(1);
        this.mockMvc.perform(MockMvcRequestBuilders.delete("/data-permission/rule")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(dataPermissionDTO)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.DELETE_SUCCESS)))
                .andExpect(jsonPath("$.data", is(1)))
                .andReturn();
    }
}
