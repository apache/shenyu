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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.exception.ExceptionHandlers;
import org.apache.shenyu.admin.model.dto.BatchCommonDTO;
import org.apache.shenyu.admin.model.dto.PluginDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.PluginQuery;
import org.apache.shenyu.admin.service.PluginService;
import org.apache.shenyu.admin.service.SyncDataService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.admin.model.vo.PluginVO;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.utils.DateUtils;
import org.apache.shenyu.common.utils.GsonUtils;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.hamcrest.core.Is.is;
import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Test cases for ShenyuDictController.
 */
@RunWith(MockitoJUnitRunner.class)
public final class PluginControllerTest {

    private MockMvc mockMvc;

    @InjectMocks
    private PluginController pluginController;

    @Mock
    private PluginService pluginService;

    @Mock
    private SyncDataService syncDataService;

    private PluginVO pluginVO;

    @Before
    public void setUp() {
        this.mockMvc = MockMvcBuilders.standaloneSetup(pluginController)
                .setControllerAdvice(new ExceptionHandlers())
                .build();
        this.pluginVO = new PluginVO("123", 1, 1, "t_n", "1", true,
                DateUtils.localDateTimeToString(LocalDateTime.now()), DateUtils.localDateTimeToString(LocalDateTime.now()));
    }

    @Test
    public void testQueryPlugins() throws Exception {
        final PageParameter pageParameter = new PageParameter();
        List<PluginVO> pluginVOS = new ArrayList<>();
        pluginVOS.add(pluginVO);
        final CommonPager<PluginVO> commonPager = new CommonPager<>();
        commonPager.setPage(pageParameter);
        commonPager.setDataList(pluginVOS);
        final PluginQuery pluginQuery = new PluginQuery("t_n", 1, pageParameter);
        given(this.pluginService.listByPage(pluginQuery)).willReturn(commonPager);
        this.mockMvc.perform(MockMvcRequestBuilders.get("/plugin")
                .param("name", "t_n")
                .param("enabled", "1")
                .param("currentPage", pageParameter.getCurrentPage() + "")
                .param("pageSize", pageParameter.getPageSize() + ""))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.QUERY_SUCCESS)))
                .andExpect(jsonPath("$.data.dataList[0].name", is(pluginVO.getName())))
                .andReturn();
    }

    @Test
    public void testQueryAllPlugins() throws Exception {
        given(this.pluginService.listAll())
                .willReturn(new ArrayList<>());
        this.mockMvc.perform(MockMvcRequestBuilders.get("/plugin/all"))
                .andExpect(status().isOk())
                .andReturn();
    }

    @Test
    public void testDetailPlugin() throws Exception {
        given(this.pluginService.findById("123")).willReturn(pluginVO);
        this.mockMvc.perform(MockMvcRequestBuilders.get("/plugin/{id}", "123"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.DETAIL_SUCCESS)))
                .andExpect(jsonPath("$.data.id", is(pluginVO.getId())))
                .andReturn();
    }

    @Test
    public void testCreatePlugin() throws Exception {
        PluginDTO pluginDTO = new PluginDTO();
        pluginDTO.setId("123");
        pluginDTO.setName("test");
        pluginDTO.setEnabled(true);
        pluginDTO.setRole(1);
        given(this.pluginService.createOrUpdate(pluginDTO)).willReturn(StringUtils.EMPTY);
        this.mockMvc.perform(MockMvcRequestBuilders.post("/plugin/")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(pluginDTO)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.CREATE_SUCCESS)))
                .andReturn();

        given(this.pluginService.createOrUpdate(pluginDTO)).willReturn(AdminConstants.PLUGIN_NAME_IS_EXIST);
        this.mockMvc.perform(MockMvcRequestBuilders.post("/plugin/")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(pluginDTO)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(AdminConstants.PLUGIN_NAME_IS_EXIST)))
                .andReturn();

        given(this.pluginService.createOrUpdate(pluginDTO)).willReturn(AdminConstants.PLUGIN_NAME_NOT_EXIST);
        this.mockMvc.perform(MockMvcRequestBuilders.post("/plugin/")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(pluginDTO)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(AdminConstants.PLUGIN_NAME_NOT_EXIST)))
                .andReturn();

    }

    @Test
    public void testUpdatePlugin() throws Exception {
        PluginDTO pluginDTO = new PluginDTO();
        pluginDTO.setId("123");
        pluginDTO.setName("test1");
        pluginDTO.setEnabled(true);
        pluginDTO.setRole(1);
        given(this.pluginService.createOrUpdate(pluginDTO)).willReturn(StringUtils.EMPTY);
        this.mockMvc.perform(MockMvcRequestBuilders.put("/plugin/{id}", "123")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(pluginDTO)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.UPDATE_SUCCESS)))
                .andReturn();

        given(this.pluginService.createOrUpdate(pluginDTO)).willReturn(AdminConstants.PLUGIN_NAME_IS_EXIST);
        this.mockMvc.perform(MockMvcRequestBuilders.put("/plugin/{id}", "123")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(pluginDTO)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(AdminConstants.PLUGIN_NAME_IS_EXIST)))
                .andReturn();

        given(this.pluginService.createOrUpdate(pluginDTO)).willReturn(AdminConstants.PLUGIN_NAME_NOT_EXIST);
        this.mockMvc.perform(MockMvcRequestBuilders.put("/plugin/{id}", "123")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(pluginDTO)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(AdminConstants.PLUGIN_NAME_NOT_EXIST)))
                .andReturn();
    }

    @Test
    public void testDeletePlugins() throws Exception {
        given(this.pluginService.delete(Collections.singletonList("123"))).willReturn(StringUtils.EMPTY);
        this.mockMvc.perform(MockMvcRequestBuilders.delete("/plugin/batch")
                .contentType(MediaType.APPLICATION_JSON)
                .content("[\"123\"]"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.DELETE_SUCCESS)))
                .andReturn();

        given(this.pluginService.delete(Collections.singletonList("123"))).willReturn(AdminConstants.SYS_PLUGIN_ID_NOT_EXIST);
        this.mockMvc.perform(MockMvcRequestBuilders.delete("/plugin/batch")
                .contentType(MediaType.APPLICATION_JSON)
                .content("[\"123\"]"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(AdminConstants.SYS_PLUGIN_ID_NOT_EXIST)))
                .andReturn();

        given(this.pluginService.delete(Collections.singletonList("123"))).willReturn(AdminConstants.SYS_PLUGIN_NOT_DELETE);
        this.mockMvc.perform(MockMvcRequestBuilders.delete("/plugin/batch")
                .contentType(MediaType.APPLICATION_JSON)
                .content("[\"123\"]"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(AdminConstants.SYS_PLUGIN_NOT_DELETE)))
                .andReturn();
    }

    @Test
    public void testEnabled() throws Exception {
        BatchCommonDTO batchCommonDTO = new BatchCommonDTO();
        batchCommonDTO.setEnabled(false);
        batchCommonDTO.setIds(Collections.singletonList("123"));
        given(this.pluginService.enabled(batchCommonDTO.getIds(), batchCommonDTO.getEnabled())).willReturn(StringUtils.EMPTY);
        this.mockMvc.perform(MockMvcRequestBuilders.post("/plugin/enabled")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(batchCommonDTO)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.ENABLE_SUCCESS)))
                .andReturn();

        given(this.pluginService.enabled(batchCommonDTO.getIds(), batchCommonDTO.getEnabled())).willReturn(AdminConstants.SYS_PLUGIN_ID_NOT_EXIST);
        this.mockMvc.perform(MockMvcRequestBuilders.post("/plugin/enabled")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(batchCommonDTO)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(AdminConstants.SYS_PLUGIN_ID_NOT_EXIST)))
                .andReturn();
    }

    @Test
    public void testSyncPluginAll() throws Exception {
        given(this.syncDataService.syncAll(DataEventTypeEnum.REFRESH)).willReturn(true);
        this.mockMvc.perform(MockMvcRequestBuilders.post("/plugin/syncPluginAll"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.SYNC_SUCCESS)))
                .andReturn();

        given(this.syncDataService.syncAll(DataEventTypeEnum.REFRESH)).willReturn(false);
        this.mockMvc.perform(MockMvcRequestBuilders.post("/plugin/syncPluginAll"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.SYNC_FAIL)))
                .andReturn();
    }

    @Test
    public void testSyncPluginData() throws Exception {
        given(this.syncDataService.syncPluginData("123")).willReturn(true);
        this.mockMvc.perform(MockMvcRequestBuilders.put("/plugin/syncPluginData/{id}", "123"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.SYNC_SUCCESS)))
                .andReturn();

        given(this.syncDataService.syncPluginData("123")).willReturn(false);
        this.mockMvc.perform(MockMvcRequestBuilders.put("/plugin/syncPluginData/{id}", "123"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.SYNC_FAIL)))
                .andReturn();
    }

}
