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

package org.dromara.soul.admin.controller;

import org.dromara.soul.admin.dto.BatchCommonDTO;
import org.dromara.soul.admin.dto.PluginDTO;
import org.dromara.soul.admin.service.PluginService;
import org.dromara.soul.admin.service.SyncDataService;
import org.dromara.soul.admin.utils.SoulResultMessage;
import org.dromara.soul.admin.vo.PluginVO;
import org.dromara.soul.common.enums.DataEventTypeEnum;
import org.dromara.soul.common.utils.DateUtils;
import org.dromara.soul.common.utils.GsonUtils;
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
import java.util.Arrays;

import static org.hamcrest.core.Is.is;
import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Test cases for SoulDictController.
 *
 * @author guanys
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
        this.mockMvc = MockMvcBuilders.standaloneSetup(pluginController).build();
        this.pluginVO = new PluginVO("123", 1, 1, "t_n", "1", true,
                DateUtils.localDateTimeToString(LocalDateTime.now()), DateUtils.localDateTimeToString(LocalDateTime.now()));
    }

    @Test
    public void testDetailPlugin() throws Exception {
        given(this.pluginService.findById("123")).willReturn(pluginVO);
        this.mockMvc.perform(MockMvcRequestBuilders.get("/plugin/{id}", "123"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(SoulResultMessage.DETAIL_SUCCESS)))
                .andExpect(jsonPath("$.data.id", is(pluginVO.getId())))
                .andReturn();
    }

    @Test
    public void testCreatePlugin() throws Exception {
        PluginDTO pluginDTO = new PluginDTO();
        pluginDTO.setId("123");
        pluginDTO.setName("test");
        given(this.pluginService.createOrUpdate(pluginDTO)).willReturn("1");
        this.mockMvc.perform(MockMvcRequestBuilders.post("/plugin/")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(pluginDTO)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(SoulResultMessage.CREATE_SUCCESS)))
                .andReturn();
    }

    @Test
    public void testUpdatePlugin() throws Exception {
        PluginDTO pluginDTO = new PluginDTO();
        pluginDTO.setId("123");
        pluginDTO.setName("test1");
        given(this.pluginService.createOrUpdate(pluginDTO)).willReturn(SoulResultMessage.UPDATE_SUCCESS);
        this.mockMvc.perform(MockMvcRequestBuilders.put("/plugin/{id}", "123")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(pluginDTO)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(SoulResultMessage.UPDATE_SUCCESS)))
                .andReturn();
    }

    @Test
    public void testDeletePlugins() throws Exception {
        given(this.pluginService.delete(Arrays.asList("123"))).willReturn(SoulResultMessage.DELETE_SUCCESS);
        this.mockMvc.perform(MockMvcRequestBuilders.delete("/plugin/batch")
                .contentType(MediaType.APPLICATION_JSON)
                .content("[\"123\"]"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(SoulResultMessage.DELETE_SUCCESS)))
                .andReturn();
    }

    @Test
    public void testEnabled() throws Exception {
        BatchCommonDTO batchCommonDTO = new BatchCommonDTO();
        batchCommonDTO.setEnabled(false);
        batchCommonDTO.setIds(Arrays.asList("123"));
        given(this.pluginService.enabled(batchCommonDTO.getIds(), batchCommonDTO.getEnabled())).willReturn(SoulResultMessage.ENABLE_SUCCESS);
        this.mockMvc.perform(MockMvcRequestBuilders.post("/plugin/enabled")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(batchCommonDTO)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(SoulResultMessage.ENABLE_SUCCESS)))
                .andReturn();
    }

    @Test
    public void testSyncPluginAll() throws Exception {
        given(this.syncDataService.syncAll(DataEventTypeEnum.REFRESH)).willReturn(true);
        this.mockMvc.perform(MockMvcRequestBuilders.post("/plugin/syncPluginAll"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(SoulResultMessage.SYNC_SUCCESS)))
                .andReturn();
    }

    @Test
    public void testSyncPluginData() throws Exception {
        given(this.syncDataService.syncPluginData("123")).willReturn(true);
        this.mockMvc.perform(MockMvcRequestBuilders.put("/plugin/syncPluginData/{id}", "123"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(SoulResultMessage.SYNC_SUCCESS)))
                .andReturn();
    }

}
