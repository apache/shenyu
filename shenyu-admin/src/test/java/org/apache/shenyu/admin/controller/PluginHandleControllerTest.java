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

import org.apache.shenyu.admin.exception.ExceptionHandlers;
import org.apache.shenyu.admin.model.dto.PluginHandleDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.PluginHandleQuery;
import org.apache.shenyu.admin.service.PluginHandleService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.admin.model.vo.PluginHandleVO;
import org.apache.shenyu.common.utils.DateUtils;
import org.apache.shenyu.common.utils.GsonUtils;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import static org.hamcrest.core.Is.is;
import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Test case for PluginHandleController.
 */
@RunWith(SpringRunner.class)
public final class PluginHandleControllerTest {

    private MockMvc mockMvc;

    @InjectMocks
    private PluginHandleController pluginHandleController;

    @Mock
    private PluginHandleService pluginHandleService;

    private final PluginHandleVO pluginHandleVO = new PluginHandleVO("1", "2", "3", "label",
            1, 1, 1, null, DateUtils.localDateTimeToString(LocalDateTime.now()),
            DateUtils.localDateTimeToString(LocalDateTime.now()), new ArrayList<>());

    @Before
    public void setUp() {
        this.mockMvc = MockMvcBuilders.standaloneSetup(pluginHandleController)
                .setControllerAdvice(new ExceptionHandlers())
                .build();
    }

    @Test
    public void testQueryPluginHandles() throws Exception {
        given(this.pluginHandleService.listByPage(new PluginHandleQuery("2", null, null, new PageParameter(1, 1))))
                .willReturn(new CommonPager<>());
        this.mockMvc.perform(MockMvcRequestBuilders.get("/plugin-handle/", "1", 1, 1))
                .andExpect(status().isOk())
                .andReturn();
    }

    @Test
    public void testQueryAllPluginHandlesByPluginId() throws Exception {
        given(this.pluginHandleService.list("1", 1)).willReturn(Collections.singletonList(pluginHandleVO));
        this.mockMvc.perform(MockMvcRequestBuilders.get("/plugin-handle/all/{pluginId}/{type}", "1", 1))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.QUERY_SUCCESS)))
                .andExpect(jsonPath("$.data[0].id", is(pluginHandleVO.getId())))
                .andReturn();
    }

    @Test
    public void testDetailRule() throws Exception {
        given(this.pluginHandleService.findById("1")).willReturn(pluginHandleVO);
        this.mockMvc.perform(MockMvcRequestBuilders.get("/plugin-handle/{id}", "1"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.DETAIL_SUCCESS)))
                .andExpect(jsonPath("$.data.id", is(pluginHandleVO.getId())))
                .andReturn();
    }

    @Test
    public void testCreatePluginHandle() throws Exception {
        PluginHandleDTO pluginHandleDTO = new PluginHandleDTO();
        pluginHandleDTO.setPluginId("1213");
        pluginHandleDTO.setDataType(1);
        pluginHandleDTO.setField("f");
        given(this.pluginHandleService.createOrUpdate(pluginHandleDTO)).willReturn(1);
        this.mockMvc.perform(MockMvcRequestBuilders.post("/plugin-handle/")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(pluginHandleDTO)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.CREATE_SUCCESS)))
                .andReturn();
    }

    @Test
    public void testUpdatePluginHandle() throws Exception {
        PluginHandleDTO pluginHandleDTO = new PluginHandleDTO();
        pluginHandleDTO.setId("1");
        pluginHandleDTO.setPluginId("1213");
        pluginHandleDTO.setDataType(1);
        pluginHandleDTO.setField("f");
        given(this.pluginHandleService.createOrUpdate(pluginHandleDTO)).willReturn(1);
        this.mockMvc.perform(MockMvcRequestBuilders.put("/plugin-handle/{id}", "1")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(pluginHandleDTO)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.UPDATE_SUCCESS)))
                .andReturn();
    }

    @Test
    public void testDeletePluginHandles() throws Exception {
        given(this.pluginHandleService.deletePluginHandles(Collections.singletonList("1"))).willReturn(1);
        this.mockMvc.perform(MockMvcRequestBuilders.delete("/plugin-handle/batch", "1")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(Collections.singletonList("1"))))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.DELETE_SUCCESS)))
                .andReturn();
    }
}
