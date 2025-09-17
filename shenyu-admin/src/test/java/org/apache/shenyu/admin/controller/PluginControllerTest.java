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

import com.google.common.collect.Lists;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.exception.ExceptionHandlers;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.model.dto.BatchCommonDTO;
import org.apache.shenyu.admin.model.dto.PluginDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.PluginQuery;
import org.apache.shenyu.admin.model.vo.PluginVO;
import org.apache.shenyu.admin.service.PluginService;
import org.apache.shenyu.admin.service.SyncDataService;
import org.apache.shenyu.admin.spring.SpringBeanUtils;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.utils.DateUtils;
import org.apache.shenyu.common.utils.GsonUtils;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Base64;
import java.util.Collections;
import java.util.List;

import static org.hamcrest.core.Is.is;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Test cases for ShenyuDictController.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class PluginControllerTest {

    private MockMvc mockMvc;

    @InjectMocks
    private PluginController pluginController;

    @Mock
    private PluginService pluginService;

    @Mock
    private SyncDataService syncDataService;

    @Mock
    private PluginMapper pluginMapper;

    private PluginVO pluginVO;

    @BeforeEach
    public void setUp() {
        this.mockMvc = MockMvcBuilders.standaloneSetup(pluginController)
                .setControllerAdvice(new ExceptionHandlers(null))
                .build();
        this.pluginVO = new PluginVO("123", "1", "t_n", "1", 1, true,
                DateUtils.localDateTimeToString(LocalDateTime.now()), DateUtils.localDateTimeToString(LocalDateTime.now()), "", Lists.newArrayList());
        SpringBeanUtils.getInstance().setApplicationContext(mock(ConfigurableApplicationContext.class));

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
        this.mockMvc.perform(MockMvcRequestBuilders.get("/plugin-template")
                        .param("name", "t_n")
                        .param("enabled", "1")
                        .param("currentPage", String.valueOf(pageParameter.getCurrentPage()))
                        .param("pageSize", String.valueOf(pageParameter.getPageSize())))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.QUERY_SUCCESS)))
                .andExpect(jsonPath("$.data.dataList[0].name", is(pluginVO.getName())))
                .andReturn();
    }

    @Test
    public void testQueryAllPlugins() throws Exception {
        given(this.pluginService.listAll())
                .willReturn(new ArrayList<>());
        this.mockMvc.perform(MockMvcRequestBuilders.get("/plugin-template/all"))
                .andExpect(status().isOk())
                .andReturn();
    }

    @Test
    public void testDetailPlugin() throws Exception {
        given(this.pluginService.findById("123")).willReturn(pluginVO);
        this.mockMvc.perform(MockMvcRequestBuilders.get("/plugin-template/{id}", "123"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.DETAIL_SUCCESS)))
                .andExpect(jsonPath("$.data.id", is(pluginVO.getId())))
                .andReturn();
    }

    @Test
    public void testCreatePlugin() throws Exception {
        MockMultipartFile file = new MockMultipartFile("file", "test.jar", MediaType.TEXT_PLAIN_VALUE, "This is a test file.".getBytes());
        PluginDTO pluginDTO = new PluginDTO();
        pluginDTO.setName("test1");
        pluginDTO.setEnabled(true);
        pluginDTO.setRole("1");
        pluginDTO.setSort(100);
        pluginDTO.setFile(Base64.getEncoder().encodeToString(file.getBytes()));
        when(SpringBeanUtils.getInstance().getBean(PluginMapper.class)).thenReturn(pluginMapper);
        when(pluginMapper.existed(pluginDTO.getId())).thenReturn(false);
        given(this.pluginService.createOrUpdate(pluginDTO)).willReturn(ShenyuResultMessage.CREATE_SUCCESS);

        this.mockMvc.perform(MockMvcRequestBuilders.multipart("/plugin-template")
                        .param("name", pluginDTO.getName())
                        .param("enabled", String.valueOf(pluginDTO.getEnabled()))
                        .param("role", pluginDTO.getRole())
                        .param("sort", String.valueOf(pluginDTO.getSort()))
                        .param("file", pluginDTO.getFile()))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.CREATE_SUCCESS)))
                .andReturn();
        // update success
        pluginDTO.setId("123");
        when(pluginMapper.existed(pluginDTO.getId())).thenReturn(true);
        given(this.pluginService.createOrUpdate(pluginDTO)).willReturn(ShenyuResultMessage.UPDATE_SUCCESS);
        this.mockMvc.perform(MockMvcRequestBuilders.multipart("/plugin-template")
                        .param("file", pluginDTO.getFile())
                        .param("id", pluginDTO.getId())
                        .param("name", pluginDTO.getName())
                        .param("enabled", String.valueOf(pluginDTO.getEnabled()))
                        .param("role", pluginDTO.getRole())
                        .param("sort", String.valueOf(pluginDTO.getSort())))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.UPDATE_SUCCESS)))
                .andReturn();

        // update fail
        when(pluginMapper.existed(pluginDTO.getId())).thenReturn(false);
        this.mockMvc.perform(MockMvcRequestBuilders.multipart("/plugin-template")
                        .file(file)
                        .param("id", pluginDTO.getId())
                        .param("name", pluginDTO.getName())
                        .param("enabled", String.valueOf(pluginDTO.getEnabled()))
                        .param("role", pluginDTO.getRole())
                        .param("sort", String.valueOf(pluginDTO.getSort())))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", Matchers.containsString("Request error! invalid argument")))
                .andReturn();
    }

    @Test
    public void testUpdatePlugin() throws Exception {
        PluginDTO pluginDTO = new PluginDTO();
        pluginDTO.setId("123");
        pluginDTO.setName("test1");
        pluginDTO.setEnabled(true);
        pluginDTO.setRole("1");
        pluginDTO.setSort(100);
        when(SpringBeanUtils.getInstance().getBean(PluginMapper.class)).thenReturn(pluginMapper);
        when(pluginMapper.existed(pluginDTO.getId())).thenReturn(true);
        given(this.pluginService.createOrUpdate(pluginDTO)).willReturn(ShenyuResultMessage.UPDATE_SUCCESS);
        this.mockMvc.perform(MockMvcRequestBuilders.put("/plugin-template/{id}", pluginDTO.getId())
                        .contentType(MediaType.APPLICATION_FORM_URLENCODED_VALUE)
                        .param("name", pluginDTO.getName())
                        .param("enabled", String.valueOf(pluginDTO.getEnabled()))
                        .param("role", pluginDTO.getRole())
                        .param("sort", String.valueOf(pluginDTO.getSort())))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.UPDATE_SUCCESS)))
                .andReturn();
        when(pluginMapper.existed(pluginDTO.getId())).thenReturn(null);
        this.mockMvc.perform(MockMvcRequestBuilders.put("/plugin-template/{id}", pluginDTO.getId())
                        .contentType(MediaType.MULTIPART_FORM_DATA_VALUE)
                        .param("name", pluginDTO.getName())
                        .param("enabled", String.valueOf(pluginDTO.getEnabled()))
                        .param("role", pluginDTO.getRole())
                        .param("sort", String.valueOf(pluginDTO.getSort())))
                .andExpect(jsonPath("$.message", Matchers.containsString("Request error! invalid argument")))
                .andReturn();
        when(pluginMapper.existed(pluginDTO.getId())).thenReturn(true);
        given(this.pluginService.createOrUpdate(pluginDTO)).willReturn(ShenyuResultMessage.CREATE_SUCCESS);
        this.mockMvc.perform(MockMvcRequestBuilders.put("/plugin-template/{id}", pluginDTO.getId())
                        .contentType(MediaType.APPLICATION_FORM_URLENCODED_VALUE)
                        .param("name", pluginDTO.getName())
                        .param("enabled", String.valueOf(pluginDTO.getEnabled()))
                        .param("role", pluginDTO.getRole())
                        .param("sort", String.valueOf(pluginDTO.getSort())))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.CREATE_SUCCESS)))
                .andReturn();
    }

    @Test
    public void testDeletePlugins() throws Exception {
        given(this.pluginService.delete(Collections.singletonList("123"))).willReturn(StringUtils.EMPTY);
        this.mockMvc.perform(MockMvcRequestBuilders.delete("/plugin-template/batch")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("[\"123\"]"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.DELETE_SUCCESS)))
                .andReturn();

        given(this.pluginService.delete(Collections.singletonList("123"))).willReturn(AdminConstants.SYS_PLUGIN_ID_NOT_EXIST);
        this.mockMvc.perform(MockMvcRequestBuilders.delete("/plugin-template/batch")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("[\"123\"]"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(AdminConstants.SYS_PLUGIN_ID_NOT_EXIST)))
                .andReturn();

        given(this.pluginService.delete(Collections.singletonList("123"))).willReturn(AdminConstants.SYS_PLUGIN_NOT_DELETE);
        this.mockMvc.perform(MockMvcRequestBuilders.delete("/plugin-template/batch")
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
        this.mockMvc.perform(MockMvcRequestBuilders.post("/plugin-template/enabled")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(GsonUtils.getInstance().toJson(batchCommonDTO)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.ENABLE_SUCCESS)))
                .andReturn();

        given(this.pluginService.enabled(batchCommonDTO.getIds(), batchCommonDTO.getEnabled())).willReturn(AdminConstants.SYS_PLUGIN_ID_NOT_EXIST);
        this.mockMvc.perform(MockMvcRequestBuilders.post("/plugin-template/enabled")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(GsonUtils.getInstance().toJson(batchCommonDTO)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(AdminConstants.SYS_PLUGIN_ID_NOT_EXIST)))
                .andReturn();
    }
}
