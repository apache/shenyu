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

import org.apache.shenyu.admin.model.dto.BatchCommonDTO;
import org.apache.shenyu.admin.model.dto.SoulDictDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.SoulDictQuery;
import org.apache.shenyu.admin.service.SoulDictService;
import org.apache.shenyu.admin.utils.SoulResultMessage;
import org.apache.shenyu.admin.model.vo.SoulDictVO;
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
import java.util.Collections;

import static org.hamcrest.core.Is.is;
import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Test cases for SoulDictController.
 *
 * @author dengliming
 */
@RunWith(MockitoJUnitRunner.class)
public final class SoulDictControllerTest {

    private MockMvc mockMvc;

    @InjectMocks
    private SoulDictController soulDictController;

    @Mock
    private SoulDictService soulDictService;

    private final SoulDictVO soulDictVO = new SoulDictVO("123", "1", "t", "t_n", "1", "desc", 2, true,
            DateUtils.localDateTimeToString(LocalDateTime.now()), DateUtils.localDateTimeToString(LocalDateTime.now()));

    @Before
    public void setUp() {
        this.mockMvc = MockMvcBuilders.standaloneSetup(soulDictController).build();
    }

    @Test
    public void testQueryDicts() throws Exception {
        final PageParameter pageParameter = new PageParameter();
        final SoulDictQuery soulDictQuery = new SoulDictQuery("1", "t", "t_n", pageParameter);
        final CommonPager<SoulDictVO> commonPager = new CommonPager<>(pageParameter, Collections.singletonList(soulDictVO));
        given(this.soulDictService.listByPage(soulDictQuery)).willReturn(commonPager);
        this.mockMvc.perform(MockMvcRequestBuilders.get("/soul-dict")
                .param("type", "1")
                .param("dictCode", "t")
                .param("dictName", "t_n")
                .param("currentPage", Integer.toString(pageParameter.getCurrentPage()))
                .param("pageSize", Integer.toString(pageParameter.getPageSize())))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(SoulResultMessage.QUERY_SUCCESS)))
                .andExpect(jsonPath("$.data.dataList[0].id", is(commonPager.getDataList().get(0).getId())))
                .andReturn();
    }

    @Test
    public void testFindByType() throws Exception {
        given(this.soulDictService.list("1")).willReturn(Collections.singletonList(soulDictVO));
        this.mockMvc.perform(MockMvcRequestBuilders.get("/soul-dict/all/{type}", "1"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(SoulResultMessage.QUERY_SUCCESS)))
                .andExpect(jsonPath("$.data[0].id", is(soulDictVO.getId())))
                .andReturn();
    }

    @Test
    public void testDetail() throws Exception {
        given(this.soulDictService.findById("123")).willReturn(soulDictVO);
        this.mockMvc.perform(MockMvcRequestBuilders.get("/soul-dict/{id}", "123"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(SoulResultMessage.DETAIL_SUCCESS)))
                .andExpect(jsonPath("$.data.id", is(soulDictVO.getId())))
                .andReturn();
    }

    @Test
    public void testCreateSoulDict() throws Exception {
        SoulDictDTO soulDictDTO = new SoulDictDTO();
        soulDictDTO.setId("123");
        soulDictDTO.setDesc("test");
        given(this.soulDictService.createOrUpdate(soulDictDTO)).willReturn(1);
        this.mockMvc.perform(MockMvcRequestBuilders.post("/soul-dict/")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(soulDictDTO)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(SoulResultMessage.CREATE_SUCCESS)))
                .andReturn();
    }

    @Test
    public void testUpdateSoulDict() throws Exception {
        SoulDictDTO soulDictDTO = new SoulDictDTO();
        soulDictDTO.setId("123");
        soulDictDTO.setDesc("test");
        given(this.soulDictService.createOrUpdate(soulDictDTO)).willReturn(1);
        this.mockMvc.perform(MockMvcRequestBuilders.put("/soul-dict/{id}", "123")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(soulDictDTO)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(SoulResultMessage.UPDATE_SUCCESS)))
                .andReturn();
    }

    @Test
    public void testDeleteSoulDicts() throws Exception {
        given(this.soulDictService.deleteSoulDicts(Collections.singletonList("123"))).willReturn(1);
        this.mockMvc.perform(MockMvcRequestBuilders.delete("/soul-dict/batch")
                .contentType(MediaType.APPLICATION_JSON)
                .content("[\"123\"]"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(SoulResultMessage.DELETE_SUCCESS)))
                .andReturn();
    }

    @Test
    public void testBatchEnabled() throws Exception {
        BatchCommonDTO batchCommonDTO = new BatchCommonDTO();
        batchCommonDTO.setEnabled(false);
        batchCommonDTO.setIds(Collections.singletonList("123"));
        given(this.soulDictService.enabled(batchCommonDTO.getIds(), batchCommonDTO.getEnabled())).willReturn(1);
        this.mockMvc.perform(MockMvcRequestBuilders.post("/soul-dict/batchEnabled")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(batchCommonDTO)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is("batch enable success")))
                .andReturn();
    }
}
