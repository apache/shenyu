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
import org.apache.shenyu.admin.mapper.ShenyuDictMapper;
import org.apache.shenyu.admin.model.dto.BatchCommonDTO;
import org.apache.shenyu.admin.model.dto.ShenyuDictDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.ShenyuDictQuery;
import org.apache.shenyu.admin.model.vo.ShenyuDictVO;
import org.apache.shenyu.admin.service.ShenyuDictService;
import org.apache.shenyu.admin.spring.SpringBeanUtils;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.utils.DateUtils;
import org.apache.shenyu.common.utils.GsonUtils;
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
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.time.LocalDateTime;
import java.util.Collections;
import java.util.UUID;

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
public final class ShenyuDictControllerTest {

    private MockMvc mockMvc;

    @InjectMocks
    private ShenyuDictController shenyuDictController;

    @Mock
    private ShenyuDictService shenyuDictService;
    
    @Mock
    private ShenyuDictMapper shenyuDictMapper;

    private final ShenyuDictVO shenyuDictVO = new ShenyuDictVO("123", "1", "t", "t_n", "1", "desc", 2, true,
            DateUtils.localDateTimeToString(LocalDateTime.now()), DateUtils.localDateTimeToString(LocalDateTime.now()));

    @BeforeEach
    public void setUp() {
        this.mockMvc = MockMvcBuilders.standaloneSetup(shenyuDictController)
                .setControllerAdvice(new ExceptionHandlers())
                .build();
    }

    @Test
    public void testQueryDicts() throws Exception {
        final PageParameter pageParameter = new PageParameter();
        final ShenyuDictQuery shenyuDictQuery = new ShenyuDictQuery("1", "t", "t_n", pageParameter);
        final CommonPager<ShenyuDictVO> commonPager = new CommonPager<>(pageParameter, Collections.singletonList(shenyuDictVO));
        given(this.shenyuDictService.listByPage(shenyuDictQuery)).willReturn(commonPager);
        this.mockMvc.perform(MockMvcRequestBuilders.get("/shenyu-dict")
                .param("type", "1")
                .param("dictCode", "t")
                .param("dictName", "t_n")
                .param("currentPage", Integer.toString(pageParameter.getCurrentPage()))
                .param("pageSize", Integer.toString(pageParameter.getPageSize())))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.QUERY_SUCCESS)))
                .andExpect(jsonPath("$.data.dataList[0].id", is(commonPager.getDataList().get(0).getId())))
                .andReturn();
    }

    @Test
    public void testFindByType() throws Exception {
        given(this.shenyuDictService.list("1")).willReturn(Collections.singletonList(shenyuDictVO));
        this.mockMvc.perform(MockMvcRequestBuilders.get("/shenyu-dict/all/{type}", "1"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.QUERY_SUCCESS)))
                .andExpect(jsonPath("$.data[0].id", is(shenyuDictVO.getId())))
                .andReturn();
    }

    @Test
    public void testDetail() throws Exception {
        given(this.shenyuDictService.findById("123")).willReturn(shenyuDictVO);
        this.mockMvc.perform(MockMvcRequestBuilders.get("/shenyu-dict/{id}", "123"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.DETAIL_SUCCESS)))
                .andExpect(jsonPath("$.data.id", is(shenyuDictVO.getId())))
                .andReturn();
    }

    @Test
    public void testCreateShenyuDict() throws Exception {
        ShenyuDictDTO shenyuDictDTO = buildTestDict();
        
        SpringBeanUtils.getInstance().setApplicationContext(mock(ConfigurableApplicationContext.class));
        when(SpringBeanUtils.getInstance().getBean(ShenyuDictMapper.class)).thenReturn(shenyuDictMapper);
        when(shenyuDictMapper.existed(shenyuDictDTO.getId())).thenReturn(true);
        
        given(this.shenyuDictService.createOrUpdate(shenyuDictDTO)).willReturn(1);
        this.mockMvc.perform(MockMvcRequestBuilders.post("/shenyu-dict/")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(shenyuDictDTO)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.CREATE_SUCCESS)))
                .andReturn();
    }

    @Test
    public void testUpdateShenyuDict() throws Exception {
        ShenyuDictDTO shenyuDictDTO = buildTestDict();
        
        SpringBeanUtils.getInstance().setApplicationContext(mock(ConfigurableApplicationContext.class));
        when(SpringBeanUtils.getInstance().getBean(ShenyuDictMapper.class)).thenReturn(shenyuDictMapper);
        when(shenyuDictMapper.existed(shenyuDictDTO.getId())).thenReturn(true);
        
        given(this.shenyuDictService.createOrUpdate(shenyuDictDTO)).willReturn(1);
        this.mockMvc.perform(MockMvcRequestBuilders.put("/shenyu-dict/{id}", "123")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(shenyuDictDTO)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.UPDATE_SUCCESS)))
                .andReturn();
    }
    
    @Test
    public void testDeleteShenyuDicts() throws Exception {

        ShenyuDictDTO shenyuDictDTO = new ShenyuDictDTO();
        shenyuDictDTO.setId(UUID.randomUUID().toString());
        shenyuDictDTO.setType("mode");
        shenyuDictDTO.setDictName("test");
        shenyuDictDTO.setDictValue("v");
        shenyuDictDTO.setSort(1);
        given(this.shenyuDictService.deleteShenyuDicts(Collections.singletonList(shenyuDictDTO.getId()))).willReturn(1);
        this.mockMvc.perform(MockMvcRequestBuilders.delete("/shenyu-dict/batch")
                .contentType(MediaType.APPLICATION_JSON)
                .content("[\"" + shenyuDictDTO.getId() + "\"]"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.DELETE_SUCCESS)))
                .andReturn();
    }
    
    @Test
    public void testBatchEnabled() throws Exception {
        BatchCommonDTO batchCommonDTO = new BatchCommonDTO();
        batchCommonDTO.setEnabled(false);
        batchCommonDTO.setIds(Collections.singletonList("123"));
        given(this.shenyuDictService.enabled(batchCommonDTO.getIds(), batchCommonDTO.getEnabled())).willReturn(1);
        this.mockMvc.perform(MockMvcRequestBuilders.post("/shenyu-dict/batchEnabled")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(batchCommonDTO)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is("batch enable success")))
                .andReturn();
    }
    
    private ShenyuDictDTO buildTestDict() {
        ShenyuDictDTO shenyuDictDTO = new ShenyuDictDTO();
        shenyuDictDTO.setId("123");
        shenyuDictDTO.setType("mode");
        shenyuDictDTO.setDictName("test");
        shenyuDictDTO.setDictValue("v");
        shenyuDictDTO.setDictCode("code");
        shenyuDictDTO.setSort(1);
        shenyuDictDTO.setEnabled(true);
        return shenyuDictDTO;
    }
}
