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
import java.util.Date;
import java.util.List;
import org.apache.shenyu.admin.exception.ExceptionHandlers;
import org.apache.shenyu.admin.model.dto.TagDTO;
import org.apache.shenyu.admin.model.vo.TagVO;
import org.apache.shenyu.admin.service.TagService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.utils.GsonUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import static org.hamcrest.core.Is.is;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Test cases for {@link TagController}.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class TagControllerTest {

    private MockMvc mockMvc;

    @InjectMocks
    private TagController tagController;

    @Mock
    private TagService tagService;

    @BeforeEach
    public void setUp() {
        this.mockMvc = MockMvcBuilders.standaloneSetup(tagController)
                .setControllerAdvice(new ExceptionHandlers())
                .build();
    }

    @Test
    public void testCreateTag() throws Exception {
        TagDTO tagDTO = buildTagDTO();
        given(tagService.create(tagDTO)).willReturn(1);
        this.mockMvc.perform(MockMvcRequestBuilders.post("/tag")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(GsonUtils.getInstance().toJson(tagDTO)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.CREATE_SUCCESS)))
                .andReturn();
    }

    @Test
    public void testQueryRootTag() throws Exception {
        List<TagVO> tagVOS = new ArrayList<>();
        given(tagService.findByParentTagId("0")).willReturn(tagVOS);
        this.mockMvc.perform(MockMvcRequestBuilders.get("/tag/queryRootTag"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.QUERY_SUCCESS)))
                .andReturn();
    }

    @Test
    public void testqueryById() throws Exception {
        given(tagService.findById("123")).willReturn(buildTagVO());
        this.mockMvc.perform(MockMvcRequestBuilders.get("/tag/id/{id}", "123"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.DETAIL_SUCCESS)))
                .andReturn();
    }

    @Test
    public void testQueryListByParentTagId() throws Exception {
        List<TagVO> list = new ArrayList<>();
        list.add(buildTagVO());
        given(tagService.findByParentTagId(anyString())).willReturn(list);
        this.mockMvc.perform(MockMvcRequestBuilders.get("/tag/parentTagId/{parentTagId}", "123"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.DETAIL_SUCCESS)))
                .andReturn();
    }

    @Test
    public void testQueryByName() throws Exception {
        List<TagVO> list = new ArrayList<>();
        list.add(buildTagVO());
        given(tagService.findByQuery(anyString())).willReturn(list);
        this.mockMvc.perform(MockMvcRequestBuilders.get("/tag/name/{name}", "123"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.DETAIL_SUCCESS)))
                .andReturn();
    }

    @Test
    public void testUpdateTag() throws Exception {
        TagDTO tagDTO = buildTagDTO();
        given(tagService.update(tagDTO)).willReturn(1);
        this.mockMvc.perform(MockMvcRequestBuilders.put("/tag/id/{id}", "123")
                        .contentType(MediaType.APPLICATION_JSON)
                        .param("id", "123")
                        .content(GsonUtils.getInstance().toJson(tagDTO)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.UPDATE_SUCCESS)))
                .andReturn();
    }

    @Test
    public void testDeleteTags() throws Exception {
        List<String> ids = new ArrayList<>();
        ids.add("123");
        ids.add("456");
        given(tagService.delete(ids)).willReturn(ids.size());
        this.mockMvc.perform(MockMvcRequestBuilders.delete("/tag/batchDelete")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(GsonUtils.getInstance().toJson(ids)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.DELETE_SUCCESS)))
                .andExpect(jsonPath("$.data", is(2)))
                .andReturn();
    }

    /**
     *  buildTagDTO.
     * @return tagDTO
     */
    public TagDTO buildTagDTO() {
        TagDTO tagDTO = new TagDTO();
        tagDTO.setTagDesc("this is a tag");
        tagDTO.setName("test tag");
        tagDTO.setId(null);
        tagDTO.setParentTagId("123");
        return tagDTO;
    }

    /**
     * buildTagVO.
     * @return TagVO
     */
    public TagVO buildTagVO() {
        TagVO tagVO = new TagVO();
        tagVO.setTagDesc("123");
        tagVO.setName("test tag");
        tagVO.setDateCreated(new Date().toString());
        tagVO.setDateUpdated(new Date().toString());
        return tagVO;
    }

}
