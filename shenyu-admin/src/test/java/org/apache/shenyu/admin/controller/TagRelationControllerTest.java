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
import org.apache.shenyu.admin.exception.ExceptionHandlers;
import org.apache.shenyu.admin.model.dto.TagRelationDTO;
import org.apache.shenyu.admin.model.entity.TagRelationDO;
import org.apache.shenyu.admin.service.TagRelationService;
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
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Test cases for {@link TagRelationController}.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class TagRelationControllerTest {

    private MockMvc mockMvc;

    @InjectMocks
    private TagRelationController tagRelationController;

    @Mock
    private TagRelationService tagRelationService;

    @BeforeEach
    public void setUp() {
        this.mockMvc = MockMvcBuilders.standaloneSetup(tagRelationController)
                .setControllerAdvice(new ExceptionHandlers())
                .build();
    }

    @Test
    public void testQueryApiByTagId() throws Exception {
        List<TagRelationDO> tagRelationDOS = new ArrayList<>();
        tagRelationDOS.add(buildTagRelationDO());
        given(tagRelationService.findByTagId(anyString())).willReturn(tagRelationDOS);
        this.mockMvc.perform(MockMvcRequestBuilders.get("/tag-relation/tagId/{tagId}", "123"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.DETAIL_SUCCESS)))
                .andReturn();
    }

    @Test
    public void testUpdateTagRelation() throws Exception {
        TagRelationDTO tagRelationDTO = buildTagRelationDTO();
        given(tagRelationService.update(any())).willReturn(1);
        this.mockMvc.perform(MockMvcRequestBuilders.put("/tag-relation/id/123")
                        .contentType(MediaType.APPLICATION_JSON)
                        .param("id", "123")
                        .content(GsonUtils.getInstance().toJson(tagRelationDTO)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.UPDATE_SUCCESS)))
                .andReturn();
    }

    @Test
    public void testDeleteTagRelation() throws Exception {
        List<String> ids = new ArrayList<>();
        ids.add("123");
        ids.add("456");
        given(tagRelationService.delete(ids)).willReturn(ids.size());
        this.mockMvc.perform(MockMvcRequestBuilders.delete("/tag-relation/batchDelete")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(GsonUtils.getInstance().toJson(ids)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.DELETE_SUCCESS)))
                .andExpect(jsonPath("$.data", is(2)))
                .andReturn();
    }

    /**
     * buildTagRelationDO.
     * @return TagRelationDO
     */
    private TagRelationDO buildTagRelationDO() {
        TagRelationDO tagRelationDO = new TagRelationDO();
        tagRelationDO.setTagId("123");
        tagRelationDO.setApiId("124");
        return tagRelationDO;
    }

    /**
     * buildTagRelationDTO.
     * @return TagRelationDTO
     */
    private TagRelationDTO buildTagRelationDTO() {
        TagRelationDTO tagRelationDTO = new TagRelationDTO();
        tagRelationDTO.setTagId("123");
        tagRelationDTO.setApiId("12334");
        return tagRelationDTO;
    }
}
