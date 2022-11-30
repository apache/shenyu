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
import org.apache.shenyu.admin.exception.ExceptionHandlers;
import org.apache.shenyu.admin.model.dto.MockRequestRecordDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.vo.MockRequestRecordVO;
import org.apache.shenyu.admin.service.MockRequestRecordService;
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
import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Test cases for MockRequestRecordControllerTest.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class MockRequestRecordControllerTest {

    private MockMvc mockMvc;

    @InjectMocks
    private MockRequestRecordController mockRequestRecordController;

    @Mock
    private MockRequestRecordService mockRequestRecordService;

    @BeforeEach
    public void setUp() {
        this.mockMvc = MockMvcBuilders.standaloneSetup(mockRequestRecordController)
                .setControllerAdvice(new ExceptionHandlers())
                .build();
    }

    @Test
    public void testCreateOrUpdate() throws Exception {
        given(mockRequestRecordService.createOrUpdate(any())).willReturn(1);
        this.mockMvc.perform(MockMvcRequestBuilders.post("/mock/insertOrUpdate")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(GsonUtils.getInstance().toJson(new MockRequestRecordDTO())))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.SUCCESS)))
                .andReturn();
    }

    @Test
    public void testBatchDelete() throws Exception {
        given(mockRequestRecordService.batchDelete(any())).willReturn(1);
        this.mockMvc.perform(MockMvcRequestBuilders.delete("/mock/batchDelete")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(GsonUtils.getInstance().toJson(Lists.newArrayList("1"))))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.DELETE_SUCCESS)))
                .andReturn();
    }

    @Test
    public void testListByPage() throws Exception {
        CommonPager<MockRequestRecordVO> commonPager = new CommonPager<>();
        commonPager.setDataList(Lists.newArrayList(buildMockRequestRecordVO()));
        given(mockRequestRecordService.listByPage(any())).willReturn(commonPager);
        this.mockMvc.perform(MockMvcRequestBuilders.get("/mock/findPageByQuery")
                        .param("apiId", "123")
                        .param("currentPage", 1 + "")
                        .param("pageSize", 10 + ""))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.QUERY_SUCCESS)))
                .andExpect(jsonPath("$.data.dataList[0].apiId", is("123")))
                .andReturn();
    }

    private MockRequestRecordVO buildMockRequestRecordVO() {
        MockRequestRecordVO mockRequestRecordVO = new MockRequestRecordVO();
        mockRequestRecordVO.setApiId("123");
        mockRequestRecordVO.setId("123");
        return mockRequestRecordVO;
    }
}
