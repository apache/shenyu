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
import org.apache.shenyu.admin.mapper.ApiMapper;
import org.apache.shenyu.admin.model.dto.ApiDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.ApiQuery;
import org.apache.shenyu.admin.model.vo.ApiVO;
import org.apache.shenyu.admin.service.ApiService;
import org.apache.shenyu.admin.spring.SpringBeanUtils;
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
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.hamcrest.core.Is.is;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Test cases for ApiController.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class ApiControllerTest {

    private MockMvc mockMvc;

    @InjectMocks
    private ApiController apiController;

    @Mock
    private ApiService apiService;

    @Mock
    private ApiMapper apiMapper;

    private ApiVO apiVO;

    @BeforeEach
    public void setUp() {
        this.mockMvc = MockMvcBuilders.standaloneSetup(apiController)
                .setControllerAdvice(new ExceptionHandlers())
                .build();
        this.apiVO = ApiVO.builder()
                .id("123")
                .contextPath("string")
                .apiPath("string")
                .httpMethod(0)
                .consume("string")
                .produce("string")
                .version("string")
                .rpcType("/dubbo")
                .state(0)
                .apiSource(0)
                .document("document")
                .build();
        SpringBeanUtils.getInstance().setApplicationContext(mock(ConfigurableApplicationContext.class));
    }

    @Test
    public void testQueryApis() throws Exception {
        final PageParameter pageParameter = new PageParameter();
        List<ApiVO> apiVOS = new ArrayList<>();
        apiVOS.add(apiVO);
        final CommonPager<ApiVO> commonPager = new CommonPager<>();
        commonPager.setPage(pageParameter);
        commonPager.setDataList(apiVOS);
        final ApiQuery apiQuery = new ApiQuery("string", 0, "", pageParameter);
        given(this.apiService.listByPage(apiQuery)).willReturn(commonPager);
        this.mockMvc.perform(MockMvcRequestBuilders.get("/api")
                        .param("apiPath", "string")
                        .param("state", "0")
                        .param("tagId", "")
                        .param("currentPage", pageParameter.getCurrentPage() + "")
                        .param("pageSize", pageParameter.getPageSize() + ""))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.QUERY_SUCCESS)))
                .andExpect(jsonPath("$.data.dataList[0].contextPath", is(apiVO.getContextPath())))
                .andReturn();
    }

    @Test
    public void testDetailApi() throws Exception {
        given(this.apiService.findById("123")).willReturn(apiVO);
        this.mockMvc.perform(MockMvcRequestBuilders.get("/api/{id}", "123"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.DETAIL_SUCCESS)))
                .andExpect(jsonPath("$.data.id", is(apiVO.getId())))
                .andReturn();
    }

    @Test
    public void testCreateApi() throws Exception {
        ApiDTO apiDTO = new ApiDTO();
        apiDTO.setContextPath("string");
        apiDTO.setApiPath("string");
        apiDTO.setHttpMethod(0);
        apiDTO.setConsume("string");
        apiDTO.setProduce("string");
        apiDTO.setVersion("string");
        apiDTO.setRpcType("/dubbo");
        apiDTO.setState(0);
        apiDTO.setApiOwner("string");
        apiDTO.setApiDesc("string");
        apiDTO.setApiSource(0);
        apiDTO.setDocument("document");
        apiDTO.setExt("ext");
        given(this.apiService.createOrUpdate(apiDTO)).willReturn(ShenyuResultMessage.CREATE_SUCCESS);
        this.mockMvc.perform(MockMvcRequestBuilders.post("/api")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(GsonUtils.getInstance().toJson(apiDTO)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.CREATE_SUCCESS)))
                .andReturn();
    }

    @Test
    public void testCreateApiStateMinException() throws Exception {
        ApiDTO apiDTO = new ApiDTO();
        apiDTO.setContextPath("string");
        apiDTO.setApiPath("string");
        apiDTO.setHttpMethod(0);
        apiDTO.setConsume("string");
        apiDTO.setProduce("string");
        apiDTO.setVersion("string");
        apiDTO.setRpcType("/dubbo");
        apiDTO.setState(-1);
        apiDTO.setApiOwner("string");
        apiDTO.setApiDesc("string");
        apiDTO.setApiSource(0);
        apiDTO.setDocument("document");
        apiDTO.setExt("ext");
        this.mockMvc.perform(MockMvcRequestBuilders.post("/api")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(GsonUtils.getInstance().toJson(apiDTO)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is("Request error! invalid argument [state: state min 0]")))
                .andReturn();
    }

    @Test
    public void testCreateApiStateMaxException() throws Exception {
        ApiDTO apiDTO = new ApiDTO();
        apiDTO.setContextPath("string");
        apiDTO.setApiPath("string");
        apiDTO.setHttpMethod(0);
        apiDTO.setConsume("string");
        apiDTO.setProduce("string");
        apiDTO.setVersion("string");
        apiDTO.setRpcType("/dubbo");
        apiDTO.setState(8);
        apiDTO.setApiOwner("string");
        apiDTO.setApiDesc("string");
        apiDTO.setApiSource(0);
        apiDTO.setDocument("document");
        apiDTO.setExt("ext");
        this.mockMvc.perform(MockMvcRequestBuilders.post("/api")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(GsonUtils.getInstance().toJson(apiDTO)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is("Request error! invalid argument [state: state max 2]")))
                .andReturn();
    }

    @Test
    public void testUpdateApi() throws Exception {
        ApiDTO apiDTO = new ApiDTO();
        apiDTO.setContextPath("string");
        apiDTO.setApiPath("string");
        apiDTO.setHttpMethod(0);
        apiDTO.setConsume("string");
        apiDTO.setProduce("string");
        apiDTO.setVersion("string");
        apiDTO.setRpcType("/dubbo");
        apiDTO.setState(0);
        apiDTO.setApiOwner("string");
        apiDTO.setApiDesc("string");
        apiDTO.setApiSource(0);
        apiDTO.setDocument("document");
        apiDTO.setExt("ext");
        apiDTO.setId("123");
        when(SpringBeanUtils.getInstance().getBean(ApiMapper.class)).thenReturn(apiMapper);
        when(apiMapper.existed(apiDTO.getId())).thenReturn(true);
        given(this.apiService.createOrUpdate(apiDTO)).willReturn(ShenyuResultMessage.UPDATE_SUCCESS);
        this.mockMvc.perform(MockMvcRequestBuilders.put("/api/{id}", apiDTO.getId())
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(GsonUtils.getInstance().toJson(apiDTO)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.UPDATE_SUCCESS)))
                .andReturn();

    }

    @Test
    public void testDeleteApis() throws Exception {
        given(this.apiService.delete(Collections.singletonList("123"))).willReturn(StringUtils.EMPTY);
        this.mockMvc.perform(MockMvcRequestBuilders.delete("/api/batch")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("[\"123\"]"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.DELETE_SUCCESS)))
                .andReturn();

    }

}
