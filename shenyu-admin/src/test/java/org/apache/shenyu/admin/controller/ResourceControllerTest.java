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

import static com.google.common.collect.Lists.newArrayList;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.mock;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;

import org.apache.shenyu.admin.exception.ExceptionHandlers;
import org.apache.shenyu.admin.mapper.ResourceMapper;
import org.apache.shenyu.admin.model.dto.CreateResourceDTO;
import org.apache.shenyu.admin.model.dto.ResourceDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.ResourceQuery;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.model.vo.PermissionMenuVO;
import org.apache.shenyu.admin.model.vo.ResourceVO;
import org.apache.shenyu.admin.service.ResourceService;
import org.apache.shenyu.admin.spring.SpringBeanUtils;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.utils.GsonUtils;

import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

/**
 * test for {@linkplain ResourceController}.
 */
@ExtendWith(MockitoExtension.class)
public class ResourceControllerTest {

    private MockMvc mockMvc;

    @InjectMocks
    private ResourceController resourceController;

    @Mock
    private ResourceService resourceService;
    
    @Mock
    private ResourceMapper resourceMapper;

    @BeforeEach
    public void setUp() {
        this.mockMvc = MockMvcBuilders.standaloneSetup(resourceController)
                .setControllerAdvice(new ExceptionHandlers())
                .build();
    }

    @Test
    public void testQueryResourceResultSuccess() throws Exception {
        final String queryTitle = "query title";
        final Integer queryCurrentPage = 1;
        final Integer queryPageSize = 10;
        final PageParameter pageParameter = new PageParameter(queryCurrentPage, queryPageSize);
        CommonPager<ResourceVO> queryResult = new CommonPager<>(pageParameter, Collections.singletonList(new ResourceVO()));
        given(resourceService.listByPage(new ResourceQuery(queryTitle, new PageParameter(queryCurrentPage, queryPageSize)))).willReturn(queryResult);

        this.mockMvc.perform(MockMvcRequestBuilders.get("/resource")
                .queryParam("title", queryTitle)
                .queryParam("currentPage", String.valueOf(queryCurrentPage))
                .queryParam("pageSize", String.valueOf(queryPageSize)))
                .andExpect(content().json(GsonUtils.getInstance().toJson(ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, queryResult))))
                .andReturn();
    }

    @Test
    public void testQueryResourceResultFailed() throws Exception {
        final String queryTitle = "query title";
        final Integer queryCurrentPage = 1;
        final Integer queryPageSize = 10;
        final PageParameter pageParameter = new PageParameter(queryCurrentPage, queryPageSize);
        CommonPager<ResourceVO> queryResult = new CommonPager<>(pageParameter, Collections.emptyList());
        given(resourceService.listByPage(new ResourceQuery(queryTitle, new PageParameter(queryCurrentPage, queryPageSize)))).willReturn(queryResult);

        this.mockMvc.perform(MockMvcRequestBuilders.get("/resource")
                .queryParam("title", queryTitle)
                .queryParam("currentPage", String.valueOf(queryCurrentPage))
                .queryParam("pageSize", String.valueOf(queryPageSize)))
                .andExpect(content().json(GsonUtils.getInstance().toJson(ShenyuAdminResult.error(ShenyuResultMessage.QUERY_FAILED))))
                .andReturn();
    }

    @Test
    public void testQueryMenuTreeResultSuccess() throws Exception {
        final List<PermissionMenuVO.MenuInfo> mockResult = newArrayList(new PermissionMenuVO.MenuInfo());
        given(resourceService.getMenuTree()).willReturn(mockResult);

        this.mockMvc.perform(MockMvcRequestBuilders.get("/resource/menu"))
                .andExpect(content().json(GsonUtils.getInstance().toJson(ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, mockResult))))
                .andReturn();
    }

    @Test
    public void testQueryMenuTreeResultFailed() throws Exception {
        given(resourceService.getMenuTree()).willReturn(newArrayList());

        this.mockMvc.perform(MockMvcRequestBuilders.get("/resource/menu"))
                .andExpect(content().json(GsonUtils.getInstance().toJson(ShenyuAdminResult.error(ShenyuResultMessage.QUERY_FAILED))))
                .andReturn();
    }

    @Test
    public void testGetButtonResultSuccess() throws Exception {
        final String mockId = "mock id";
        final List<ResourceVO> mockResult = newArrayList(new ResourceVO());
        given(resourceService.findByParentId(mockId)).willReturn(mockResult);

        this.mockMvc.perform(MockMvcRequestBuilders.get("/resource/button")
                .queryParam("id", mockId))
                .andExpect(content().json(GsonUtils.getInstance().toJson(ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, mockResult))))
                .andReturn();
    }

    @Test
    public void testGetButtonResultFailed() throws Exception {
        final String mockId = "mock id";
        given(resourceService.findByParentId(mockId)).willReturn(newArrayList());

        this.mockMvc.perform(MockMvcRequestBuilders.get("/resource/button")
                .queryParam("id", mockId))
                .andExpect(content().json(GsonUtils.getInstance().toJson(ShenyuAdminResult.error(ShenyuResultMessage.QUERY_FAILED))))
                .andReturn();
    }

    @Test
    public void testDetailResourceResultSuccess() throws Exception {
        final String mockId = "mock-id";
        final ResourceVO mockResult = new ResourceVO();
        given(resourceService.findById(mockId)).willReturn(mockResult);
        this.mockMvc.perform(MockMvcRequestBuilders.get("/resource/" + mockId))
                .andExpect(content().json(GsonUtils.getInstance().toJson(ShenyuAdminResult.success(ShenyuResultMessage.DETAIL_SUCCESS, mockResult))))
                .andReturn();
    }

    @Test
    public void testDetailResourceResultFailed() throws Exception {
        final String mockId = "mock-id";
        given(resourceService.findById(mockId)).willReturn(null);
        this.mockMvc.perform(MockMvcRequestBuilders.get("/resource/" + mockId))
                .andExpect(content().json(GsonUtils.getInstance().toJson(ShenyuAdminResult.error(ShenyuResultMessage.DETAIL_FAILED))))
                .andReturn();
    }

    @Test
    public void testCreateResource() throws Exception {
        final CreateResourceDTO createResourceDTO = new CreateResourceDTO();
        fill(createResourceDTO);
        given(resourceService.create(any())).willReturn(1);

        this.mockMvc.perform(MockMvcRequestBuilders.post("/resource")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(createResourceDTO)))
                .andExpect(content().json(GsonUtils.getInstance().toJson(ShenyuAdminResult.success(ShenyuResultMessage.CREATE_SUCCESS, 1))))
                .andReturn();
    }

    @Test
    public void testUpdateResource() throws Exception {
        final String mockId = "mock-id";
        final ResourceDTO resourceDTO = new ResourceDTO();
        resourceDTO.setId(mockId);
        fill(resourceDTO);
        SpringBeanUtils.getInstance().setApplicationContext(mock(ConfigurableApplicationContext.class));
        given(resourceService.update(resourceDTO)).willReturn(1);

        this.mockMvc.perform(MockMvcRequestBuilders.put("/resource/" + mockId)
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(resourceDTO)))
                .andExpect(content().json(GsonUtils.getInstance().toJson(ShenyuAdminResult.success(ShenyuResultMessage.UPDATE_SUCCESS, 1))))
                .andReturn();
    }
    
    @Test
    public void testDeleteResource() throws Exception {
        final List<String> mockParameter = newArrayList("mock-id");
        given(resourceService.delete(mockParameter)).willReturn(1);

        this.mockMvc.perform(MockMvcRequestBuilders.delete("/resource/batch")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(mockParameter)))
                .andExpect(content().json(GsonUtils.getInstance().toJson(ShenyuAdminResult.success(ShenyuResultMessage.DELETE_SUCCESS, 1))))
                .andReturn();
    }
    
    private void fill(final ResourceDTO resourceDTO) {
        resourceDTO.setTitle("test");
        resourceDTO.setName("test");
        resourceDTO.setParentId("test");
        resourceDTO.setUrl("test");
        resourceDTO.setComponent("test");
        resourceDTO.setIcon("test");
        resourceDTO.setPerms("test");
        resourceDTO.setIsLeaf(true);
        resourceDTO.setIsRoute(1);
        resourceDTO.setResourceType(1);
        resourceDTO.setStatus(1);
        resourceDTO.setSort(1);
    }

    private void fill(final CreateResourceDTO createResourceDTO) {
        createResourceDTO.setTitle("test");
        createResourceDTO.setName("test");
        createResourceDTO.setParentId("test");
        createResourceDTO.setUrl("test");
        createResourceDTO.setComponent("test");
        createResourceDTO.setIcon("test");
        createResourceDTO.setPerms("test");
        createResourceDTO.setIsLeaf(true);
        createResourceDTO.setIsRoute(1);
        createResourceDTO.setResourceType(1);
        createResourceDTO.setStatus(1);
        createResourceDTO.setSort(1);
    }
}
