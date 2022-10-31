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

import org.apache.shenyu.admin.mapper.RoleMapper;
import org.apache.shenyu.admin.model.dto.RoleDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.RoleQuery;
import org.apache.shenyu.admin.model.vo.RoleEditVO;
import org.apache.shenyu.admin.model.vo.RoleVO;
import org.apache.shenyu.admin.service.RoleService;
import org.apache.shenyu.admin.spring.SpringBeanUtils;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.utils.DateUtils;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.UUIDUtils;
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
import java.util.List;

import static org.hamcrest.core.Is.is;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Test cases for RoleController.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class RoleControllerTest {

    private static final String SUPER = "super";

    private MockMvc mockMvc;

    @InjectMocks
    private RoleController roleController;

    @Mock
    private RoleService roleService;
    
    @Mock
    private RoleMapper roleMapper;

    @BeforeEach
    public void setUp() {
        this.mockMvc = MockMvcBuilders.standaloneSetup(roleController).build();
    }

    @Test
    public void testSelectAll() throws Exception {
        given(roleService.selectAll()).willReturn(Collections.emptyList());
        this.mockMvc.perform(MockMvcRequestBuilders.get("/role/getAllRoles"))
            .andExpect(status().isOk())
            .andExpect(jsonPath("$.message", is(ShenyuResultMessage.QUERY_SUCCESS)))
            .andReturn();
    }

    @Test
    public void testQueryRole() throws Exception {
        RoleVO roleVO = buildRoleVO();
        PageParameter pageParameter = new PageParameter();
        RoleQuery query = new RoleQuery(roleVO.getRoleName(), pageParameter);
        given(roleService.listByPage(query)).willReturn(new CommonPager<>(pageParameter, Collections.singletonList(roleVO)));
        String urlTemplate = "/role?roleName={roleName}&currentPage={currentPage}&pageSize={pageSize}";
        this.mockMvc.perform(MockMvcRequestBuilders.get(urlTemplate, roleVO.getRoleName(), pageParameter.getCurrentPage(), pageParameter.getPageSize()))
            .andExpect(status().isOk())
            .andExpect(jsonPath("$.message", is(ShenyuResultMessage.QUERY_SUCCESS)))
            .andExpect(jsonPath("$.data.dataList[0].id", is(roleVO.getId())))
            .andReturn();
    }

    @Test
    public void testDetailRole() throws Exception {
        given(roleService.findById(anyString())).willReturn(null);
        String urlTemplate = "/role/{id}";
        this.mockMvc.perform(MockMvcRequestBuilders.get(urlTemplate, "test_id"))
            .andExpect(status().isOk())
            .andExpect(jsonPath("$.message", is(ShenyuResultMessage.DETAIL_FAILED)))
            .andReturn();
        RoleEditVO roleEditVO = buildRoleEditVO();
        given(roleService.findById(roleEditVO.getSysRole().getId())).willReturn(roleEditVO);
        urlTemplate = "/role/{id}";
        this.mockMvc.perform(MockMvcRequestBuilders.get(urlTemplate, roleEditVO.getSysRole().getId()))
            .andExpect(status().isOk())
            .andExpect(jsonPath("$.message", is(ShenyuResultMessage.DETAIL_SUCCESS)))
            .andReturn();
    }

    @Test
    public void testCreateRole() throws Exception {
        RoleDTO roleDTO = buildRoleDTO();
        given(roleService.createOrUpdate(roleDTO)).willReturn(1);
        SpringBeanUtils.getInstance().setApplicationContext(mock(ConfigurableApplicationContext.class));
        when(SpringBeanUtils.getInstance().getBean(RoleMapper.class)).thenReturn(roleMapper);
        when(roleMapper.existed(roleDTO.getId())).thenReturn(true);
        this.mockMvc.perform(MockMvcRequestBuilders.post("/role")
            .contentType(MediaType.APPLICATION_JSON)
            .content(GsonUtils.getInstance().toJson(roleDTO)))
            .andExpect(status().isOk())
            .andExpect(jsonPath("$.message", is(ShenyuResultMessage.CREATE_SUCCESS)))
            .andReturn();
        roleDTO.setRoleName(SUPER);
        given(roleService.createOrUpdate(roleDTO)).willReturn(1);
        this.mockMvc.perform(MockMvcRequestBuilders.post("/role")
            .contentType(MediaType.APPLICATION_JSON)
            .content(GsonUtils.getInstance().toJson(roleDTO)))
            .andExpect(status().isOk())
            .andExpect(jsonPath("$.message", is(ShenyuResultMessage.ROLE_CREATE_ERROR)))
            .andReturn();
    }

    @Test
    public void testUpdateRole() throws Exception {
        RoleDTO roleDTO = buildRoleDTO();
        given(roleService.createOrUpdate(roleDTO)).willReturn(1);
        SpringBeanUtils.getInstance().setApplicationContext(mock(ConfigurableApplicationContext.class));
        when(SpringBeanUtils.getInstance().getBean(RoleMapper.class)).thenReturn(roleMapper);
        when(roleMapper.existed(roleDTO.getId())).thenReturn(true);
        this.mockMvc.perform(MockMvcRequestBuilders.put("/role/{id}", roleDTO.getId())
            .contentType(MediaType.APPLICATION_JSON)
            .content(GsonUtils.getInstance().toJson(roleDTO)))
            .andExpect(status().isOk())
            .andExpect(jsonPath("$.message", is(ShenyuResultMessage.UPDATE_SUCCESS)))
            .andReturn();
    }

    @Test
    public void testDeleteRole() throws Exception {
        List<String> testIds = Collections.singletonList("test_id");
        given(roleService.delete(testIds)).willReturn(1);
        this.mockMvc.perform(MockMvcRequestBuilders.delete("/role/batch")
            .contentType(MediaType.APPLICATION_JSON)
            .content(GsonUtils.getInstance().toJson(testIds)))
            .andExpect(status().isOk())
            .andExpect(jsonPath("$.message", is(ShenyuResultMessage.DELETE_SUCCESS)))
            .andReturn();

    }

    private RoleVO buildRoleVO() {
        return new RoleVO(UUIDUtils.getInstance().generateShortUuid(),
            "test_role_name",
            "description",
            DateUtils.localDateTimeToString(LocalDateTime.now()),
            DateUtils.localDateTimeToString(LocalDateTime.now()));
    }

    private RoleEditVO buildRoleEditVO() {
        return new RoleEditVO(Collections.singletonList(""), buildRoleVO(), null);
    }

    private RoleDTO buildRoleDTO() {
        return RoleDTO.builder()
            .id(UUIDUtils.getInstance().generateShortUuid())
            .roleName("test_role")
            .description("role desc")
            .build();
    }

}
