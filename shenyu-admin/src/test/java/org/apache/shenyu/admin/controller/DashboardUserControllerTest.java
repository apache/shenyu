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

import org.apache.shenyu.admin.config.properties.SecretProperties;
import org.apache.shenyu.admin.model.dto.DashboardUserDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.service.DashboardUserService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.admin.model.vo.DashboardUserEditVO;
import org.apache.shenyu.admin.model.vo.DashboardUserVO;
import org.apache.shenyu.admin.model.vo.RoleVO;
import org.apache.shenyu.common.utils.GsonUtils;
import org.assertj.core.util.Lists;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.http.MediaType;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.hamcrest.core.Is.is;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.mock;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Test cases for {@link DashboardUserController}.
 */
@RunWith(MockitoJUnitRunner.class)
public final class DashboardUserControllerTest {

    private MockMvc mockMvc;

    @InjectMocks
    private DashboardUserController dashboardUserController;

    @Mock
    private DashboardUserService dashboardUserService;

    private final DashboardUserVO dashboardUserVO = new DashboardUserVO("id",
            "userName",
            "bbiB8zbUo3z3oA0VqEB/IA==",
            0,
            false,
            "dateCreated",
            "dateUpdated");

    private final DashboardUserDTO dashboardUserDTO = new DashboardUserDTO("2", "userName",
            "123456", 0, new ArrayList<>(), false);

    @Before
    public void setUp() throws Exception {
        final SecretProperties secretProperties = new SecretProperties();
        secretProperties.setKey("2095132720951327");
        secretProperties.setIv("6075877187097700");
        ReflectionTestUtils.setField(dashboardUserController, "secretProperties", secretProperties);
        mockMvc = MockMvcBuilders.standaloneSetup(dashboardUserController).build();
    }

    @Test
    public void queryDashboardUsers() throws Exception {
        final CommonPager<DashboardUserVO> commonPager = new CommonPager<>(new PageParameter(),
                Collections.singletonList(dashboardUserVO));
        given(dashboardUserService.listByPage(any())).willReturn(commonPager);
        final String url = "/dashboardUser";
        mockMvc.perform(get(url))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.QUERY_SUCCESS)))
                .andExpect(jsonPath("$.data.dataList[0].password", is("123456")))
                .andReturn();

        final CommonPager<DashboardUserVO> commonPagerError = new CommonPager<>(new PageParameter(),
                Collections.emptyList());
        given(dashboardUserService.listByPage(any())).willReturn(commonPagerError);
        mockMvc.perform(get(url))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.DASHBOARD_QUERY_ERROR)))
                .andReturn();
    }

    @Test
    public void detailDashboardUser() throws Exception {
        List<RoleVO> roles = new ArrayList<>();
        roles.add(mock(RoleVO.class));
        List<RoleVO> allRoles = new ArrayList<>();
        allRoles.add(mock(RoleVO.class));
        DashboardUserEditVO dashboardUserEditVO = DashboardUserEditVO.buildDashboardUserEditVO(dashboardUserVO, roles, allRoles);
        given(dashboardUserService.findById(any())).willReturn(dashboardUserEditVO);
        final String url = "/dashboardUser/1";
        mockMvc.perform(get(url))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.DETAIL_SUCCESS)))
                .andExpect(jsonPath("$.data.password", is("")));

        given(dashboardUserService.findById(any())).willReturn(null);
        mockMvc.perform(get(url))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.DASHBOARD_QUERY_ERROR)));
    }

    @Test
    public void createDashboardUser() throws Exception {
        final String url = "/dashboardUser";
        given(dashboardUserService.createOrUpdate(any())).willReturn(1);
        mockMvc.perform(post(url, dashboardUserDTO)
                .content(GsonUtils.getInstance().toJson(dashboardUserDTO))
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andDo(print())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.CREATE_SUCCESS)))
                .andExpect(jsonPath("$.data", is(1)));
    }

    @Test
    public void updateDashboardUser() throws Exception {
        final String url = "/dashboardUser/2";
        given(dashboardUserService.createOrUpdate(any())).willReturn(1);
        mockMvc.perform(put(url, dashboardUserDTO)
                .content(GsonUtils.getInstance().toJson(dashboardUserDTO))
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andDo(print())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.UPDATE_SUCCESS)))
                .andExpect(jsonPath("$.data", is(1)));
    }

    @Test
    public void deleteDashboardUser() throws Exception {
        final String url = "/dashboardUser/batch";
        final List<String> ids = Lists.newArrayList();
        given(dashboardUserService.delete(any())).willReturn(0);
        mockMvc.perform(delete(url, ids)
                .content(GsonUtils.getInstance().toJson(ids))
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andDo(print())
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.DELETE_SUCCESS)))
                .andExpect(jsonPath("$.data", is(0)));
    }
}
