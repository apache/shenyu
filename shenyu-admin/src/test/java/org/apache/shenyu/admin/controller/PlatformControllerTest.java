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

import org.apache.shenyu.admin.service.DashboardUserService;
import org.apache.shenyu.admin.service.EnumService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.admin.model.vo.DashboardUserVO;
import org.apache.shenyu.admin.model.vo.LoginDashboardUserVO;
import org.apache.shenyu.common.exception.CommonErrorCode;
import org.apache.shenyu.common.utils.DateUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpMethod;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.time.LocalDateTime;

import static org.hamcrest.core.Is.is;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;


/**
 * test case for PlatformController.
 */
@ExtendWith(MockitoExtension.class)
public final class PlatformControllerTest {

    private MockMvc mockMvc;

    @InjectMocks
    private PlatformController platformController;

    @Mock
    private DashboardUserService dashboardUserService;

    @Mock
    private EnumService enumService;

    /**
     * dashboardUser mock data.
     */
    private final DashboardUserVO dashboardUserVO = new DashboardUserVO("1", "admin", "2095132720951327",
            1, true, DateUtils.localDateTimeToString(LocalDateTime.now()),
            DateUtils.localDateTimeToString(LocalDateTime.now()));

    /**
     * init mockmvc.
     */
    @BeforeEach
    public void setUp() {
        this.mockMvc = MockMvcBuilders.standaloneSetup(platformController).build();
    }

    /**
     * test method loginDashboardUser.
     */
    @Test
    public void testLoginDashboardUser() throws Exception {
        final String loginUri = "/platform/login?userName=admin&password=123456";

        LoginDashboardUserVO loginDashboardUserVO = LoginDashboardUserVO.buildLoginDashboardUserVO(dashboardUserVO);
        given(this.dashboardUserService.login(eq("admin"), eq("123456"))).willReturn(loginDashboardUserVO);
        this.mockMvc.perform(MockMvcRequestBuilders.request(HttpMethod.GET, loginUri))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.code", is(CommonErrorCode.SUCCESSFUL)))
                .andExpect(jsonPath("$.message", is(ShenyuResultMessage.PLATFORM_LOGIN_SUCCESS)))
                .andExpect(jsonPath("$.data.id", is(loginDashboardUserVO.getId())))
                .andReturn();
    }

    /**
     * test method queryEnums.
     */
    @Test
    public void testQueryEnums() throws Exception {
        final String queryEnumsUri = "/platform/enum";

        this.mockMvc.perform(MockMvcRequestBuilders.request(HttpMethod.GET, queryEnumsUri))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.code", is(CommonErrorCode.SUCCESSFUL)))
                .andExpect(jsonPath("$.data", is(this.enumService.list())))
                .andReturn();
    }
}
