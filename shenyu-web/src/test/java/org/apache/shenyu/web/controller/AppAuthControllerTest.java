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

package org.apache.shenyu.web.controller;

import java.util.ArrayList;
import java.util.List;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.AuthParamData;
import org.apache.shenyu.common.dto.AuthPathData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.verify;

/**
 * Test cases for AppAuthController.
 */
@RunWith(MockitoJUnitRunner.class)
public final class AppAuthControllerTest {

    private MockMvc mockMvc;

    @InjectMocks
    private AppAuthController appAuthController;

    private AppAuthData appAuthData;

    @Mock
    private AuthDataSubscriber authDataSubscriber;

    @Before
    public void setUp() throws Exception {
        this.mockMvc = MockMvcBuilders.standaloneSetup(appAuthController).build();
        appAuthData = initAppAuthDataList();
    }

    @Test
    public void testSaveOrUpdate() throws Exception {

        final MockHttpServletResponse response = this.mockMvc.perform(MockMvcRequestBuilders.post("/shenyu/auth/saveOrUpdate")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(appAuthData)))
                .andReturn().getResponse();
        assertThat(response.getStatus()).isEqualTo(HttpStatus.OK.value());
        verify(authDataSubscriber).onSubscribe(appAuthData);
    }

    @Test
    public void testClean() throws Exception {
        String appKey = "D9FD95F496C9495DB5604778A13C3D08";
        final MockHttpServletResponse response = this.mockMvc.perform(MockMvcRequestBuilders.get("/shenyu/auth/delete")
                .contentType(MediaType.APPLICATION_JSON)
                .queryParam("appKey", appKey))
                .andReturn().getResponse();
        assertThat(response.getStatus()).isEqualTo(HttpStatus.OK.value());
        AppAuthData appAuthData = new AppAuthData();
        appAuthData.setAppKey(appKey);
        verify(authDataSubscriber).unSubscribe(appAuthData);
    }

    private AppAuthData initAppAuthDataList() {
        AppAuthData appAuthData = new AppAuthData();
        appAuthData.setAppKey("D9FD95F496C9495DB5604778A13C3D08");
        appAuthData.setAppSecret("02D25048AA1E466F8920E68B08E668DE");
        appAuthData.setEnabled(true);
        appAuthData.setParamDataList(buildAuthParamDataList("testKey", "123"));
        appAuthData.setPathDataList(buildAuthPathDataList("testKey", "/test"));
        return appAuthData;
    }

    private List<AuthParamData> buildAuthParamDataList(final String appName, final String appParam) {
        AuthParamData authParamData = new AuthParamData();
        authParamData.setAppName(appName);
        authParamData.setAppParam(appParam);

        List<AuthParamData> authParamDataList = new ArrayList<>();
        authParamDataList.add(authParamData);
        return authParamDataList;
    }

    private List<AuthPathData> buildAuthPathDataList(final String appName, final String path) {
        AuthPathData authPathData = new AuthPathData();
        authPathData.setAppName(appName);
        authPathData.setEnabled(true);
        authPathData.setPath(path);

        List<AuthPathData> authPathDataList = new ArrayList<>();
        authPathDataList.add(authPathData);
        return authPathDataList;
    }
}
