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

import java.sql.Timestamp;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import org.apache.shenyu.admin.exception.ExceptionHandlers;
import org.apache.shenyu.admin.model.dto.ProxyGatewayDTO;
import org.apache.shenyu.admin.model.entity.AppAuthDO;
import org.apache.shenyu.admin.service.AppAuthService;
import org.apache.shenyu.common.utils.GsonUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.RequestBuilder;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

/**
 * Test case for SandboxControllerTest.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class SandboxControllerTest {

    private MockMvc mockMvc;

    @InjectMocks
    private SandboxController sandboxController;

    @Mock
    private AppAuthService appAuthService;

    @BeforeEach
    public void setUp() {
        this.mockMvc = MockMvcBuilders.standaloneSetup(sandboxController)
                .setControllerAdvice(new ExceptionHandlers())
                .build();
    }

    @Test
    public void testProxyGateway() throws Exception {
        AppAuthDO appAuthDO = buildAppAuthDO();
        ProxyGatewayDTO proxyGatewayDTO = buildProxyGatewayDTO();
        when(this.appAuthService.findByAppKey(any())).thenReturn(appAuthDO);
        MockHttpServletResponse response = mockMvc.perform((RequestBuilder) MockMvcRequestBuilders.post("/sandbox/proxyGateway")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(GsonUtils.getInstance().toJson(proxyGatewayDTO)))
                .andReturn().getResponse();
        // Verify the results
        assertThat(response.getStatus()).isEqualTo(HttpStatus.OK.value());
    }

    private ProxyGatewayDTO buildProxyGatewayDTO() {
        ProxyGatewayDTO proxyGatewayDTO = new ProxyGatewayDTO();
        proxyGatewayDTO.setAppKey("123456");
        proxyGatewayDTO.setCookie("12233333");
        proxyGatewayDTO.setRequestUrl("http://baidu.com");
        Map<String, String> headerMap = new HashMap<>();
        headerMap.put("name", "123");
        proxyGatewayDTO.setHeaders(GsonUtils.getInstance().toJson(headerMap));
        Map<String, String> map = new HashMap<>();
        map.put("shopId", "134");
        proxyGatewayDTO.setBizParam(GsonUtils.getInstance().toJson(map));

        return proxyGatewayDTO;
    }

    private AppAuthDO buildAppAuthDO() {
        AppAuthDO appAuthDO = new AppAuthDO();
        appAuthDO.setAppKey("123456");
        appAuthDO.setAppSecret("133333");
        appAuthDO.setEnabled(true);
        appAuthDO.setExtInfo("");
        appAuthDO.setOpen(true);
        appAuthDO.setPhone("1397891xxxx");
        appAuthDO.setUserId("zhangsan");
        appAuthDO.setDateCreated(new Timestamp(new Date().getTime()));
        appAuthDO.setDateUpdated(new Timestamp(new Date().getTime()));
        return appAuthDO;
    }

}
