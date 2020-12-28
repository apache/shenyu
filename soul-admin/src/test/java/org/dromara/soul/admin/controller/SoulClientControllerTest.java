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

package org.dromara.soul.admin.controller;

import org.dromara.soul.admin.dto.MetaDataDTO;
import org.dromara.soul.admin.dto.SpringCloudRegisterDTO;
import org.dromara.soul.admin.dto.SpringMvcRegisterDTO;
import org.dromara.soul.admin.service.SoulClientRegisterService;
import org.dromara.soul.common.utils.GsonUtils;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Test cases for SoulClientController.
 * @author bran.chen
 */
@RunWith(MockitoJUnitRunner.class)
public final class SoulClientControllerTest {

    private MockMvc mockMvc;

    @InjectMocks
    private SoulClientController soulClientController;

    @Mock
    private SoulClientRegisterService soulClientRegisterService;

    @Before
    public void setUp() {
        this.mockMvc = MockMvcBuilders.standaloneSetup(soulClientController).build();
    }

    @Test
    public void testRegisterSpringMvc() throws Exception {
        final SpringMvcRegisterDTO springMvcRegisterDTO = buildSpringMvcRegisterDTO();
        given(this.soulClientRegisterService.registerSpringMvc(springMvcRegisterDTO)).willReturn("success");
        this.mockMvc.perform(MockMvcRequestBuilders.post("/soul-client/springmvc-register")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(springMvcRegisterDTO)))
                .andExpect(status().isOk())
                .andReturn();
    }

    private SpringMvcRegisterDTO buildSpringMvcRegisterDTO() {
        SpringMvcRegisterDTO springMvcRegisterDTO = new SpringMvcRegisterDTO();
        springMvcRegisterDTO.setAppName("appName1");
        springMvcRegisterDTO.setContext("content1");
        springMvcRegisterDTO.setPath("path1");
        springMvcRegisterDTO.setPathDesc("pathDesc1");
        springMvcRegisterDTO.setRpcType("rpcType1");
        springMvcRegisterDTO.setHost("localhost1");
        springMvcRegisterDTO.setPort(1234);
        springMvcRegisterDTO.setRuleName("ruleName1");
        springMvcRegisterDTO.setEnabled(true);
        springMvcRegisterDTO.setRegisterMetaData(true);
        return springMvcRegisterDTO;
    }

    @Test
    public void testRegisterSpringCloud() throws Exception {
        final SpringCloudRegisterDTO springCloudRegisterDTO = buildCloudRegisterDTO();
        given(this.soulClientRegisterService.registerSpringCloud(springCloudRegisterDTO)).willReturn("success");
        this.mockMvc.perform(MockMvcRequestBuilders.post("/soul-client/springcloud-register")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(springCloudRegisterDTO)))
                .andExpect(status().isOk())
                .andReturn();
    }

    private SpringCloudRegisterDTO buildCloudRegisterDTO() {
        SpringCloudRegisterDTO springCloudRegisterDTO = new SpringCloudRegisterDTO();
        springCloudRegisterDTO.setAppName("appName2");
        springCloudRegisterDTO.setContext("content2");
        springCloudRegisterDTO.setPath("path2");
        springCloudRegisterDTO.setPathDesc("pathDesc2");
        springCloudRegisterDTO.setRpcType("rpcType2");
        springCloudRegisterDTO.setRuleName("ruleName2");
        springCloudRegisterDTO.setEnabled(false);
        return springCloudRegisterDTO;
    }

    @Test
    public void testRegisterRpc() throws Exception {
        final MetaDataDTO metaDataDTO = buildMetaDataDTO("app_dubbo");
        given(this.soulClientRegisterService.registerDubbo(metaDataDTO)).willReturn("success");
        this.mockMvc.perform(MockMvcRequestBuilders.post("/soul-client/dubbo-register")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(metaDataDTO)))
                .andExpect(status().isOk())
                .andReturn();
    }

    private MetaDataDTO buildMetaDataDTO(final String appName) {
        MetaDataDTO metaDataDTO = new MetaDataDTO();
        metaDataDTO.setId("6");
        metaDataDTO.setAppName(appName);
        metaDataDTO.setContextPath("content3");
        metaDataDTO.setPath("path3");
        metaDataDTO.setRuleName("ruleName3");
        metaDataDTO.setPathDesc("pathDesc3");
        metaDataDTO.setRpcType("rpcType3");
        metaDataDTO.setServiceName("serviceName3");
        metaDataDTO.setMethodName("methodName3");
        metaDataDTO.setParameterTypes("parameterTypes3");
        metaDataDTO.setRpcExt("rpcExt3");
        metaDataDTO.setEnabled(false);
        return metaDataDTO;
    }

    @Test
    public void testRegisterSofaRpc() throws Exception {
        final MetaDataDTO metaDataDTO = buildMetaDataDTO("app_sofa");
        given(this.soulClientRegisterService.registerSofa(metaDataDTO)).willReturn("success");
        this.mockMvc.perform(MockMvcRequestBuilders.post("/soul-client/sofa-register")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(metaDataDTO)))
                .andExpect(status().isOk())
                .andReturn();
    }

    @Test
    public void testRegisterTarsRpc() {
        final MetaDataDTO metaDataDTO = new MetaDataDTO();
        metaDataDTO.setId("id");
        metaDataDTO.setAppName("appName");
        metaDataDTO.setContextPath("contextPath");
        metaDataDTO.setPath("path");
        metaDataDTO.setRuleName("ruleName");
        metaDataDTO.setPathDesc("pathDesc");
        metaDataDTO.setRpcType("rpcType");
        metaDataDTO.setServiceName("serviceName");
        metaDataDTO.setMethodName("methodName");
        metaDataDTO.setParameterTypes("parameterTypes");
        when(soulClientRegisterService.registerTars(any())).thenReturn("result");
        final String result = soulClientController.registerTarsRpc(metaDataDTO);
        assertEquals("result", result);
    }
}
