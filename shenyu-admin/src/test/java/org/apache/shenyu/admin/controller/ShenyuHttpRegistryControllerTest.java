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

import org.apache.shenyu.admin.disruptor.RegisterServerDisruptorPublisher;
import org.apache.shenyu.admin.service.register.ShenyuClientRegisterDubboServiceImpl;
import org.apache.shenyu.admin.service.register.ShenyuClientRegisterServiceFactory;
import org.apache.shenyu.admin.service.register.ShenyuClientRegisterSofaServiceImpl;
import org.apache.shenyu.admin.service.register.ShenyuClientRegisterSpringCloudServiceImpl;
import org.apache.shenyu.admin.service.register.ShenyuClientRegisterSpringMVCServiceImpl;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
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

import java.util.Map;

import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Test cases for ShenyuHttpRegistryController.
 */
@RunWith(MockitoJUnitRunner.Silent.class)
public final class ShenyuHttpRegistryControllerTest {

    private MockMvc mockMvc;

    @InjectMocks
    private ShenyuHttpRegistryController shenyuHttpRegistryController;

    @Mock
    private ShenyuClientRegisterDubboServiceImpl shenyuClientRegisterDubboService;

    @Mock
    private ShenyuClientRegisterSofaServiceImpl shenyuClientRegisterSofaService;

    @Mock
    private ShenyuClientRegisterSpringMVCServiceImpl shenyuClientRegisterSpringMVCService;

    @Mock
    private ShenyuClientRegisterSpringCloudServiceImpl shenyuClientRegisterSpringCloudService;

    @Mock
    private Map<String, ShenyuClientRegisterServiceFactory> shenyuClientRegisterService;

    @Before
    public void setUp() {
        this.mockMvc = MockMvcBuilders.standaloneSetup(shenyuHttpRegistryController).build();
        RegisterServerDisruptorPublisher publisher = RegisterServerDisruptorPublisher.getInstance();
        publisher.start(shenyuClientRegisterService);
        shenyuHttpRegistryController.init(RegisterServerDisruptorPublisher.getInstance(), null);
    }

    @Test
    public void testRegisterSpringMvc() throws Exception {
        final MetaDataRegisterDTO springMvcRegisterDTO = buildSpringMvcRegisterDTO();
        given(shenyuClientRegisterSpringMVCService.register(springMvcRegisterDTO)).willReturn("success");
        this.mockMvc.perform(MockMvcRequestBuilders.post("/shenyu-client/springmvc-register")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(springMvcRegisterDTO)))
                .andExpect(status().isOk())
                .andReturn();
    }

    private MetaDataRegisterDTO buildSpringMvcRegisterDTO() {
        MetaDataRegisterDTO springMvcRegisterDTO = new MetaDataRegisterDTO();
        springMvcRegisterDTO.setAppName("appName1");
        springMvcRegisterDTO.setContextPath("content1");
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
        final MetaDataRegisterDTO springCloudRegisterDTO = buildCloudRegisterDTO();
        given(this.shenyuClientRegisterSpringCloudService.register(springCloudRegisterDTO)).willReturn("success");
        this.mockMvc.perform(MockMvcRequestBuilders.post("/shenyu-client/springcloud-register")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(springCloudRegisterDTO)))
                .andExpect(status().isOk())
                .andReturn();
    }

    private MetaDataRegisterDTO buildCloudRegisterDTO() {
        MetaDataRegisterDTO springCloudRegisterDTO = new MetaDataRegisterDTO();
        springCloudRegisterDTO.setAppName("appName2");
        springCloudRegisterDTO.setContextPath("content2");
        springCloudRegisterDTO.setPath("path2");
        springCloudRegisterDTO.setPathDesc("pathDesc2");
        springCloudRegisterDTO.setRpcType("rpcType2");
        springCloudRegisterDTO.setRuleName("ruleName2");
        springCloudRegisterDTO.setEnabled(false);
        return springCloudRegisterDTO;
    }

    @Test
    public void testRegisterRpc() throws Exception {
        final MetaDataRegisterDTO metaDataDTO = buildMetaDataDTO("app_dubbo");
        given(this.shenyuClientRegisterDubboService.register(metaDataDTO)).willReturn("success");
        this.mockMvc.perform(MockMvcRequestBuilders.post("/shenyu-client/dubbo-register")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(metaDataDTO)))
                .andExpect(status().isOk())
                .andReturn();
    }

    private MetaDataRegisterDTO buildMetaDataDTO(final String appName) {
        MetaDataRegisterDTO metaDataDTO = new MetaDataRegisterDTO();
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
        final MetaDataRegisterDTO metaDataDTO = buildMetaDataDTO("app_sofa");
        given(this.shenyuClientRegisterSofaService.register(metaDataDTO)).willReturn("success");
        this.mockMvc.perform(MockMvcRequestBuilders.post("/shenyu-client/sofa-register")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(metaDataDTO)))
                .andExpect(status().isOk())
                .andReturn();
    }
}
