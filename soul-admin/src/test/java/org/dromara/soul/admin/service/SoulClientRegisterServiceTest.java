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

package org.dromara.soul.admin.service;

import org.dromara.soul.admin.dto.MetaDataDTO;
import org.dromara.soul.admin.dto.SpringCloudRegisterDTO;
import org.dromara.soul.admin.dto.SpringMvcRegisterDTO;
import org.dromara.soul.admin.service.impl.SoulClientRegisterServiceImpl;
import org.dromara.soul.admin.utils.SoulResultMessage;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;

/**
 * Test cases for SoulClientRegisterService.
 *
 * @author sakiila
 */
@RunWith(MockitoJUnitRunner.class)
public final class SoulClientRegisterServiceTest {

    @Mock
    private SoulClientRegisterServiceImpl soulClientRegisterService;

    @Test
    public void testRegisterSpringMvc() {
        SpringMvcRegisterDTO dto = buildSpringMvcRegisterDTO();
        given(soulClientRegisterService.registerSpringMvc(any())).willReturn(SoulResultMessage.SUCCESS);
        assertEquals(SoulResultMessage.SUCCESS, soulClientRegisterService.registerSpringMvc(dto));
    }

    @Test
    public void testRegisterSpringCloud() {
        SpringCloudRegisterDTO dto = buildCloudRegisterDTO();
        given(soulClientRegisterService.registerSpringCloud(any())).willReturn(SoulResultMessage.SUCCESS);
        assertEquals(SoulResultMessage.SUCCESS, soulClientRegisterService.registerSpringCloud(dto));
    }

    @Test
    public void testRegisterDubbo() {
        MetaDataDTO dto = buildMetaDataD();
        given(soulClientRegisterService.registerDubbo(any())).willReturn(SoulResultMessage.SUCCESS);
        assertEquals(SoulResultMessage.SUCCESS, soulClientRegisterService.registerDubbo(dto));
    }

    @Test
    public void testRegisterSofa() {
        MetaDataDTO dto = buildMetaDataD();
        given(soulClientRegisterService.registerSofa(any())).willReturn(SoulResultMessage.SUCCESS);
        assertEquals(SoulResultMessage.SUCCESS, soulClientRegisterService.registerSofa(dto));
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
        springMvcRegisterDTO.setRegisterMetaData(false);
        return springMvcRegisterDTO;
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

    private MetaDataDTO buildMetaDataD() {
        MetaDataDTO metaDataDTO = new MetaDataDTO();
        metaDataDTO.setId("6");
        metaDataDTO.setAppName("appName3");
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
}
