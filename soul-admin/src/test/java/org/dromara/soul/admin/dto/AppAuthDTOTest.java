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

package org.dromara.soul.admin.dto;

import org.assertj.core.util.Lists;
import org.junit.Before;
import org.junit.Test;
import org.junit.jupiter.api.Assertions;

import java.util.List;

/**
 * Test case for {@link AppAuthDTO}.
 *
 * @author Jiang Jining
 */
public final class AppAuthDTOTest {
    
    private AppAuthDTO appAuthDTO;
    
    @Before
    public void initAppAuth() {
        appAuthDTO = new AppAuthDTO();
        AuthParamDTO authParamFirst = new AuthParamDTO();
        authParamFirst.setAppName("Soul-test-first");
        authParamFirst.setAppParam("Auth-param-first");
        AuthParamDTO authParamSecond = new AuthParamDTO();
        authParamSecond.setAppName("Soul-test-second");
        authParamSecond.setAppParam("Auth-param-second");
        appAuthDTO.setAppKey("0f1d3b5c81ff48d9b0912e51822b4f66");
        appAuthDTO.setAppSecret("2095132720951327");
        appAuthDTO.setEnabled(true);
        appAuthDTO.setPhone("18965432211");
        appAuthDTO.setId("2dc2cd0f5e1d4ef4ad8e6d3c02c5446b");
        appAuthDTO.setUserId("9ea41b26e35446e587e96bb4089bd10b");
        appAuthDTO.setExtInfo("Extra info test data.");
        appAuthDTO.setAuthParamDTOList(Lists.newArrayList(authParamFirst, authParamSecond));
    }
    
    @Test
    public void testAppAuthDTO() {
        Assertions.assertNotNull(appAuthDTO);
        List<AuthParamDTO> authParamDTOList = appAuthDTO.getAuthParamDTOList();
        Assertions.assertNotNull(authParamDTOList);
        Assertions.assertEquals(appAuthDTO.getPhone(), "18965432211");
        Assertions.assertEquals(authParamDTOList.size(), 2);
        Assertions.assertEquals(appAuthDTO.getAppKey(), "0f1d3b5c81ff48d9b0912e51822b4f66");
        Assertions.assertEquals(appAuthDTO.getAppSecret(), "2095132720951327");
        Assertions.assertTrue(appAuthDTO.getEnabled());
        Assertions.assertEquals(appAuthDTO.getExtInfo(), "Extra info test data.");
        Assertions.assertEquals(appAuthDTO.getId(), "2dc2cd0f5e1d4ef4ad8e6d3c02c5446b");
        Assertions.assertEquals(appAuthDTO.getUserId(), "9ea41b26e35446e587e96bb4089bd10b");
    }
}
