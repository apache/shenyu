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

import com.google.common.collect.Lists;
import org.junit.Before;
import org.junit.Test;
import org.junit.jupiter.api.Assertions;

import java.util.List;

/**
 * Test case for  {@link AuthApplyDTO}.
 *
 * @author Jiang Jining
 */
public final class AuthApplyDTOTest {
    
    private AuthApplyDTO authApplyDTO;
    
    @Before
    public void initAuthApplyDTO() {
        authApplyDTO = new AuthApplyDTO();
        authApplyDTO.setAppKey("Soul-application-key");
        authApplyDTO.setAppName("Soul-application-app-name");
        authApplyDTO.setAppParam("Soul-application-param");
        authApplyDTO.setExtInfo("");
        authApplyDTO.setPathList(Lists.newArrayList());
        authApplyDTO.setPhone("18212345678");
        authApplyDTO.setUserId(null);
    }
    
    @Test
    public void testAuthApplyDTO() {
        Assertions.assertNotNull(authApplyDTO);
        Assertions.assertNull(authApplyDTO.getUserId());
        Assertions.assertEquals(authApplyDTO.getAppKey(), "Soul-application-key");
        Assertions.assertEquals(authApplyDTO.getAppName(), "Soul-application-app-name");
        Assertions.assertEquals(authApplyDTO.getAppParam(), "Soul-application-param");
        Assertions.assertEquals(authApplyDTO.getExtInfo(), "");
        Assertions.assertEquals(authApplyDTO.getPhone(), "18212345678");
        List<String> pathList = authApplyDTO.getPathList();
        Assertions.assertNotNull(pathList);
        Assertions.assertEquals(pathList.size(), 0);
    }
}