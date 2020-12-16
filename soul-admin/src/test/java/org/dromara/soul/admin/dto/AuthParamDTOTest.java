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

import org.junit.Before;
import org.junit.Test;
import org.junit.jupiter.api.Assertions;

/**
 * Test case for {@link AuthParamDTO}.
 *
 * @author Jiang Jining
 */
public final class AuthParamDTOTest {
    
    private AuthParamDTO authParamDTO;
    
    @Before
    public void initAuthParamDTO() {
        authParamDTO = new AuthParamDTO();
        authParamDTO.setAppName("Soul-test");
        authParamDTO.setAppParam("Soul-test-param");
    }
    
    @Test
    public void testAuthParamDTO() {
        Assertions.assertNotNull(authParamDTO);
        Assertions.assertEquals(authParamDTO.getAppName(), "Soul-test");
        Assertions.assertEquals(authParamDTO.getAppParam(), "Soul-test-param");
        authParamDTO.setAppName("Soul-test-new");
        authParamDTO.setAppParam("New param");
        Assertions.assertEquals(authParamDTO.getAppName(), "Soul-test-new");
        Assertions.assertEquals(authParamDTO.getAppParam(), "New param");
    }
}