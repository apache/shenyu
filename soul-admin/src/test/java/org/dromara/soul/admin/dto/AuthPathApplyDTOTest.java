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
 * Test case for {@link AuthPathApplyDTO}.
 *
 * @author Jiang Jining
 */
public final class AuthPathApplyDTOTest {
    
    private AuthPathApplyDTO authPathApplyDTO;
    
    @Before
    public void initAuthPathApplyDTO() {
        authPathApplyDTO = new AuthPathApplyDTO();
        authPathApplyDTO.setPath("/appPath");
        authPathApplyDTO.setAppName("Soul-test");
    }
    
    @Test
    public void testAuthPathApplyDTO() {
        Assertions.assertNotNull(authPathApplyDTO);
        Assertions.assertEquals(authPathApplyDTO.getPath(), "/appPath");
        Assertions.assertEquals(authPathApplyDTO.getAppName(), "Soul-test");
        authPathApplyDTO.setPath("/test/path");
        authPathApplyDTO.setAppName("test-app");
        Assertions.assertEquals(authPathApplyDTO.getPath(), "/test/path");
        Assertions.assertEquals(authPathApplyDTO.getAppName(), "test-app");
    }
}
