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
 * Test case for {@link AuthPathDTO}.
 *
 * @author Jiang Jining
 */
public final class AuthPathDTOTest {
    
    private AuthPathDTO authPathDTO;
    
    @Before
    public void initAuthPathDTO() {
        authPathDTO = new AuthPathDTO();
        authPathDTO.setAppName("test-api");
        authPathDTO.setPath("/api/test");
        authPathDTO.setEnabled(false);
    }
    
    @Test
    public void testAuthPathDTO() {
        Assertions.assertNotNull(authPathDTO);
        Assertions.assertEquals(authPathDTO.getAppName(), "test-api");
        Assertions.assertEquals(authPathDTO.getPath(), "/api/test");
        Assertions.assertFalse(authPathDTO.getEnabled());
        authPathDTO.setEnabled(true);
        authPathDTO.setAppName("new-api");
        authPathDTO.setPath("/api/test/new");
        Assertions.assertTrue(authPathDTO.getEnabled());
        Assertions.assertEquals(authPathDTO.getAppName(), "new-api");
        Assertions.assertEquals(authPathDTO.getPath(), "/api/test/new");
    }
}
