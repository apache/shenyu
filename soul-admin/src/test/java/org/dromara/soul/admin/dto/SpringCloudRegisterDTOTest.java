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
 * Test case for {@link SpringCloudRegisterDTO}.
 *
 * @author Jiang Jining
 */
public final class SpringCloudRegisterDTOTest {
    
    private SpringCloudRegisterDTO springCloudRegisterDTO;
    
    @Before
    public void initSpringCloudRegisterDTO() {
        springCloudRegisterDTO = new SpringCloudRegisterDTO();
        springCloudRegisterDTO.setAppName("Spring-cloud-test");
        springCloudRegisterDTO.setEnabled(false);
        springCloudRegisterDTO.setContext("Spring-cloud-test-context");
        springCloudRegisterDTO.setPathDesc("Test path desc");
        springCloudRegisterDTO.setRpcType("Test RPC");
        springCloudRegisterDTO.setRuleName("Test rule name");
        springCloudRegisterDTO.setPath("Test path");
    }
    
    @Test
    public void testSpringCloudRegisterDTO() {
        Assertions.assertNotNull(springCloudRegisterDTO);
        Assertions.assertEquals(springCloudRegisterDTO.getAppName(), "Spring-cloud-test");
        Assertions.assertFalse(springCloudRegisterDTO.isEnabled());
        Assertions.assertEquals(springCloudRegisterDTO.getContext(), "Spring-cloud-test-context");
        Assertions.assertEquals(springCloudRegisterDTO.getPath(), "Test path");
        Assertions.assertEquals(springCloudRegisterDTO.getPathDesc(), "Test path desc");
        Assertions.assertEquals(springCloudRegisterDTO.getRpcType(), "Test RPC");
        Assertions.assertEquals(springCloudRegisterDTO.getRuleName(), "Test rule name");
        springCloudRegisterDTO.setEnabled(true);
        Assertions.assertTrue(springCloudRegisterDTO.isEnabled());
    }
}
