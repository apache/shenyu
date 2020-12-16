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
 * Test case for {@link SpringMvcRegisterDTO}.
 *
 * @author Jiang Jining
 */
public final class SpringMvcRegisterDTOTest {
    
    private SpringMvcRegisterDTO springCloudRegisterDTO;
    
    @Before
    public void initSpringCloudRegisterDTO() {
        springCloudRegisterDTO = new SpringMvcRegisterDTO();
        springCloudRegisterDTO.setEnabled(false);
        springCloudRegisterDTO.setRegisterMetaData(false);
        springCloudRegisterDTO.setHost("127.0.0.1");
        springCloudRegisterDTO.setPort(8080);
        springCloudRegisterDTO.setPath("Test path");
        springCloudRegisterDTO.setRuleName("Test rule");
        springCloudRegisterDTO.setAppName("Test app name");
        springCloudRegisterDTO.setRpcType("Test RPC type");
        springCloudRegisterDTO.setPathDesc("Test path desc");
        springCloudRegisterDTO.setContext("Test context");
        springCloudRegisterDTO.setRegisterMetaData(false);
    }
    
    @Test
    public void testSpringCloudRegisterDTO() {
        Assertions.assertNotNull(springCloudRegisterDTO);
        Assertions.assertFalse(springCloudRegisterDTO.isEnabled());
        Assertions.assertEquals(springCloudRegisterDTO.getHost(), "127.0.0.1");
        Assertions.assertEquals(springCloudRegisterDTO.getPort(), 8080);
        Assertions.assertEquals(springCloudRegisterDTO.getPath(), "Test path");
        Assertions.assertEquals(springCloudRegisterDTO.getRuleName(), "Test rule");
        Assertions.assertEquals(springCloudRegisterDTO.getAppName(), "Test app name");
        Assertions.assertEquals(springCloudRegisterDTO.getRpcType(), "Test RPC type");
        Assertions.assertEquals(springCloudRegisterDTO.getPathDesc(), "Test path desc");
        Assertions.assertEquals(springCloudRegisterDTO.getContext(), "Test context");
        Assertions.assertFalse(springCloudRegisterDTO.isRegisterMetaData());
        springCloudRegisterDTO.setEnabled(true);
        springCloudRegisterDTO.setRegisterMetaData(true);
        Assertions.assertTrue(springCloudRegisterDTO.isEnabled());
        Assertions.assertTrue(springCloudRegisterDTO.isRegisterMetaData());
    }
}