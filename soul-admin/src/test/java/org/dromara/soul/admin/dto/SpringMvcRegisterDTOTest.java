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
    
    private SpringMvcRegisterDTO springMvcRegisterDTO;
    
    @Before
    public void initSpringCloudRegisterDTO() {
        springMvcRegisterDTO = new SpringMvcRegisterDTO();
        springMvcRegisterDTO.setEnabled(false);
        springMvcRegisterDTO.setRegisterMetaData(false);
        springMvcRegisterDTO.setHost("127.0.0.1");
        springMvcRegisterDTO.setPort(8080);
        springMvcRegisterDTO.setPath("Test path");
        springMvcRegisterDTO.setRuleName("Test rule");
        springMvcRegisterDTO.setAppName("Test app name");
        springMvcRegisterDTO.setRpcType("Test RPC type");
        springMvcRegisterDTO.setPathDesc("Test path desc");
        springMvcRegisterDTO.setContext("Test context");
        springMvcRegisterDTO.setRegisterMetaData(false);
    }
    
    @Test
    public void testSpringCloudRegisterDTO() {
        Assertions.assertNotNull(springMvcRegisterDTO);
        Assertions.assertFalse(springMvcRegisterDTO.isEnabled());
        Assertions.assertEquals(springMvcRegisterDTO.getHost(), "127.0.0.1");
        Assertions.assertEquals(springMvcRegisterDTO.getPort(), 8080);
        Assertions.assertEquals(springMvcRegisterDTO.getPath(), "Test path");
        Assertions.assertEquals(springMvcRegisterDTO.getRuleName(), "Test rule");
        Assertions.assertEquals(springMvcRegisterDTO.getAppName(), "Test app name");
        Assertions.assertEquals(springMvcRegisterDTO.getRpcType(), "Test RPC type");
        Assertions.assertEquals(springMvcRegisterDTO.getPathDesc(), "Test path desc");
        Assertions.assertEquals(springMvcRegisterDTO.getContext(), "Test context");
        Assertions.assertFalse(springMvcRegisterDTO.isRegisterMetaData());
        springMvcRegisterDTO.setEnabled(true);
        springMvcRegisterDTO.setRegisterMetaData(true);
        Assertions.assertTrue(springMvcRegisterDTO.isEnabled());
        Assertions.assertTrue(springMvcRegisterDTO.isRegisterMetaData());
    }
}
