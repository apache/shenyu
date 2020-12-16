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
import org.springframework.beans.BeanUtils;

/**
 * Test case for {@link MetaDataDTO}.
 *
 * @author Jiang Jining
 */
public final class MetaDataDTOTest {
    
    private MetaDataDTO metaDataDTO;
    
    private MetaDataDTO metaDataDTOCopy;
    
    @Before
    public void initMetaDataDTO() {
        metaDataDTO = new MetaDataDTO();
        metaDataDTOCopy = new MetaDataDTO();
        metaDataDTO.setAppName("Test-app");
        metaDataDTO.setEnabled(true);
        metaDataDTO.setPath("/test/path");
        metaDataDTO.setId("1335611671700856832");
        metaDataDTO.setMethodName("Test-method");
        metaDataDTO.setContextPath("Test-context-path");
        metaDataDTO.setPathDesc("Test path desc.");
        metaDataDTO.setRpcExt("RPC Test.");
        metaDataDTO.setRpcType("Test type.");
        metaDataDTO.setParameterTypes("Test parameter types.");
        metaDataDTO.setRuleName("Test Rule Name.");
        metaDataDTO.setServiceName("Service-name");
        BeanUtils.copyProperties(metaDataDTO, metaDataDTOCopy);
    }
    
    @Test
    public void testMetaDataDTO() {
        Assertions.assertNotNull(metaDataDTO);
        Assertions.assertEquals(metaDataDTO.getAppName(), "Test-app");
        Assertions.assertEquals(metaDataDTO.getId(), "1335611671700856832");
        Assertions.assertEquals(metaDataDTO.getPath(), "/test/path");
        Assertions.assertEquals(metaDataDTO.getContextPath(), "Test-context-path");
        Assertions.assertEquals(metaDataDTO.getServiceName(), "Service-name");
        Assertions.assertEquals(metaDataDTO.getMethodName(), "Test-method");
        Assertions.assertTrue(metaDataDTO.getEnabled());
        Assertions.assertEquals(metaDataDTO.getRuleName(), "Test Rule Name.");
        Assertions.assertEquals(metaDataDTO.getPathDesc(), "Test path desc.");
        Assertions.assertEquals(metaDataDTO.getRpcType(), "Test type.");
        Assertions.assertEquals(metaDataDTO.getRpcExt(), "RPC Test.");
        Assertions.assertEquals(metaDataDTO.getParameterTypes(), "Test parameter types.");
        Assertions.assertEquals(metaDataDTO, metaDataDTOCopy);
    }
}