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
 * Test case for {@link PluginHandleDTO}.
 *
 * @author Jiang Jining
 */
public final class PluginHandleDTOTest {
    
    private PluginHandleDTO pluginHandleDTO;
    
    @Before
    public void initPluginHandlerDTO() {
        pluginHandleDTO = new PluginHandleDTO();
        pluginHandleDTO.setId("8837278239239937721");
        pluginHandleDTO.setSort(1);
        pluginHandleDTO.setDataType(2);
        pluginHandleDTO.setPluginId("138273826611521");
        pluginHandleDTO.setLabel("Test label");
        pluginHandleDTO.setType(3);
        pluginHandleDTO.setExtObj("External Object test");
        pluginHandleDTO.setField("Test field");
    }
    
    @Test
    public void testPluginHandleDTO() {
        Assertions.assertNotNull(pluginHandleDTO);
        Assertions.assertEquals(pluginHandleDTO.getId(), "8837278239239937721");
        Assertions.assertEquals(pluginHandleDTO.getDataType(), 2);
        Assertions.assertEquals(pluginHandleDTO.getPluginId(), "138273826611521");
        Assertions.assertEquals(pluginHandleDTO.getLabel(), "Test label");
        Assertions.assertEquals(pluginHandleDTO.getType(), 3);
        Assertions.assertEquals(pluginHandleDTO.getSort(), 1);
        Assertions.assertEquals(pluginHandleDTO.getExtObj(), "External Object test");
        Assertions.assertEquals(pluginHandleDTO.getField(), "Test field");
    }
}