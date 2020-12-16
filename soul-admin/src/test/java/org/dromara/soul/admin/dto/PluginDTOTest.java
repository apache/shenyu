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
 * Test case for {@link PluginDTO}.
 *
 * @author Jiang Jining
 */
public final class PluginDTOTest {
    
    private PluginDTO pluginDTO;
    
    @Before
    public void initPluginDTO() {
        pluginDTO = new PluginDTO();
        pluginDTO.setEnabled(false);
        pluginDTO.setId("1335612349480407040");
        pluginDTO.setName("Plugin-test");
        pluginDTO.setRole(1);
        pluginDTO.setConfig("Config-test");
    }
    
    @Test
    public void testPluginDTO() {
        Assertions.assertNotNull(pluginDTO);
        Assertions.assertEquals(pluginDTO.getId(), "1335612349480407040");
        Assertions.assertFalse(pluginDTO.getEnabled());
        Assertions.assertEquals(pluginDTO.getName(), "Plugin-test");
        Assertions.assertEquals(pluginDTO.getRole(), 1);
        Assertions.assertEquals(pluginDTO.getConfig(), "Config-test");
        pluginDTO.setEnabled(true);
        Assertions.assertTrue(pluginDTO.getEnabled());
    }
}
