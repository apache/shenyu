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

package org.apache.shenyu.plugin.brpc.handler;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test case for {@link BrpcPluginDataHandler}.
 */
public final class BrpcPluginDataHandlerTest {
    
    private BrpcPluginDataHandler brpcPluginDataHandlerUnderTest;
    
    @BeforeEach
    public void setUp() {
        brpcPluginDataHandlerUnderTest = new BrpcPluginDataHandler();
    }
    
    @Test
    public void testHandlerPlugin() {
        final PluginData pluginData = new PluginData("id", "name", "config", "0", false, null);
        brpcPluginDataHandlerUnderTest.handlerPlugin(pluginData);
        assertTrue(pluginData.getName().endsWith("name"));
    }
    
    @Test
    public void testPluginNamed() {
        final String result = brpcPluginDataHandlerUnderTest.pluginNamed();
        assertEquals(PluginEnum.BRPC.getName(), result);
    }
}
