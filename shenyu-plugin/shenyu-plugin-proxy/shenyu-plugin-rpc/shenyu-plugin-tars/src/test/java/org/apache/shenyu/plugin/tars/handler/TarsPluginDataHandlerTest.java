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

package org.apache.shenyu.plugin.tars.handler;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.convert.plugin.TarsRegisterConfig;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.Singleton;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test case for {@link TarsPluginDataHandler}.
 */
@ExtendWith(MockitoExtension.class)
public final class TarsPluginDataHandlerTest {
    
    private TarsPluginDataHandler tarsPluginDataHandlerUnderTest;
    
    @BeforeEach
    public void setUp() {
        tarsPluginDataHandlerUnderTest = new TarsPluginDataHandler();
    }
    
    @Test
    public void testHandlerPlugin() {
        final PluginData pluginData = new PluginData("id", "name", "{\"threadpool\":\"cached\",\"corethreads\":1,\"threads\":2,\"queues\":3}", "0", true, null);
        tarsPluginDataHandlerUnderTest.handlerPlugin(pluginData);
        assertTrue(pluginData.getName().endsWith("name"));
        TarsRegisterConfig config = Singleton.INST.get(TarsRegisterConfig.class);
        Assertions.assertEquals(config.getThreadpool(), "cached");
        Assertions.assertEquals(config.getCorethreads(), 1);
        Assertions.assertEquals(config.getThreads(), 2);
        Assertions.assertEquals(config.getQueues(), 3);
    }
    
    @Test
    public void testPluginNamed() {
        final String result = tarsPluginDataHandlerUnderTest.pluginNamed();
        assertEquals(PluginEnum.TARS.getName(), result);
    }
}
