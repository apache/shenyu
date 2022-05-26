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

package org.apache.shenyu.plugin.motan.handler;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.convert.plugin.MotanRegisterConfig;
import org.apache.shenyu.common.utils.Singleton;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * The Test Case For MotanPluginDataHandler.
 */
public final class MotanPluginDataHandlerTest {

    private MotanPluginDataHandler motanPluginDataHandler;

    private PluginData pluginData;

    @BeforeEach
    public void setUp() {
        this.motanPluginDataHandler = new MotanPluginDataHandler();
        this.pluginData = new PluginData();
    }

    @Test
    public void testHandlerPlugin() {
        pluginData.setEnabled(true);
        pluginData.setConfig("{\"register\" : \"127.0.0.1:2181\"}");
        motanPluginDataHandler.handlerPlugin(pluginData);
        Assertions.assertEquals(Singleton.INST.get(MotanRegisterConfig.class).getRegister(), "127.0.0.1:2181");
    }

    @Test
    public void testPluginNamed() {
        Assertions.assertEquals(motanPluginDataHandler.pluginNamed(), "motan");
    }
}
