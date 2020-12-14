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

package org.dromara.soul.plugin.apache.dubbo.handler;

import org.dromara.soul.common.config.DubboRegisterConfig;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.plugin.base.utils.Singleton;
import org.junit.Assert;
import org.junit.Before;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.MethodSorters;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * Test cases for ApacheDubboPluginDataHandler.
 *
 * @author kennhua
 */
@RunWith(MockitoJUnitRunner.class)
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public final class ApacheDubboPluginDataHandlerTest {

    private ApacheDubboPluginDataHandler apacheDubboPluginDataHandler;

    private final String registryConfig = "{\"protocol\":\"zookeeper\",\"register\":\"127.0.0.1:2181\"}";

    @Before
    public void setUp() {
        apacheDubboPluginDataHandler = new ApacheDubboPluginDataHandler();
    }

    @Test
    public void testPluginEnable() {
        PluginData pluginData = new PluginData("", "", registryConfig, 1, true);
        apacheDubboPluginDataHandler.handlerPlugin(pluginData);
        Assert.assertEquals(Singleton.INST.get(DubboRegisterConfig.class).getRegister(), "127.0.0.1:2181");
    }

    @Test
    public void testPluginDisable() {
        PluginData pluginData = new PluginData("", "", registryConfig, 1, false);
        apacheDubboPluginDataHandler.handlerPlugin(pluginData);
        Assert.assertNull(Singleton.INST.get(DubboRegisterConfig.class));
    }
}
