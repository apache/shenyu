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

package org.apache.shenyu.plugin.sofa.handler;

import org.apache.shenyu.common.config.SofaRegisterConfig;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.plugin.base.utils.Singleton;

import org.junit.Assert;
import org.junit.Before;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.MethodSorters;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * SofaPluginDataHandlerTest.
 */
@RunWith(MockitoJUnitRunner.class)
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public final class SofaPluginDataHandlerTest {
    private SofaPluginDataHandler sofaPluginDataHandler;

    private final String registryConfig = "{\"protocol\":\"zookeeper\",\"register\":\"127.0.0.1:2181\"}";

    @Before
    public void setUp() {
        sofaPluginDataHandler = new SofaPluginDataHandler();
        MetaData metaData = new MetaData();
        metaData.setId("1332017966661636096");
        metaData.setAppName("sofa");
        metaData.setPath("/sofa/findAll");
        metaData.setServiceName("org.apache.shenyu.test.dubbo.api.service.DubboTestService");
        metaData.setMethodName("findAll");
    }

    @Test
    public void testPluginEnable() {
        PluginData pluginData = new PluginData("", "", registryConfig, "1", true);
        sofaPluginDataHandler.handlerPlugin(pluginData);
        Assert.assertEquals(Singleton.INST.get(SofaRegisterConfig.class).getRegister(), "127.0.0.1:2181");
    }

    @Test
    public void testPluginDisable() {
        PluginData pluginData = new PluginData("", "", registryConfig, "1", false);
        sofaPluginDataHandler.handlerPlugin(pluginData);
        Assert.assertNull(Singleton.INST.get(SofaRegisterConfig.class));
    }
}
