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

package org.apache.shenyu.plugin.apache.dubbo.handler;

import org.apache.shenyu.common.dto.convert.plugin.DubboRegisterConfig;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.junit.Before;
import org.junit.Test;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;

/**
 * The Test Case For ApacheDubboPluginDataHandler.
 */
public final class ApacheDubboPluginDataHandlerTest {

    private ApacheDubboPluginDataHandler apacheDubboPluginDataHandler;

    @Before
    public void setUp() {
        apacheDubboPluginDataHandler = new ApacheDubboPluginDataHandler();
    }

    @Test
    public void handlerPlugin() {
        DubboRegisterConfig dubboRegisterConfig = mock(DubboRegisterConfig.class);
        PluginData pluginData = PluginData.builder()
                .enabled(true)
                .config(GsonUtils.getInstance().toJson(dubboRegisterConfig))
                .build();
        apacheDubboPluginDataHandler.handlerPlugin(pluginData);
        pluginData.setConfig(null);
        apacheDubboPluginDataHandler.handlerPlugin(pluginData);
    }

    @Test
    public void pluginNamed() {
        assertThat(apacheDubboPluginDataHandler.pluginNamed(), is(PluginEnum.DUBBO.getName()));
    }
}
