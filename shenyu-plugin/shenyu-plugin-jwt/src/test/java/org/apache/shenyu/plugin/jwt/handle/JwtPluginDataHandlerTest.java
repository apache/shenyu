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

package org.apache.shenyu.plugin.jwt.handle;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.base.utils.Singleton;
import org.apache.shenyu.plugin.jwt.config.JwtConfig;
import org.junit.Before;
import org.junit.Test;

import java.util.Map;

import static org.junit.Assert.assertEquals;

/**
 * Test case for {@link JwtPluginDataHandler}.
 */
public final class JwtPluginDataHandlerTest {

    private JwtPluginDataHandler jwtPluginDataHandlerUnderTest;

    @Before
    public void setUp() {
        jwtPluginDataHandlerUnderTest = new JwtPluginDataHandler();
    }

    @Test
    public void testHandlerPlugin() {
        final PluginData pluginData = new PluginData("pluginId", "pluginName", "{\"secretKey\":\"sinsy\",\"filterPath\":\"/cloud/ecg/common,/cloud/ecg/selectAll\"}", "0", false);
        jwtPluginDataHandlerUnderTest.handlerPlugin(pluginData);
        JwtConfig jwtConfig = Singleton.INST.get(JwtConfig.class);
        Map<String, String> map = GsonUtils.getInstance().toObjectMap(pluginData.getConfig(), String.class);
        assertEquals(jwtConfig.getSecretKey(), map.get("secretKey"));
        assertEquals(StringUtils.join(jwtConfig.getFilterPath(), ","), map.get("filterPath"));
    }

    @Test
    public void testPluginNamed() {
        final String result = jwtPluginDataHandlerUnderTest.pluginNamed();
        assertEquals(PluginEnum.JWT.getName(), result);
    }
}
