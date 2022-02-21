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

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.convert.rule.impl.JwtRuleHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.jwt.config.JwtConfig;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Test case for {@link JwtPluginDataHandler}.
 */
public final class JwtPluginDataHandlerTest {

    private JwtPluginDataHandler jwtPluginDataHandlerUnderTest;

    private List<JwtRuleHandle.Convert> converts;

    private JwtRuleHandle.Convert convert;

    private JwtRuleHandle jwtRuleHandle;

    @BeforeEach
    public void setUp() {
        jwtPluginDataHandlerUnderTest = new JwtPluginDataHandler();
        converts = new ArrayList<>();
        convert = new JwtRuleHandle.Convert();
        jwtRuleHandle = new JwtRuleHandle();
    }

    @Test
    public void testHandlerPlugin() {
        final PluginData pluginData = new PluginData("pluginId", "pluginName", "{\"secretKey\":\"shenyu\"}", "0", false);
        jwtPluginDataHandlerUnderTest.handlerPlugin(pluginData);
        JwtConfig jwtConfig = Singleton.INST.get(JwtConfig.class);
        Map<String, String> map = GsonUtils.getInstance().toObjectMap(pluginData.getConfig(), String.class);
        assertEquals(jwtConfig.getSecretKey(), map.get("secretKey"));
    }

    @Test
    public void testHandlerRule() {
        RuleData ruleData = new RuleData();
        ruleData.setId("jwtRule");
        ruleData.setSelectorId("jwt");
        convert.setJwtVal("userId");
        convert.setHeaderVal("userId");
        converts.add(convert);
        jwtRuleHandle.setConverter(converts);

        ruleData.setHandle(GsonUtils.getGson().toJson(jwtRuleHandle));
        jwtPluginDataHandlerUnderTest.handlerRule(ruleData);
        assertEquals(jwtRuleHandle.getConverter().toString(), JwtPluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(ruleData)).getConverter().toString());
    }

    @Test
    public void testPluginNamed() {
        final String result = jwtPluginDataHandlerUnderTest.pluginNamed();
        assertEquals(PluginEnum.JWT.getName(), result);
    }
}
