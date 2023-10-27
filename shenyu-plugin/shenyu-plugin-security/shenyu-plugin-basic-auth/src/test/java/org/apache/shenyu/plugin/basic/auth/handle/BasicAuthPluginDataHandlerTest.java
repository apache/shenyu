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

package org.apache.shenyu.plugin.basic.auth.handle;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.basic.auth.config.BasicAuthConfig;
import org.apache.shenyu.plugin.basic.auth.rule.DefaultBasicAuthRuleHandle;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Test case for {@link BasicAuthPluginDataHandler}.
 */
public final class BasicAuthPluginDataHandlerTest {

    private BasicAuthPluginDataHandler basicAuthPluginDataHandler;

    @BeforeEach
    public void setUp() {
        basicAuthPluginDataHandler = new BasicAuthPluginDataHandler();
    }

    @Test
    public void testHandlerPlugin() {
        final PluginData pluginData = new PluginData("pluginId", "pluginName", "{\"defaultHandleJson\":\"test:test123\"}", "0", false, null);
        basicAuthPluginDataHandler.handlerPlugin(pluginData);
        BasicAuthConfig basicAuthConfig = Singleton.INST.get(BasicAuthConfig.class);
        Map<String, String> map = GsonUtils.getInstance().toObjectMap(pluginData.getConfig(), String.class);
        assertEquals(basicAuthConfig.getDefaultHandleJson(), map.get("defaultHandleJson"));
    }

    @Test
    public void testHandlerRule() {
        RuleData ruleData = new RuleData();
        ruleData.setId("basicAuthRule");
        ruleData.setSelectorId("basicAuth");
        String handleJson = "{\"authorization\":\"test:test123\"}";
        ruleData.setHandle(handleJson);
        basicAuthPluginDataHandler.handlerPlugin(new PluginData("pluginId", "pluginName", "{\"defaultHandleJson\":\"test:test123\"}", "0", false, null));
        basicAuthPluginDataHandler.handlerRule(ruleData);
        Map<String, String> map = GsonUtils.getInstance().toObjectMap(handleJson, String.class);
        assertEquals(map.get("authorization"), ((DefaultBasicAuthRuleHandle) BasicAuthPluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(ruleData))).getAuthorization());
    }

    @Test
    public void testPluginNamed() {
        final String result = basicAuthPluginDataHandler.pluginNamed();
        assertEquals(PluginEnum.BASIC_AUTH.getName(), result);
    }
}
