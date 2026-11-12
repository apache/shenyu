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
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.jwt.config.JwtConfig;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Test case for {@link JwtPluginDataHandler}.
 */
public final class JwtPluginDataHandlerTest {

    private JwtPluginDataHandler jwtPluginDataHandlerUnderTest;

    @BeforeEach
    public void setUp() {
        jwtPluginDataHandlerUnderTest = new JwtPluginDataHandler();
    }

    @Test
    public void testHandlerPlugin() {
        final PluginData pluginData = new PluginData("pluginId", "pluginName", "{\"secretKey\":\"shenyu\"}", "0", false, null);
        jwtPluginDataHandlerUnderTest.handlerPlugin(pluginData);
        JwtConfig jwtConfig = Singleton.INST.get(JwtConfig.class);
        Map<String, String> map = GsonUtils.getInstance().toObjectMap(pluginData.getConfig(), String.class);
        assertEquals(jwtConfig.getSecretKey(), map.get("secretKey"));
    }

    @Test
    public void testResourceMetadataConfigurationCanBeCleared() {
        PluginData pluginData = new PluginData("pluginId", "pluginName",
                "{\"secretKey\":\"shenyu\",\"resourceMetadataUrl\":\"https://gateway.example/metadata\"}", "0", false, null);
        jwtPluginDataHandlerUnderTest.handlerPlugin(pluginData);
        assertEquals("https://gateway.example/metadata", Singleton.INST.get(JwtConfig.class).getResourceMetadataUrl());
        pluginData.setConfig("{\"secretKey\":\"shenyu\",\"resourceMetadataUrl\":\"\"}");
        jwtPluginDataHandlerUnderTest.handlerPlugin(pluginData);
        assertNull(Singleton.INST.get(JwtConfig.class).getResourceMetadataUrl());
        pluginData.setConfig("{\"secretKey\":\"shenyu\"}");
        jwtPluginDataHandlerUnderTest.handlerPlugin(pluginData);
        assertNull(Singleton.INST.get(JwtConfig.class).getResourceMetadataUrl());
    }

    @ParameterizedTest
    @ValueSource(strings = {"/metadata", "file:///metadata", "https://user:password@gateway.example/metadata",
            "https://gateway.example/metadata#fragment", "https://gateway.example/\"injected", "https://gateway.example/\r\nInjected: value"})
    public void testInvalidMetadataDoesNotReplaceActiveConfiguration(final String metadataUrl) {
        PluginData pluginData = new PluginData("pluginId", "pluginName", "{\"secretKey\":\"shenyu\"}", "0", false, null);
        jwtPluginDataHandlerUnderTest.handlerPlugin(pluginData);
        JwtConfig previous = Singleton.INST.get(JwtConfig.class);
        pluginData.setConfig(GsonUtils.getInstance().toJson(Map.of("secretKey", "replacement", "resourceMetadataUrl", metadataUrl)));
        assertThrows(IllegalArgumentException.class, () -> jwtPluginDataHandlerUnderTest.handlerPlugin(pluginData));
        assertSame(previous, Singleton.INST.get(JwtConfig.class));
    }

    @Test
    public void testHandlerRule() {
        RuleData ruleData = new RuleData();
        ruleData.setId("jwtRule");
        ruleData.setSelectorId("jwt");
        String handleJson = "{\"converter\":[{\"jwtVal\":\"sub\",\"headerVal\":\"id\"}]}";
        ruleData.setHandle(handleJson);
        jwtPluginDataHandlerUnderTest.handlerRule(ruleData);
        assertEquals(handleJson, JwtPluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(ruleData)).toJson());
    }

    @Test
    public void testPluginNamed() {
        final String result = jwtPluginDataHandlerUnderTest.pluginNamed();
        assertEquals(PluginEnum.JWT.getName(), result);
    }
}
