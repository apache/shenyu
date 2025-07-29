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

package org.apache.shenyu.plugin.ai.proxy.handler;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.AiProxyHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.base.cache.CommonHandleCache;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class AiProxyPluginHandlerTest {

    private AiProxyPluginHandler handler;

    @BeforeEach
    void setUp() {
        handler = new AiProxyPluginHandler();
    }

    @Test
    void testHandlerPluginWithValidData() {
        PluginData pluginData = mock(PluginData.class);
        when(pluginData.getEnabled()).thenReturn(true);
        when(pluginData.getConfig()).thenReturn("{\"baseUrl\":\"https://api.example.com\",\"apiKey\":\"test-key\"}");

        handler.handlerPlugin(pluginData);

        AiCommonConfig config = Singleton.INST.get(AiCommonConfig.class);
        assertNotNull(config);
        assertEquals("https://api.example.com", config.getBaseUrl());
        assertEquals("test-key", config.getApiKey());
    }

    @Test
    void testHandlerPluginWithNullData() {
        handler.handlerPlugin(null);
        AiCommonConfig config = Singleton.INST.get(AiCommonConfig.class);
        assertNull(config);
    }

    @Test
    void testHandlerSelectorWithValidData() {
        SelectorData selectorData = mock(SelectorData.class);
        when(selectorData.getId()).thenReturn("selector-id");
        when(selectorData.getHandle()).thenReturn("{\"provider\":\"OPEN_AI\",\"baseUrl\":\"https://api.example.com\"}");

        handler.handlerSelector(selectorData);

        CommonHandleCache<String, AiProxyHandle> cache = AiProxyPluginHandler.SELECTOR_CACHED_HANDLE.get();
        AiProxyHandle handle = cache.obtainHandle("selector-id_default_rule");
        assertNotNull(handle);
        assertEquals("OPEN_AI", handle.getProvider());
        assertEquals("https://api.example.com", handle.getBaseUrl());
    }

    @Test
    void testHandlerSelectorWithNullHandle() {
        SelectorData selectorData = mock(SelectorData.class);
        when(selectorData.getHandle()).thenReturn(null);

        handler.handlerSelector(selectorData);

        CommonHandleCache<String, AiProxyHandle> cache = AiProxyPluginHandler.SELECTOR_CACHED_HANDLE.get();
        assertNull(cache.obtainHandle("selector-id_default_rule"));
    }

    @Test
    void testRemoveSelector() {
        SelectorData selectorData = mock(SelectorData.class);
        when(selectorData.getId()).thenReturn("selector-id");
        when(selectorData.getHandle()).thenReturn("{\"provider\":\"OPEN_AI\",\"baseUrl\":\"https://api.example.com\"}");

        handler.handlerSelector(selectorData);
        handler.removeSelector(selectorData);

        CommonHandleCache<String, AiProxyHandle> cache = AiProxyPluginHandler.SELECTOR_CACHED_HANDLE.get();
        assertNull(cache.obtainHandle("selector-id_default_rule"));
    }

    @Test
    void testPluginNamed() {
        assertEquals(PluginEnum.AI_PROXY.getName(), handler.pluginNamed());
    }
}
