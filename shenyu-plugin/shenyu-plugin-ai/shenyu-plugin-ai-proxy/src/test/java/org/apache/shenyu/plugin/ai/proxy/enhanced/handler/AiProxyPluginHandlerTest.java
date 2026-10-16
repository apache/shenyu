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

package org.apache.shenyu.plugin.ai.proxy.enhanced.handler;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.AiProxyHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyConfigService;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

class AiProxyPluginHandlerTest {

    private AiProxyPluginHandler handler;

    @BeforeEach
    void setUp() {
        handler = new AiProxyPluginHandler();
    }

    @Test
    void testPluginNamed() {
        assertEquals(PluginEnum.AI_PROXY.getName(), handler.pluginNamed());
    }

    @Test
    void testHandlerSelectorCachesHandle() {
        SelectorData selector = new SelectorData();
        selector.setId("sel-1");
        selector.setHandle("{\"provider\":\"open_ai\",\"baseUrl\":\"https://api.openai.com\",\"apiKey\":\"sk-test\",\"model\":\"gpt-4\"}");

        handler.handlerSelector(selector);

        String key = CacheKeyUtils.INST.getKey("sel-1", Constants.DEFAULT_RULE);
        AiProxyHandle cached = handler.getSelectorCachedHandle().obtainHandle(key);
        assertNotNull(cached);
        assertEquals("open_ai", cached.getProvider());
        assertEquals("https://api.openai.com", cached.getBaseUrl());
        assertEquals("sk-test", cached.getApiKey());
        assertEquals("gpt-4", cached.getModel());
    }

    @Test
    void testHandlerSelectorWithNullHandle() {
        SelectorData selector = new SelectorData();
        selector.setId("sel-2");
        selector.setHandle(null);

        handler.handlerSelector(selector);

        String key = CacheKeyUtils.INST.getKey("sel-2", Constants.DEFAULT_RULE);
        assertNull(handler.getSelectorCachedHandle().obtainHandle(key));
    }

    @Test
    void testHandlerSelectorWithEmptyHandle() {
        SelectorData selector = new SelectorData();
        selector.setId("sel-3");
        selector.setHandle("");

        handler.handlerSelector(selector);

        String key = CacheKeyUtils.INST.getKey("sel-3", Constants.DEFAULT_RULE);
        assertNull(handler.getSelectorCachedHandle().obtainHandle(key));
    }

    @Test
    void testHandlerSelectorWithFallbackNormalize() {
        SelectorData selector = new SelectorData();
        selector.setId("sel-4");
        selector.setHandle("{\"provider\":\"open_ai\",\"fallbackEnabled\":\"true\",\"fallbackModel\":\"fallback-model\"}");

        handler.handlerSelector(selector);

        String key = CacheKeyUtils.INST.getKey("sel-4", Constants.DEFAULT_RULE);
        AiProxyHandle cached = handler.getSelectorCachedHandle().obtainHandle(key);
        assertNotNull(cached);
        assertNotNull(cached.getFallbackConfig());
        assertEquals("fallback-model", cached.getFallbackConfig().getModel());
    }

    @Test
    void testRemoveSelector() {
        SelectorData selector = new SelectorData();
        selector.setId("sel-5");
        selector.setHandle("{\"provider\":\"open_ai\",\"model\":\"gpt-4\"}");

        handler.handlerSelector(selector);

        String key = CacheKeyUtils.INST.getKey("sel-5", Constants.DEFAULT_RULE);
        assertNotNull(handler.getSelectorCachedHandle().obtainHandle(key));

        handler.removeSelector(selector);
        assertNull(handler.getSelectorCachedHandle().obtainHandle(key));
    }

    @Test
    void testNullPluginUpdateIsIgnored() {
        handler.handlerPlugin(null);
    }

    @Test
    void testGlobalConfigurationIsReplacedAndCopied() {
        handler.handlerPlugin(plugin("{\"provider\":\"OpenAI\",\"apiKey\":\"first-key\",\"model\":\"first-model\"}"));
        AiCommonConfig first = handler.getGlobalConfig();
        assertEquals("first-model", first.getModel());
        first.setApiKey("request-key");
        assertEquals("first-key", handler.getGlobalConfig().getApiKey());
        handler.handlerPlugin(plugin("{\"model\":\"second-model\"}"));
        assertEquals("second-model", handler.getGlobalConfig().getModel());
        assertNull(handler.getGlobalConfig().getApiKey());
        assertNull(handler.getGlobalConfig().getProvider());
    }

    @ParameterizedTest
    @NullAndEmptySource
    @ValueSource(strings = {" ", "null", "{}"})
    void testClearingPluginConfigurationRemovesOldDefaults(final String config) {
        handler.handlerPlugin(plugin("{\"apiKey\":\"old-key\",\"model\":\"old-model\"}"));
        handler.handlerPlugin(plugin(config));
        AiCommonConfig cleared = new AiProxyConfigService(handler).resolvePrimaryConfig(null);
        assertNull(cleared.getApiKey());
        assertNull(cleared.getModel());
    }

    @Test
    void testDisableReenableAndRemovePlugin() {
        PluginData data = plugin("{\"model\":\"global-model\"}");
        handler.handlerPlugin(data);
        data.setEnabled(false);
        handler.handlerPlugin(data);
        assertNull(handler.getGlobalConfig());
        data.setEnabled(true);
        handler.handlerPlugin(data);
        assertEquals("global-model", handler.getGlobalConfig().getModel());
        handler.removePlugin(data);
        assertNull(handler.getGlobalConfig());
    }

    @Test
    void testHandlerInstancesDoNotShareGlobalConfiguration() {
        AiProxyPluginHandler other = new AiProxyPluginHandler();
        handler.handlerPlugin(plugin("{\"model\":\"first-model\"}"));
        assertNull(other.getGlobalConfig());
        other.handlerPlugin(plugin("{\"model\":\"second-model\"}"));
        assertEquals("first-model", handler.getGlobalConfig().getModel());
        assertEquals("second-model", other.getGlobalConfig().getModel());
        handler.removePlugin(null);
        assertEquals("second-model", other.getGlobalConfig().getModel());
    }

    private PluginData plugin(final String config) {
        PluginData data = new PluginData();
        data.setConfig(config);
        return data;
    }
}
