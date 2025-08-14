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

package org.apache.shenyu.plugin.ai.transformer.response.handler;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.convert.plugin.AiResponseTransformerConfig;
import org.apache.shenyu.common.dto.convert.rule.AiResponseTransformerHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.ai.transformer.response.cache.ChatClientCache;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.ai.common.spring.ai.AiModelFactory;
import org.apache.shenyu.plugin.ai.common.spring.ai.registry.AiModelFactoryRegistry;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.ai.chat.model.ChatModel;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

/**
 * Test for AiResponseTransformerPluginHandler.
 */
@ExtendWith(MockitoExtension.class)
class AiResponseTransformerPluginHandlerTest {

    @Mock
    private AiModelFactoryRegistry aiModelFactoryRegistry;

    @Mock
    private AiModelFactory aiModelFactory;

    @Mock
    private ChatModel chatModel;

    private AiResponseTransformerPluginHandler handler;

    @BeforeEach
    void setUp() {
        handler = new AiResponseTransformerPluginHandler(aiModelFactoryRegistry);
    }

    @Test
    void testPluginNamed() {
        assertEquals(PluginEnum.AI_RESPONSE_TRANSFORMER.getName(), handler.pluginNamed());
    }

    @Test
    void testHandlerPlugin() {
        // Create test configuration
        AiResponseTransformerConfig config = new AiResponseTransformerConfig();
        config.setProvider("openai");
        config.setBaseUrl("https://api.openai.com/v1");
        config.setApiKey("test-api-key");
        config.setModel("gpt-3.5-turbo");
        config.setContent("Transform the response");

        PluginData pluginData = new PluginData();
        pluginData.setEnabled(true);
        pluginData.setConfig(GsonUtils.getInstance().toJson(config));

        // Mock AI model factory
        when(aiModelFactoryRegistry.getFactory(any())).thenReturn(aiModelFactory);
        when(aiModelFactory.createAiModel(any(AiCommonConfig.class))).thenReturn(chatModel);

        // Execute test
        handler.handlerPlugin(pluginData);

        // Verify ChatClientCache is properly initialized
        ChatClientCache cache = ChatClientCache.getInstance();
        assertNotNull(cache.getClient("default"));
    }

    @Test
    void testHandlerPluginWithEnhancedContent() {
        // Create test configuration with enhanced content
        AiResponseTransformerConfig config = new AiResponseTransformerConfig();
        config.setProvider("openai");
        config.setBaseUrl("https://api.openai.com/v1");
        config.setApiKey("test-api-key");
        config.setModel("gpt-4");
        config.setContent("Please transform the LLM response to standard API format. Requirements: "
                + "1. Keep HTTP status code as 200 2. Set Content-Type to application/json 3. Wrap the LLM text response in "
                + "{\"data\": {\"content\": \"LLM response content\"}, \"status\": \"success\"} JSON structure");

        PluginData pluginData = new PluginData();
        pluginData.setEnabled(true);
        pluginData.setConfig(GsonUtils.getInstance().toJson(config));

        // Mock AI model factory
        when(aiModelFactoryRegistry.getFactory(any())).thenReturn(aiModelFactory);
        when(aiModelFactory.createAiModel(any(AiCommonConfig.class))).thenReturn(chatModel);

        // Execute test
        handler.handlerPlugin(pluginData);

        // Verify ChatClientCache is properly initialized
        ChatClientCache cache = ChatClientCache.getInstance();
        assertNotNull(cache.getClient("default"));
    }

    @Test
    void testHandlerRule() {
        // Create test rule data
        AiResponseTransformerHandle handle = new AiResponseTransformerHandle();
        handle.setProvider("openai");
        handle.setBaseUrl("https://api.openai.com/v1");
        handle.setApiKey("test-api-key");
        handle.setModel("gpt-3.5-turbo");
        handle.setContent("Transform the response");

        RuleData ruleData = new RuleData();
        ruleData.setId("test-rule-id");
        ruleData.setHandle(GsonUtils.getInstance().toJson(handle));

        // Execute test
        handler.handlerRule(ruleData);

        // Verify rule is properly cached
        assertNotNull(AiResponseTransformerPluginHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(ruleData)));
    }

    @Test
    void testHandlerRuleWithEnhancedContent() {
        // Create test rule data with enhanced content
        AiResponseTransformerHandle handle = new AiResponseTransformerHandle();
        handle.setProvider("openai");
        handle.setBaseUrl("https://api.openai.com/v1");
        handle.setApiKey("test-api-key");
        handle.setModel("gpt-4");
        handle.setContent("Please transform the response to standard JSON format, including status and data fields");

        RuleData ruleData = new RuleData();
        ruleData.setId("test-rule-id");
        ruleData.setHandle(GsonUtils.getInstance().toJson(handle));

        // Execute test
        handler.handlerRule(ruleData);

        // Verify rule is properly cached
        assertNotNull(AiResponseTransformerPluginHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(ruleData)));
    }

    @Test
    void testRemoveRule() {
        // Create test rule data
        AiResponseTransformerHandle handle = new AiResponseTransformerHandle();
        handle.setProvider("openai");

        RuleData ruleData = new RuleData();
        ruleData.setId("test-rule-id");
        ruleData.setHandle(GsonUtils.getInstance().toJson(handle));

        // First add the rule
        handler.handlerRule(ruleData);

        // Then remove the rule
        handler.removeRule(ruleData);

        // Verify rule is properly removed
        assertNull(AiResponseTransformerPluginHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(ruleData)));
    }

    @Test
    void testConvertConfig() {
        // Create test configuration
        AiResponseTransformerConfig config = new AiResponseTransformerConfig();
        config.setProvider("openai");
        config.setBaseUrl("https://api.openai.com/v1");
        config.setApiKey("test-api-key");
        config.setModel("gpt-3.5-turbo");

        // Execute conversion
        AiCommonConfig commonConfig = AiResponseTransformerPluginHandler.convertConfig(config);

        // Verify conversion result
        assertEquals("openai", commonConfig.getProvider());
        assertEquals("https://api.openai.com/v1", commonConfig.getBaseUrl());
        assertEquals("test-api-key", commonConfig.getApiKey());
        assertEquals("gpt-3.5-turbo", commonConfig.getModel());
    }

    @Test
    void testConvertConfigWithNullValues() {
        // Create test configuration with null values
        AiResponseTransformerConfig config = new AiResponseTransformerConfig();
        config.setProvider("openai");
        config.setBaseUrl(null);
        config.setApiKey("test-api-key");
        config.setModel(null);

        // Execute conversion
        AiCommonConfig commonConfig = AiResponseTransformerPluginHandler.convertConfig(config);

        // Verify conversion result
        assertEquals("openai", commonConfig.getProvider());
        assertNull(commonConfig.getBaseUrl());
        assertEquals("test-api-key", commonConfig.getApiKey());
        assertNull(commonConfig.getModel());
    }

    @Test
    void testHandlerPluginWithNullConfig() {
        PluginData pluginData = new PluginData();
        pluginData.setEnabled(true);
        pluginData.setConfig(null);

        // Execute test, should not throw exception
        handler.handlerPlugin(pluginData);
    }

    @Test
    void testHandlerPluginWithDisabledPlugin() {
        PluginData pluginData = new PluginData();
        pluginData.setEnabled(false);

        // Execute test, should not initialize ChatClient
        handler.handlerPlugin(pluginData);
    }

    @Test
    void testHandlerPluginWithInvalidConfig() {
        PluginData pluginData = new PluginData();
        pluginData.setEnabled(true);
        pluginData.setConfig("invalid json config");

        // Execute test, should not throw exception
        handler.handlerPlugin(pluginData);
    }

    @Test
    void testHandlerRuleWithNullHandle() {
        RuleData ruleData = new RuleData();
        ruleData.setId("test-rule-id");
        ruleData.setHandle(null);

        // Execute test, should not throw exception
        handler.handlerRule(ruleData);
    }

    @Test
    void testHandlerRuleWithInvalidHandle() {
        RuleData ruleData = new RuleData();
        ruleData.setId("test-rule-id");
        ruleData.setHandle("invalid json handle");

        // Execute test, should not throw exception
        handler.handlerRule(ruleData);
    }

    @Test
    void testRemoveRuleWithNullHandle() {
        RuleData ruleData = new RuleData();
        ruleData.setId("test-rule-id");
        ruleData.setHandle(null);

        // Execute test, should not throw exception
        handler.removeRule(ruleData);
    }

    @Test
    void testRemoveRuleWithInvalidHandle() {
        RuleData ruleData = new RuleData();
        ruleData.setId("test-rule-id");
        ruleData.setHandle("invalid json handle");

        // Execute test, should not throw exception
        handler.removeRule(ruleData);
    }

    @Test
    void testConvertConfigWithEmptyConfig() {
        // Create empty test configuration
        AiResponseTransformerConfig config = new AiResponseTransformerConfig();

        // Execute conversion
        AiCommonConfig commonConfig = AiResponseTransformerPluginHandler.convertConfig(config);

        // Verify conversion result, all fields should be null
        assertNull(commonConfig.getProvider());
        assertNull(commonConfig.getBaseUrl());
        assertNull(commonConfig.getApiKey());
        assertNull(commonConfig.getModel());
    }
} 
