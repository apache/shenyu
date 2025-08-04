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
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.ai.common.cache.ChatClientCache;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.common.spring.ai.AiModelFactory;
import org.apache.shenyu.plugin.ai.common.spring.ai.registry.AiModelFactoryRegistry;
import org.apache.shenyu.plugin.ai.transformer.response.config.AiResponseTransformerConfig;
import org.apache.shenyu.plugin.ai.transformer.response.handle.AiResponseTransformerHandle;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.ai.chat.model.ChatModel;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
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
        // 创建测试配置
        AiResponseTransformerConfig config = new AiResponseTransformerConfig();
        config.setProvider("openai");
        config.setBaseUrl("https://api.openai.com/v1");
        config.setApiKey("test-api-key");
        config.setModel("gpt-3.5-turbo");
        config.setContent("Transform the response");

        PluginData pluginData = new PluginData();
        pluginData.setEnabled(true);
        pluginData.setConfig(GsonUtils.getInstance().toJson(config));

        // 模拟AI模型工厂
        when(aiModelFactoryRegistry.getFactory(any())).thenReturn(aiModelFactory);
        when(aiModelFactory.createAiModel(any(AiCommonConfig.class))).thenReturn(chatModel);

        // 执行测试
        handler.handlerPlugin(pluginData);

        // 验证ChatClientCache是否被正确初始化
        ChatClientCache cache = ChatClientCache.getInstance();
        assertNotNull(cache.getClient("default"));
    }

    @Test
    void testHandlerRule() {
        // 创建测试规则数据
        AiResponseTransformerHandle handle = new AiResponseTransformerHandle();
        handle.setProvider("openai");
        handle.setBaseUrl("https://api.openai.com/v1");
        handle.setApiKey("test-api-key");
        handle.setModel("gpt-3.5-turbo");
        handle.setContent("Transform the response");

        RuleData ruleData = new RuleData();
        ruleData.setId("test-rule-id");
        ruleData.setHandle(GsonUtils.getInstance().toJson(handle));

        // 执行测试
        handler.handlerRule(ruleData);

        // 验证规则是否被正确缓存
        assertNotNull(AiResponseTransformerPluginHandler.CACHED_HANDLE.get().obtainHandle("test-rule-id"));
    }

    @Test
    void testRemoveRule() {
        // 创建测试规则数据
        AiResponseTransformerHandle handle = new AiResponseTransformerHandle();
        handle.setProvider("openai");

        RuleData ruleData = new RuleData();
        ruleData.setId("test-rule-id");
        ruleData.setHandle(GsonUtils.getInstance().toJson(handle));

        // 先添加规则
        handler.handlerRule(ruleData);

        // 然后移除规则
        handler.removeRule(ruleData);

        // 验证规则是否被正确移除
        assertEquals(0, AiResponseTransformerPluginHandler.CACHED_HANDLE.get().getCachedHandle().size());
    }

    @Test
    void testConvertConfig() {
        // 创建测试配置
        AiResponseTransformerConfig config = new AiResponseTransformerConfig();
        config.setProvider("openai");
        config.setBaseUrl("https://api.openai.com/v1");
        config.setApiKey("test-api-key");
        config.setModel("gpt-3.5-turbo");

        // 执行转换
        AiCommonConfig commonConfig = AiResponseTransformerPluginHandler.convertConfig(config);

        // 验证转换结果
        assertEquals("openai", commonConfig.getProvider());
        assertEquals("https://api.openai.com/v1", commonConfig.getBaseUrl());
        assertEquals("test-api-key", commonConfig.getApiKey());
        assertEquals("gpt-3.5-turbo", commonConfig.getModel());
    }

    @Test
    void testHandlerPluginWithNullConfig() {
        PluginData pluginData = new PluginData();
        pluginData.setEnabled(true);
        pluginData.setConfig(null);

        // 执行测试，不应该抛出异常
        handler.handlerPlugin(pluginData);
    }

    @Test
    void testHandlerPluginWithDisabledPlugin() {
        PluginData pluginData = new PluginData();
        pluginData.setEnabled(false);

        // 执行测试，不应该初始化ChatClient
        handler.handlerPlugin(pluginData);
    }
} 