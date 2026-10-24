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

package org.apache.shenyu.plugin.ai.transformer.response;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.convert.plugin.AiResponseTransformerConfig;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.common.spring.ai.AiModelFactory;
import org.apache.shenyu.plugin.ai.common.spring.ai.registry.AiModelFactoryRegistry;
import org.apache.shenyu.plugin.ai.transformer.response.cache.ChatClientCache;
import org.apache.shenyu.plugin.ai.transformer.response.handler.AiResponseTransformerPluginHandler;
import org.apache.shenyu.plugin.ai.transformer.response.template.AiResponseTransformerTemplate;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedConstruction;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.ai.chat.model.ChatModel;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/**
 * Effective rule configuration must agree with the cached model and preserve global defaults.
 */
class AiResponseTransformerConfigurationTest {

    private final AiModelFactoryRegistry registry = mock(AiModelFactoryRegistry.class);

    private final AiModelFactory factory = mock(AiModelFactory.class);

    private final AiResponseTransformerPluginHandler handler = new AiResponseTransformerPluginHandler(registry);

    private final AiResponseTransformerPlugin plugin = new AiResponseTransformerPlugin(List.of(), registry);

    private AiResponseTransformerConfig global;

    @BeforeEach
    void setup() {
        global = new AiResponseTransformerConfig();
        global.setProvider("openai");
        global.setBaseUrl("https://global.example");
        global.setApiKey("global-key");
        global.setModel("global-model");
        global.setContent("global prompt");
        Singleton.INST.single(AiResponseTransformerConfig.class, global);
    }

    @AfterEach
    void cleanup() {
        handler.removeRule(rule("partial", "{}"));
        handler.removeRule(rule("other", "{}"));
        ChatClientCache.getInstance().destroyClient("default");
        Singleton.INST.single(AiResponseTransformerConfig.class, new AiResponseTransformerConfig());
    }

    @ParameterizedTest
    @ValueSource(strings = {"{}", "{\"content\":\"rule prompt\"}", "{\"model\":\"rule-model\"}", "{\"apiKey\":\"rule-key\"}"})
    void testPartialHandleInheritsGlobalConnectionAndPrompt(final String handle) {
        RuleData rule = rule("partial", handle);
        handler.handlerRule(rule);
        prepareFactory();
        AtomicReference<String> prompt = new AtomicReference<>();
        try (MockedConstruction<AiResponseTransformerTemplate> templates = mockConstruction(AiResponseTransformerTemplate.class,
                (mock, context) -> prompt.set((String) context.arguments().get(0)))) {
            execute(rule);
            assertEquals(1, templates.constructed().size());
        }
        AiCommonConfig effective = captureConfig(1).get(0);
        assertEquals("openai", effective.getProvider());
        assertEquals("https://global.example", effective.getBaseUrl());
        assertEquals(handle.contains("rule-key") ? "rule-key" : "global-key", effective.getApiKey());
        assertEquals(handle.contains("rule-model") ? "rule-model" : "global-model", effective.getModel());
        assertEquals(handle.contains("rule prompt") ? "rule prompt" : "global prompt", prompt.get());
        assertEquals("global-key", global.getApiKey());
        assertEquals("global-model", global.getModel());
        assertEquals("global prompt", global.getContent());
    }

    @Test
    void testConnectionOverridesDoNotAffectOtherRules() {
        prepareFactory();
        RuleData overridden = rule("partial", "{\"provider\":\"openai\",\"baseUrl\":\"https://rule.example\",\"apiKey\":\"rule-key\"}");
        handler.handlerRule(overridden);
        execute(overridden);
        RuleData inherited = rule("other", "{}");
        handler.handlerRule(inherited);
        execute(inherited);
        List<AiCommonConfig> configs = captureConfig(2);
        assertEquals("https://rule.example", configs.get(0).getBaseUrl());
        assertEquals("rule-key", configs.get(0).getApiKey());
        assertEquals("https://global.example", configs.get(1).getBaseUrl());
        assertEquals("global-key", configs.get(1).getApiKey());
    }

    @Test
    void testGlobalUpdateReplacesInheritedClientAndStableSettingsReuseIt() {
        prepareFactory();
        RuleData rule = rule("partial", "{\"content\":\"rule prompt\"}");
        handler.handlerRule(rule);
        execute(rule);
        ChatClient first = ChatClientCache.getInstance().getClient("partial");
        execute(rule);
        assertSame(first, ChatClientCache.getInstance().getClient("partial"));
        global.setBaseUrl("https://updated.example");
        global.setApiKey("updated-key");
        global.setModel("updated-model");
        execute(rule);
        assertNotSame(first, ChatClientCache.getInstance().getClient("partial"));
        List<AiCommonConfig> configs = captureConfig(2);
        assertEquals("https://updated.example", configs.get(1).getBaseUrl());
        assertEquals("updated-key", configs.get(1).getApiKey());
        assertEquals("updated-model", configs.get(1).getModel());
    }

    @Test
    void testRuleUpdateReplacesClientWithNewEffectiveModel() {
        prepareFactory();
        RuleData rule = rule("partial", "{}");
        handler.handlerRule(rule);
        execute(rule);
        rule.setHandle("{\"model\":\"updated-model\"}");
        handler.handlerRule(rule);
        execute(rule);
        List<AiCommonConfig> configs = captureConfig(2);
        assertEquals("global-model", configs.get(0).getModel());
        assertEquals("updated-model", configs.get(1).getModel());
        assertEquals("global-key", configs.get(1).getApiKey());
    }

    @Test
    void testBlankConnectionOverrideIsInvalidAndDoesNotUseGlobalClient() {
        RuleData rule = rule("partial", "{\"apiKey\":\" \"}");
        handler.handlerRule(rule);
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/test"));
        plugin.doExecute(exchange, forwarded -> {
            assertSame(exchange, forwarded);
            return Mono.empty();
        }, null, rule).block();
        verifyNoInteractions(registry, factory);
    }

    private void prepareFactory() {
        when(registry.getFactory(any())).thenReturn(factory);
        when(factory.createAiModel(any())).thenReturn(mock(ChatModel.class));
    }

    private void execute(final RuleData rule) {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/test"));
        plugin.doExecute(exchange, forwarded -> {
            assertInstanceOf(AiResponseTransformerPlugin.AiResponseTransformerDecorator.class, forwarded.getResponse());
            return Mono.empty();
        }, null, rule).block();
    }

    private List<AiCommonConfig> captureConfig(final int count) {
        ArgumentCaptor<AiCommonConfig> captor = ArgumentCaptor.forClass(AiCommonConfig.class);
        verify(factory, times(count)).createAiModel(captor.capture());
        return captor.getAllValues();
    }

    private RuleData rule(final String id, final String handle) {
        return RuleData.builder().id(id).selectorId("selector").handle(handle).build();
    }
}
