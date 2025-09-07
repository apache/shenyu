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

package org.apache.shenyu.plugin.ai.common.spring.ai.factory;

import org.apache.shenyu.common.enums.AiModelProviderEnum;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.springframework.ai.chat.model.ChatModel;
import org.springframework.ai.openai.OpenAiChatModel;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class OpenAiModelFactoryTest {

    private OpenAiModelFactory factory;

    @BeforeEach
    void setUp() {
        factory = new OpenAiModelFactory();
    }

    @Test
    void testCreateAiModel() {
        AiCommonConfig config = mock(AiCommonConfig.class);
        when(config.getBaseUrl()).thenReturn("https://api.openai.com");
        when(config.getApiKey()).thenReturn("test-api-key");
        when(config.getModel()).thenReturn("gpt-3.5-turbo");
        when(config.getTemperature()).thenReturn(0.7);
        when(config.getMaxTokens()).thenReturn(100);

        ChatModel chatModel = factory.createAiModel(config);

        assertNotNull(chatModel);
        assertTrue(chatModel instanceof OpenAiChatModel);
    }

    @Test
    void testSupportsWithOpenAi() {
        assertTrue(factory.supports(AiModelProviderEnum.OPEN_AI));
    }

    @Test
    void testSupportsWithOtherModelType() {
        assertFalse(factory.supports(AiModelProviderEnum.DEEP_SEEK));
    }

}
