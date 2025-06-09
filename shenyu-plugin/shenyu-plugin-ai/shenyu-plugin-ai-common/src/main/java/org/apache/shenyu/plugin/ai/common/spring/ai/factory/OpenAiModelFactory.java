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
import org.apache.shenyu.plugin.ai.common.spring.ai.AiModelFactory;
import org.springframework.ai.chat.model.ChatModel;
import org.springframework.ai.openai.OpenAiChatModel;
import org.springframework.ai.openai.OpenAiChatOptions;
import org.springframework.ai.openai.api.OpenAiApi;

import java.util.Optional;

/**
 * this is the OpenAiModelFactory.
 */
public class OpenAiModelFactory implements AiModelFactory {

    private static final AiModelProviderEnum MODEL_PROVIDER_ENUM = AiModelProviderEnum.OPEN_AI;

    @Override
    public ChatModel createAiModel(final AiCommonConfig config) {
        OpenAiApi openAiApi = OpenAiApi.builder()
                .baseUrl(config.getBaseUrl())
                .apiKey(config.getApiKey())
                .build();
        OpenAiChatOptions.Builder model = OpenAiChatOptions.builder().model(config.getModel());
        Optional.ofNullable(config.getTemperature()).ifPresent(model::temperature);
        Optional.ofNullable(config.getMaxTokens()).ifPresent(model::maxTokens);
        return OpenAiChatModel.builder().openAiApi(openAiApi)
                .defaultOptions(model.build())
                .build();
    }

    @Override
    public boolean supports(final AiModelProviderEnum modelType) {
        return MODEL_PROVIDER_ENUM.equals(modelType);
    }
}
