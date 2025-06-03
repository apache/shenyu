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

package org.apache.shenyu.plugin.ai.transformer.request.cache;

import org.apache.shenyu.common.dto.convert.plugin.AiRequestTransformerConfig;
import org.apache.shenyu.common.enums.AiModelProviderEnum;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.ai.chat.model.ChatModel;
import org.springframework.ai.deepseek.DeepSeekChatModel;
import org.springframework.ai.deepseek.DeepSeekChatOptions;
import org.springframework.ai.deepseek.api.DeepSeekApi;
import org.springframework.ai.openai.OpenAiChatModel;
import org.springframework.ai.openai.OpenAiChatOptions;
import org.springframework.ai.openai.api.OpenAiApi;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/**
 * this is chatClient cache.
 */
public class ChatClientCache {

    private static final Map<String, ChatClient> CHAT_CLIENT_MAP = new HashMap<>();

    public ChatClientCache() {
    }

    /**
     * Init.
     *gi
     * @param aiRequestTransformerConfig the aiRequestTransformerConfig
     */
    public ChatClient init(final String ruleId, final AiRequestTransformerConfig aiRequestTransformerConfig) {
        ChatModel chatModel = initAiModel(aiRequestTransformerConfig);
        ChatClient chatClient = ChatClient.builder(chatModel).build();
        CHAT_CLIENT_MAP.put(ruleId + aiRequestTransformerConfig.getProvider(), chatClient);
        return chatClient;
    }


    /**
     * Init chatModel.
     *
     * @param aiRequestTransformerConfig the aiRequestTransformerConfig.
     * @return chatModel.
     */
    public ChatModel initAiModel(final AiRequestTransformerConfig aiRequestTransformerConfig) {
        switch (Objects.requireNonNull(AiModelProviderEnum.getByName(aiRequestTransformerConfig.getProvider()))) {
            case DEEP_SEEK :
                DeepSeekApi deepSeekApi = DeepSeekApi.builder()
                        .baseUrl(aiRequestTransformerConfig.getBaseUrl())
                        .apiKey(aiRequestTransformerConfig.getApiKey())
                        .build();
                return DeepSeekChatModel.builder().deepSeekApi(deepSeekApi)
                         .defaultOptions(
                                 DeepSeekChatOptions.builder()
                                         .model(aiRequestTransformerConfig.getModel())
                                         .build())
                         .build();
            case OPEN_AI:
                OpenAiApi openAiApi = OpenAiApi.builder()
                        .baseUrl(aiRequestTransformerConfig.getBaseUrl())
                        .apiKey(aiRequestTransformerConfig.getApiKey())
                        .build();
                return OpenAiChatModel.builder().openAiApi(openAiApi)
                        .defaultOptions(
                                OpenAiChatOptions.builder()
                                        .model(aiRequestTransformerConfig.getModel())
                                        .build()
                        ).build();
            default:
                return null;
        }
    }

    /**
     * Gets instance.
     *
     * @return the instance
     */
    public static ChatClientCache getInstance() {
        return ApplicationConfigCacheInstance.INSTANCE;
    }

    /**
     * Gets chatClient.
     * @param key key
     * @return the instance
     */
    public ChatClient getClient(final String key) {
        return CHAT_CLIENT_MAP.get(key);
    }

    /**
     * The type Application config cache instance.
     */
    static final class ApplicationConfigCacheInstance {
        /**
         * The Instance.
         */
        static final ChatClientCache INSTANCE = new ChatClientCache();

        private ApplicationConfigCacheInstance() {

        }
    }
}
