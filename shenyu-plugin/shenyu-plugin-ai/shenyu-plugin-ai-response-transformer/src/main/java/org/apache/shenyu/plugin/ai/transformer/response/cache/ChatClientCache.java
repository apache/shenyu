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

package org.apache.shenyu.plugin.ai.transformer.response.cache;

import org.springframework.ai.chat.client.ChatClient;
import org.springframework.ai.chat.model.ChatModel;

import java.util.HashMap;
import java.util.Map;

/**
 * this is chatClient cache for ai response transformer.
 */
public class ChatClientCache {

    private static final Map<String, ChatClient> CHAT_CLIENT_MAP = new HashMap<>();

    /**
     * Init.
     *
     * @param ruleId the ruleId
     * @param chatModel the chatModel
     * @return chatClient the chatClient
     */
    public ChatClient init(final String ruleId, final ChatModel chatModel) {
        ChatClient chatClient = ChatClient.builder(chatModel).build();
        CHAT_CLIENT_MAP.put(ruleId, chatClient);
        return chatClient;
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
     *
     * @param key key
     * @return the instance
     */
    public ChatClient getClient(final String key) {
        return CHAT_CLIENT_MAP.get(key);
    }

    /**
     * Destroy chatClient.
     *
     * @param key key
     */
    public void destroyClient(final String key) {
        CHAT_CLIENT_MAP.remove(key);
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
