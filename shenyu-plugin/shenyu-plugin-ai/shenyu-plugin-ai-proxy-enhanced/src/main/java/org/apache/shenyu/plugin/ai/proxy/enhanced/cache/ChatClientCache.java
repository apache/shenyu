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

package org.apache.shenyu.plugin.ai.proxy.enhanced.cache;

import org.springframework.ai.chat.client.ChatClient;
import org.springframework.ai.chat.model.ChatModel;
import org.springframework.stereotype.Component;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Supplier;

/**
 * This is ChatClient cache.
 */
@Component
public final class ChatClientCache {

    private static final String FALLBACK_KEY_SUFFIX = ":fallback";

    private final Map<String, ChatClient> chatClientMap = new ConcurrentHashMap<>();

    /**
     * Instantiates a new Chat client cache.
     */
    public ChatClientCache() {
    }

    /**
     * Gets client or compute if absent.
     *
     * @param key               the key
     * @param chatModelSupplier the chat model supplier
     * @return the chat client
     */
    public ChatClient computeIfAbsent(final String key, final Supplier<ChatModel> chatModelSupplier) {
        return chatClientMap.computeIfAbsent(key, k -> ChatClient.builder(chatModelSupplier.get()).build());
    }

    /**
     * Removes all cached clients associated with a selector ID.
     *
     * @param selectorId the selector id
     */
    public void remove(final String selectorId) {
        chatClientMap.remove(selectorId);
        chatClientMap.remove(selectorId + FALLBACK_KEY_SUFFIX);
    }

    /**
     * Puts a client directly into the cache.
     *
     * <p>NOTE: This method is intended for testing purposes to inject mock clients.
     *
     * @param key    the key
     * @param client the client to cache
     */
    public void put(final String key, final ChatClient client) {
        chatClientMap.put(key, client);
    }
}