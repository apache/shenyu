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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Supplier;

/**
 * This is ChatClient cache.
 */
public final class ChatClientCache {
    
    private static final Logger LOG = LoggerFactory.getLogger(ChatClientCache.class);
    
    private static final int MAX_CACHE_SIZE = 500;
    
    private final Map<String, ChatClient> chatClientMap = new ConcurrentHashMap<>();
    
    /**
     * Instantiates a new Chat client cache.
     */
    public ChatClientCache() {
    }
    
    /**
     * Gets client or compute if absent.
     *
     * @param key the key
     * @param chatModelSupplier the chat model supplier
     * @return the chat client
     */
    public ChatClient computeIfAbsent(final String key, final Supplier<ChatModel> chatModelSupplier) {
        if (chatClientMap.size() > MAX_CACHE_SIZE) {
            LOG.warn("[ChatClientCache] Cache size exceeded {}, clearing all.", MAX_CACHE_SIZE);
            chatClientMap.clear();
        }
        return chatClientMap.computeIfAbsent(key, k -> ChatClient.builder(chatModelSupplier.get()).build());
    }
    
    /**
     * Removes all cached clients associated with a selector ID (by prefix matching
     * "selectorId|").
     *
     * @param selectorId the selector id
     */
    public void remove(final String selectorId) {
        if (java.util.Objects.isNull(selectorId)) {
            return;
        }
        final String prefix = selectorId + "|";
        chatClientMap.keySet().removeIf(k -> k.equals(selectorId) || k.startsWith(prefix));
        LOG.info("[ChatClientCache] invalidate selectorId={} (by prefix)", selectorId);
    }
    
    /**
     * Clear all cached clients.
     */
    public void clearAll() {
        chatClientMap.clear();
        LOG.info("[ChatClientCache] cleared all cached clients");
    }
}