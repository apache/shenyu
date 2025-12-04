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
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Supplier;

/**
 * This is ChatClient cache.
 */
public final class ChatClientCache {
    
    private static final Logger LOG = LoggerFactory.getLogger(ChatClientCache.class);
    
    private static final int MAX_CACHE_SIZE = getCacheSize();
    
    private final Map<String, ChatClient> chatClientMap = new ConcurrentHashMap<>();
    
    private final AtomicBoolean evictionInProgress = new AtomicBoolean(false);
    
    /**
     * Instantiates a new Chat client cache.
     */
    public ChatClientCache() {
    }

    private static int getCacheSize() {
        String value = System.getProperty("shenyu.plugin.ai.proxy.enhanced.cache.maxSize",
                System.getenv("SHENYU_PLUGIN_AI_PROXY_ENHANCED_CACHE_MAXSIZE"));
        if (Objects.nonNull(value)) {
            try {
                return Integer.parseInt(value);
            } catch (NumberFormatException e) {
                LoggerFactory.getLogger(ChatClientCache.class)
                        .warn("[ChatClientCache] Invalid cache size '{}', using default 500.", value);
            }
        }
        return 500;
    }
    
    /**
     * Gets client or compute if absent.
     *
     * @param key the key
     * @param chatModelSupplier the chat model supplier
     * @return the chat client
     */
    public ChatClient computeIfAbsent(final String key, final Supplier<ChatModel> chatModelSupplier) {
        // Check size before computing, but use synchronized block to prevent race conditions
        final int currentSize = chatClientMap.size();
        if (currentSize > MAX_CACHE_SIZE) {
            // Use atomic flag to ensure only one thread performs eviction
            if (evictionInProgress.compareAndSet(false, true)) {
                try {
                    synchronized (chatClientMap) {
                        // Double-check after acquiring lock
                        if (chatClientMap.size() > MAX_CACHE_SIZE) {
                            evictOldestEntries();
                        }
                    }
                } finally {
                    evictionInProgress.set(false);
                }
            }
        }
        return chatClientMap.computeIfAbsent(key, k -> ChatClient.builder(chatModelSupplier.get()).build());
    }
    
    /**
     * Evict oldest entries when cache size exceeds limit.
     * Removes approximately 25% of entries to avoid thundering herd problem.
     */
    private void evictOldestEntries() {
        final int currentSize = chatClientMap.size();
        if (currentSize <= MAX_CACHE_SIZE) {
            return;
        }
        
        // Evict 25% of entries, but at least 10 entries
        final int evictCount = Math.max(10, currentSize / 4);
        LOG.warn("[ChatClientCache] Cache size {} exceeded limit {}, evicting {} oldest entries", 
                currentSize, MAX_CACHE_SIZE, evictCount);
        
        // Since ConcurrentHashMap doesn't maintain insertion order,
        // we evict entries based on iteration order (which is somewhat arbitrary but better than clearing all)
        int removed = 0;
        for (final String key : chatClientMap.keySet()) {
            if (removed >= evictCount) {
                break;
            }
            chatClientMap.remove(key);
            removed++;
        }
        
        LOG.info("[ChatClientCache] Evicted {} entries, cache size now: {}", removed, chatClientMap.size());
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