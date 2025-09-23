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

import org.apache.shenyu.common.dto.ProxyApiKeyData;

import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This is AI proxy api key cache.
 */
public final class AiProxyApiKeyCache {

    private static final Logger LOG = LoggerFactory.getLogger(AiProxyApiKeyCache.class);

    private final Map<String, Map<String, String>> apiKeyMapping = new ConcurrentHashMap<>();

    /**
     * Get real api key.
     *
     * @param selectorId selectorId
     * @param proxyApiKey proxy api key
     * @return real api key
     */
    public String getRealApiKey(final String selectorId, final String proxyApiKey) {
        final Map<String, String> selectorApiKeys = apiKeyMapping.get(selectorId);
        if (Objects.isNull(selectorApiKeys)) {
            return null;
        }
        return selectorApiKeys.get(proxyApiKey);
    }

    /**
     * On subscribe.
     *
     * @param data the data
     */
    public void onSubscribe(final ProxyApiKeyData data) {
        final String selectorId = data.getSelectorId();
        final String proxyApiKey = data.getProxyApiKey();
        final String realApiKey = data.getRealApiKey();
        apiKeyMapping.computeIfAbsent(selectorId, k -> new ConcurrentHashMap<>()).put(proxyApiKey, realApiKey);
    }

    /**
     * Un subscribe.
     *
     * @param data the data
     */
    public void unSubscribe(final ProxyApiKeyData data) {
        final String selectorId = data.getSelectorId();
        final Map<String, String> selectorApiKeys = apiKeyMapping.get(selectorId);
        if (Objects.nonNull(selectorApiKeys)) {
            selectorApiKeys.remove(data.getProxyApiKey());
            if (selectorApiKeys.isEmpty()) {
                apiKeyMapping.remove(selectorId);
            }
        }
    }

    /**
     * Invalidate.
     *
     * @param selectorId the selectorId
     */
    public void invalidate(final String selectorId) {
        if (Objects.nonNull(selectorId)) {
            apiKeyMapping.remove(selectorId);
            LOG.info("[AiProxyApiKeyCache] invalidate selectorId={}", selectorId);
        }
    }

    /**
     * Return cache size.
     *
     * @return cache size
     */
    public int size() {
        return apiKeyMapping.size();
    }
}
