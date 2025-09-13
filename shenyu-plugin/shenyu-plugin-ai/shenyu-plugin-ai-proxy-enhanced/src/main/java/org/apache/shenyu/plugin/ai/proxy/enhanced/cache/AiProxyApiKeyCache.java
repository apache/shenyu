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

/** AiProxyApiKeyCache stores proxyApiKey -> ProxyApiKeyData mapping. */
public final class AiProxyApiKeyCache {

    private static final Logger LOG = LoggerFactory.getLogger(AiProxyApiKeyCache.class);

    private static final AiProxyApiKeyCache INSTANCE = new AiProxyApiKeyCache();

    private final Map<String, ProxyApiKeyData> dataMap = new ConcurrentHashMap<>();

    private AiProxyApiKeyCache() {
    }

    /**
     * Gets instance.
     *
     * @return singleton
     */
    public static AiProxyApiKeyCache getInstance() {
        return INSTANCE;
    }

    /**
     * Cache data when enabled.
     *
     * @param data data
     */
    public void cache(final ProxyApiKeyData data) {
        if (Objects.nonNull(data) && Boolean.TRUE.equals(data.getEnabled())) {
            dataMap.put(data.getProxyApiKey(), data);
            if (LOG.isDebugEnabled()) {
                LOG.debug("[AiProxyApiKeyCache] cache key={}, size={}", data.getProxyApiKey(), dataMap.size());
            }
        }
    }

    /**
     * Remove data.
     *
     * @param data data
     */
    public void remove(final ProxyApiKeyData data) {
        if (Objects.nonNull(data) && Objects.nonNull(data.getProxyApiKey())) {
            dataMap.remove(data.getProxyApiKey());
            if (LOG.isDebugEnabled()) {
                LOG.debug("[AiProxyApiKeyCache] remove key={}, size={}", data.getProxyApiKey(), dataMap.size());
            }
        }
    }

    /**
     * Get real api key by proxy key.
     *
     * @param proxyApiKey proxy key
     * @return real key or null if missing/disabled
     */
    public String getRealApiKey(final String proxyApiKey) {
        final ProxyApiKeyData data = dataMap.get(proxyApiKey);
        return (Objects.nonNull(data) && Boolean.TRUE.equals(data.getEnabled()))
                ? data.getRealApiKey()
                : null;
    }

    /** Clear cache. */
    public void refresh() {
        dataMap.clear();
        if (LOG.isDebugEnabled()) {
            LOG.debug("[AiProxyApiKeyCache] refresh clear, size=0");
        }
    }

    /**
     * Current cached mapping size.
     *
     * @return size
     */
    public int size() {
        return dataMap.size();
    }
}
