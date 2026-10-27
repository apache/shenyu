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

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HexFormat;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * AiProxyApiKeyCache stores proxyApiKey -> ProxyApiKeyData mapping.
 *
 * <p>Cache debug logs identify entries by selector id plus a truncated SHA-256 digest, so neither
 * proxy credentials nor real API keys are written to logs.
 */
public final class AiProxyApiKeyCache {

    private static final Logger LOG = LoggerFactory.getLogger(AiProxyApiKeyCache.class);

    private static final int LOG_DIGEST_BYTES = 8;

    private static final AiProxyApiKeyCache INSTANCE = new AiProxyApiKeyCache();

    private final Map<String, ProxyApiKeyData> dataMap = new ConcurrentHashMap<>();

    private AiProxyApiKeyCache() {
    }

    private String key(final ProxyApiKeyData data) {
        return data.getSelectorId() + "::" + data.getProxyApiKey();
    }

    private String key(final String selectorId, final String proxyApiKey) {
        return selectorId + "::" + proxyApiKey;
    }

    private static String logKey(final ProxyApiKeyData data) {
        final String proxyApiKey = data.getProxyApiKey();
        return data.getSelectorId() + "::" + (Objects.isNull(proxyApiKey) ? "null" : digest(proxyApiKey));
    }

    private static String digest(final String value) {
        try {
            final byte[] hash = MessageDigest.getInstance("SHA-256").digest(value.getBytes(StandardCharsets.UTF_8));
            return HexFormat.of().formatHex(hash, 0, LOG_DIGEST_BYTES);
        } catch (NoSuchAlgorithmException ex) {
            return "redacted";
        }
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
     * Cache enabled data and evict explicitly disabled data, ignoring a null enabled status.
     *
     * @param data data
     */
    public void cache(final ProxyApiKeyData data) {
        if (Objects.isNull(data)) {
            return;
        }
        if (Boolean.TRUE.equals(data.getEnabled())) {
            dataMap.put(key(data), data);
            if (LOG.isDebugEnabled()) {
                LOG.debug("[AiProxyApiKeyCache] cache key={}, size={}", logKey(data), dataMap.size());
            }
        } else if (Boolean.FALSE.equals(data.getEnabled())) {
            // a disabled key arrives as an UPDATE event, so the stale enabled entry must be evicted here
            remove(data);
        }
    }

    /**
     * Remove data.
     *
     * @param data data
     */
    public void remove(final ProxyApiKeyData data) {
        if (Objects.nonNull(data) && Objects.nonNull(data.getProxyApiKey())) {
            dataMap.remove(key(data));
            if (LOG.isDebugEnabled()) {
                LOG.debug("[AiProxyApiKeyCache] remove key={}, size={}", logKey(data), dataMap.size());
            }
        }
    }

    /**
     * Get real api key by proxy key.
     *
     * @param selectorId selectorId
     * @param proxyApiKey proxy key
     * @return real key or null if missing/disabled
     */
    public String getRealApiKey(final String selectorId, final String proxyApiKey) {
        final ProxyApiKeyData data = getValidatedMapping(selectorId, proxyApiKey);
        return (Objects.nonNull(data) && Boolean.TRUE.equals(data.getEnabled()))
                ? data.getRealApiKey()
                : null;
    }

    /**
     * Capture an enabled mapping so admission and audit use the same synchronized identity.
     * @param selectorId matched selector
     * @param proxyApiKey presented credential
     * @return enabled mapping, or null
     */
    public ProxyApiKeyData getValidatedMapping(final String selectorId, final String proxyApiKey) {
        ProxyApiKeyData data = dataMap.get(key(selectorId, proxyApiKey));
        return Objects.nonNull(data) && Boolean.TRUE.equals(data.getEnabled()) ? data : null;
    }

    /** Clear cache. */
    public void refresh() {
        dataMap.clear();
        if (LOG.isDebugEnabled()) {
            LOG.debug("[AiProxyApiKeyCache] refresh clear, size=0");
        }
    }

    /**
     * Invalidate all mappings under a selector id.
     *
     * @param selectorId selector id
     */
    public void removeBySelectorId(final String selectorId) {
        if (Objects.isNull(selectorId)) {
            return;
        }
        final String prefix = selectorId + "::";
        int before = dataMap.size();
        dataMap.keySet().removeIf(k -> k.equals(selectorId) || k.startsWith(prefix));
        int after = dataMap.size();
        LOG.info("[AiProxyApiKeyCache] invalidate selectorId={}, removed={} entries", selectorId, before - after);
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
